;;; aside-acp.el --- Agent Client Protocol transport for aside  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;; Author: nottzaid
;; URL: https://github.com/nottzaid/aside

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A small client for the Agent Client Protocol
;; <https://agentclientprotocol.com>: JSON-RPC 2.0 with one message
;; per line on an agent's standard input and output.
;;
;; This file moves messages and nothing else.  It starts an agent,
;; performs the `initialize' handshake, correlates responses with
;; requests, and hands incoming requests and notifications to the
;; handlers given to `aside-acp-start'.  Everything about sessions,
;; buffers and frames lives in aside.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst aside-acp-protocol-version 1
  "The ACP protocol version this client speaks.")

(define-error 'aside-acp-error "Agent error")

(defvar aside-acp-trace-functions nil
  "Abnormal hook run for every message line sent or received.
Each function is called with three arguments: the connection, the
symbol `in' or `out', and the raw JSON line.")

(cl-defstruct (aside-acp (:constructor aside-acp--make)
                         (:copier nil))
  "A connection to one agent process."
  name process stderr
  (next-id 0)
  (pending (make-hash-table :test #'eql))
  request-handler notification-handler exit-handler
  agent-capabilities agent-info auth-methods
  ready)

;;;; Starting and stopping

(cl-defun aside-acp-start (&key name command client-info client-capabilities
                                on-request on-notification on-exit
                                on-ready on-error)
  "Start the agent COMMAND and return its connection.
NAME labels the process and its buffers.  COMMAND is a list of
program and arguments.  CLIENT-INFO and CLIENT-CAPABILITIES are sent
as-is during `initialize'.

ON-REQUEST is called with the connection, a method name, its params
and a REPLY function; it must eventually call REPLY with a result, or
with nil and an error plist (:code :message).  ON-NOTIFICATION is
called with the connection, a method name and its params.  ON-EXIT is
called with the connection and a description of how the process
ended.  ON-READY is called with the connection once the handshake
succeeds, ON-ERROR with an error plist if it fails."
  (let* ((stderr (get-buffer-create (format " *aside %s stderr*" name)))
         (conn (aside-acp--make :name name :stderr stderr
                                :request-handler on-request
                                :notification-handler on-notification
                                :exit-handler on-exit))
         (process
          (make-process
           :name (format "aside-%s" name)
           :command command
           :buffer (generate-new-buffer (format " *aside %s*" name))
           ;; A silent sentinel, so the buffer holds only what the agent said.
           :stderr (make-pipe-process :name (format "aside-%s stderr" name)
                                      :buffer (with-current-buffer stderr
                                                (erase-buffer)
                                                stderr)
                                      :noquery t
                                      :sentinel #'ignore)
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :filter #'aside-acp--filter
           :sentinel #'aside-acp--sentinel)))
    (process-put process 'aside-acp conn)
    (setf (aside-acp-process conn) process)
    (aside-acp-request
     conn "initialize"
     (list :protocolVersion aside-acp-protocol-version
           :clientCapabilities client-capabilities
           :clientInfo client-info)
     (lambda (result)
       (setf (aside-acp-agent-capabilities conn)
             (plist-get result :agentCapabilities)
             (aside-acp-agent-info conn) (plist-get result :agentInfo)
             (aside-acp-auth-methods conn) (plist-get result :authMethods)
             (aside-acp-ready conn) t)
       (when on-ready (funcall on-ready conn)))
     on-error)
    conn))

(defun aside-acp-live-p (conn)
  "Return non-nil if CONN's agent process is running."
  (and conn (process-live-p (aside-acp-process conn))))

(defun aside-acp-stop (conn)
  "Stop CONN's agent process and fail its outstanding requests."
  (when-let* ((process (and conn (aside-acp-process conn))))
    (when (process-live-p process)
      (delete-process process))))

(defun aside-acp-stderr-tail (conn &optional lines)
  "Return the last LINES (default 5) of CONN's standard error."
  (let ((buffer (aside-acp-stderr conn)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward " \t\n")
          (let ((end (point)))
            (forward-line (- 1 (or lines 5)))
            (string-trim (buffer-substring-no-properties
                          (line-beginning-position) end))))))))

;;;; Sending

(defun aside-acp--send (conn message)
  "Write MESSAGE, a plist, to CONN as one JSON line."
  (let ((line (decode-coding-string
               (json-serialize (cons :jsonrpc (cons "2.0" message)))
               'utf-8)))
    (run-hook-with-args 'aside-acp-trace-functions conn 'out line)
    (process-send-string (aside-acp-process conn) (concat line "\n"))))

(defun aside-acp-request (conn method params &optional on-success on-error)
  "Send METHOD with PARAMS to CONN and return the request id.
ON-SUCCESS is called with the result, ON-ERROR with an error plist
holding :code and :message.  Without ON-ERROR, failures are reported
in the echo area."
  (let ((id (cl-incf (aside-acp-next-id conn))))
    (puthash id (cons on-success on-error) (aside-acp-pending conn))
    (aside-acp--send conn (list :id id :method method :params params))
    id))

(defun aside-acp-notify (conn method params)
  "Send the notification METHOD with PARAMS to CONN."
  (aside-acp--send conn (list :method method :params params)))

(defun aside-acp-request-sync (conn method params &optional timeout)
  "Send METHOD with PARAMS to CONN and wait for the result.
Signal `aside-acp-error' if the agent answers with an error, and
`user-error' after TIMEOUT seconds (default 30).  \\[keyboard-quit]
stops waiting.  Never call this from a process filter or handler."
  (let* ((done nil) result failure
         (deadline (+ (float-time) (or timeout 30))))
    (aside-acp-request conn method params
                       (lambda (value) (setq done t result value))
                       (lambda (err) (setq done t failure err)))
    (while (and (not done) (< (float-time) deadline)
                (aside-acp-live-p conn))
      (accept-process-output (aside-acp-process conn) 0.05))
    (cond (failure (signal 'aside-acp-error (list (aside-acp-error-text failure))))
          (done result)
          ((not (aside-acp-live-p conn))
           (signal 'aside-acp-error (list "the agent exited")))
          (t (user-error "%s did not answer %s in time"
                         (aside-acp-name conn) method)))))

(defun aside-acp-error-text (err)
  "Return a readable description of the error plist ERR."
  (let* ((message (plist-get err :message))
         (data (plist-get err :data))
         (details (cond ((stringp data) data)
                        ((consp data) (or (plist-get data :message)
                                          (plist-get data :details)
                                          (plist-get data :error))))))
    (cond ((and (stringp details) (not (string-empty-p details))
                (not (equal details message)))
           ;; JSON-RPC's own names for errors say nothing the details don't.
           (if (and message (not (member message '("Internal error" "Invalid params"
                                                   "Invalid request" "Server error"))))
               (format "%s: %s" message details)
             (concat (upcase (substring details 0 1)) (substring details 1))))
          (message message)
          (t (format "%S" err)))))

;;;; Receiving

(defun aside-acp--filter (process output)
  "Collect OUTPUT from PROCESS and dispatch every complete line."
  (let ((conn (process-get process 'aside-acp))
        (lines nil))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (insert output)
        (when (string-search "\n" output)
          (goto-char (point-min))
          (let ((start (point-min)))
            (while (search-forward "\n" nil t)
              (push (buffer-substring-no-properties start (1- (point))) lines)
              (setq start (point)))
            (delete-region (point-min) start)))))
    (dolist (line (nreverse lines))
      (unless (string-blank-p line)
        (aside-acp--dispatch conn line)))))

(defun aside-acp--dispatch (conn line)
  "Route the JSON message LINE received on CONN."
  (run-hook-with-args 'aside-acp-trace-functions conn 'in line)
  (let ((message (condition-case nil
                     (json-parse-string line :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object :false)
                   (json-error nil))))
    (when (consp message)
      (let ((id (plist-get message :id))
            (method (plist-get message :method)))
        (cond
         ((and method id) (aside-acp--handle-request conn id method message))
         (method (aside-acp--call (aside-acp-notification-handler conn)
                                  conn method (plist-get message :params)))
         (id (aside-acp--handle-response conn id message)))))))

(defun aside-acp--handle-response (conn id message)
  "Deliver MESSAGE, the response to request ID, on CONN."
  (when-let* ((callbacks (gethash id (aside-acp-pending conn))))
    (remhash id (aside-acp-pending conn))
    (if-let* ((err (plist-get message :error)))
        (if (cdr callbacks)
            (aside-acp--call (cdr callbacks) err)
          (message "%s: %s" (aside-acp-name conn) (aside-acp-error-text err)))
      (aside-acp--call (car callbacks) (plist-get message :result)))))

(defun aside-acp--handle-request (conn id method message)
  "Pass the agent's request METHOD (with ID and MESSAGE) on CONN to its handler."
  (let* ((answered nil)
         (reply (lambda (result &optional err)
                  (unless answered
                    (setq answered t)
                    (when (aside-acp-live-p conn)
                      (aside-acp--send
                       conn (if err
                                (list :id id :error err)
                              (list :id id :result result))))))))
    (condition-case err
        (if-let* ((handler (aside-acp-request-handler conn)))
            (funcall handler conn method (plist-get message :params) reply)
          (funcall reply nil (list :code -32601 :message "Method not found")))
      (error (funcall reply nil (list :code -32603
                                      :message (error-message-string err)))))))

(defun aside-acp--call (function &rest args)
  "Apply FUNCTION to ARGS, reporting rather than propagating errors."
  (when function
    (condition-case err
        (apply function args)
      (error (message "aside: %s" (error-message-string err))))))

(defun aside-acp--sentinel (process event)
  "Report that PROCESS ended with EVENT, then fail its outstanding requests.
The exit handler runs first, so it can explain the failure better than
the failed requests can."
  (unless (process-live-p process)
    (let ((conn (process-get process 'aside-acp))
          (why (string-trim event)))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process)))
      (aside-acp--call (aside-acp-exit-handler conn) conn why)
      (let ((callbacks (hash-table-values (aside-acp-pending conn))))
        (clrhash (aside-acp-pending conn))
        (dolist (pair callbacks)
          (aside-acp--call (cdr pair)
                           (list :code -32000
                                 :message (format "%s stopped (%s)"
                                                  (aside-acp-name conn) why))))))))

(provide 'aside-acp)
;;; aside-acp.el ends here
