;;; aside-fake-agent.el --- Replay a recorded agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Plays the agent's side of a transcript recorded by aside-record.el,
;; so tests can drive aside against what real agents actually sent.
;; Emacs runs it in batch mode: load this file, call `aside-fake-agent',
;; and give it the transcript FILE and project DIR as arguments.
;;
;; Whenever the client sends what the recording shows the client sending
;; next, the agent's messages that followed it are sent back, in order.
;; Client messages the recording doesn't have are refused, and recorded
;; client messages that never come are skipped along with the agent's
;; replies to them.  Response ids follow the client's own request ids,
;; and the recorded project directory, /project, becomes DIR.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar aside-fake--entries nil
  "The transcript as a vector of (DIRECTION . MESSAGE).")

(defvar aside-fake--next 0
  "Index of the next unplayed transcript entry.")

(defvar aside-fake--ids (make-hash-table :test #'equal)
  "Map from recorded client request ids to live ones.")

(defvar aside-fake--project "/project"
  "Directory that replaces /project in replayed messages.")

(defun aside-fake--parse (line)
  "Parse the JSON LINE into an alist."
  (json-parse-string line :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun aside-fake--load (file)
  "Read the transcript FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (vconcat
     (mapcar (lambda (line)
               (let ((entry (aside-fake--parse line)))
                 (cons (intern (alist-get 'dir entry)) (alist-get 'msg entry))))
             (split-string (buffer-string) "\n" t)))))

(defun aside-fake--send (message)
  "Write MESSAGE to standard output as one JSON line."
  (princ (string-replace
          "/project" aside-fake--project
          (decode-coding-string (json-serialize message) 'utf-8)))
  (terpri))

(defun aside-fake--kind (message)
  "Return MESSAGE's kind: `request', `notification' or `response'."
  (cond ((and (alist-get 'method message) (alist-get 'id message)) 'request)
        ((alist-get 'method message) 'notification)
        (t 'response)))

(defun aside-fake--matches-p (recorded live)
  "Return non-nil if the LIVE client message plays the RECORDED one."
  (and (eq (aside-fake--kind recorded) (aside-fake--kind live))
       (if (eq (aside-fake--kind live) 'response)
           (equal (alist-get 'id recorded) (alist-get 'id live))
         (equal (alist-get 'method recorded) (alist-get 'method live)))))

(defun aside-fake--find (live)
  "Return the index of the next recorded client message LIVE plays."
  (cl-loop for i from aside-fake--next below (length aside-fake--entries)
           for (direction . recorded) = (aref aside-fake--entries i)
           when (and (eq direction 'out) (aside-fake--matches-p recorded live))
           return i))

(defun aside-fake--reply (message)
  "Return the recorded agent MESSAGE ready to send, or nil to skip it."
  (if (eq (aside-fake--kind message) 'response)
      (when-let* ((id (gethash (alist-get 'id message) aside-fake--ids)))
        (cons (cons 'id id) (assq-delete-all 'id (copy-alist message))))
    message))

(defun aside-fake--play-from (index)
  "Send the agent's messages from INDEX up to the client's next one."
  (setq aside-fake--next index)
  (while (and (< aside-fake--next (length aside-fake--entries))
              (eq (car (aref aside-fake--entries aside-fake--next)) 'in))
    (when-let* ((message (aside-fake--reply (cdr (aref aside-fake--entries aside-fake--next)))))
      (aside-fake--send message))
    (cl-incf aside-fake--next)))

(defun aside-fake--receive (live)
  "Answer the client message LIVE."
  (if-let* ((index (aside-fake--find live)))
      (progn
        (when (eq (aside-fake--kind live) 'request)
          (puthash (alist-get 'id (cdr (aref aside-fake--entries index)))
                   (alist-get 'id live) aside-fake--ids))
        (aside-fake--play-from (1+ index)))
    (when (eq (aside-fake--kind live) 'request)
      (aside-fake--send
       `((jsonrpc . "2.0") (id . ,(alist-get 'id live))
         (error . ((code . -32601)
                   (message . ,(format "%s is not in the transcript"
                                       (alist-get 'method live))))))))))

(defun aside-fake-agent ()
  "Replay the transcript named on the command line."
  (setq aside-fake--entries (aside-fake--load (pop command-line-args-left))
        aside-fake--project (or (pop command-line-args-left) "/project"))
  (condition-case nil
      (while t
        (let ((line (read-from-minibuffer "")))
          (unless (string-blank-p line)
            (aside-fake--receive (aside-fake--parse line)))))
    (end-of-file nil)))

(provide 'aside-fake-agent)
;;; aside-fake-agent.el ends here
