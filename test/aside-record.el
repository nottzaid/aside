;;; aside-record.el --- Record real agent sessions as test transcripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Drives a real agent through a fixed script and saves every ACP
;; message, in both directions, as JSON lines under test/transcripts/.
;; The test suite replays those files through a fake agent, so the
;; tests exercise what agents actually send rather than what we assume
;; they send.  Re-record whenever an agent changes its behaviour:
;;
;;   make record AGENT=opencode
;;
;; The script, all in one session:
;;   1. a plain reply           ("pong")
;;   2. a file write            (asks permission when the agent is set to)
;;   3. a shell command         (likewise)
;;   4. a cancelled turn
;; then, from a fresh process, listing and loading that session.
;;
;; Recordings use cheap or free models and a throwaway git project.
;; Account details and local paths are scrubbed before saving.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'aside-acp)

(defconst aside-record--transcripts
  (expand-file-name "transcripts" (file-name-directory
                                   (or load-file-name buffer-file-name)))
  "Where recordings are written.")

(defconst aside-record-agents
  '((opencode :command ("opencode" "acp")
              :options (("model" . "opencode/big-pickle"))
              :files (("opencode.json"
                       . "{\"permission\": {\"edit\": \"ask\", \"bash\": \"ask\"}}\n")))
    (claude :command ("claude-agent-acp")
            :options (("model" . "haiku") ("effort" . "low")))
    (codex :command ("codex-acp")
           :options (("model" . "gpt-6-luna") ("reasoning_effort" . "low")))
    (cline :command ("cline" "--acp")
           :options (("model" . "nex-agi/nex-n2.5-pro:free"))))
  "How to record each agent: its command, config options and seed files.")

(defconst aside-record-prompts
  '("Reply with exactly the word pong and nothing else. Do not use any tools."
    "Create a file named notes.txt in the current directory containing exactly one line: hello from aside. Then reply with the single word: done."
    "Run the shell command `echo aside-$((40+2))` and reply with exactly what it printed.")
  "Prompts sent, in order, before the cancelled turn.")

(defconst aside-record-cancel-prompt
  "Count from 1 to 300, one number per line. Do not use any tools.")

(defvar aside-record--log nil
  "Recorded (DIRECTION . LINE) pairs, newest first.")

(defun aside-record--trace (_conn direction line)
  "Remember LINE travelling in DIRECTION."
  (push (cons direction line) aside-record--log))

(defun aside-record--wait (predicate what &optional timeout)
  "Process events until PREDICATE is non-nil.
Fail, naming WHAT was awaited, after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 240))))
    (while (not (funcall predicate))
      (when (> (float-time) deadline)
        (error "Timed out waiting for %s" what))
      (accept-process-output nil 0.1))))

(defun aside-record--handle-request (_conn method params reply)
  "Answer the agent's request METHOD with PARAMS like a permissive editor.
REPLY delivers the answer."
  (pcase method
    ("session/request_permission"
     (let* ((options (plist-get params :options))
            (allow (or (cl-find "allow_once" options
                                :key (lambda (o) (plist-get o :kind))
                                :test #'equal)
                       (car options))))
       (funcall reply (list :outcome (list :outcome "selected"
                                           :optionId (plist-get allow :optionId))))))
    ("fs/read_text_file"
     (let ((path (plist-get params :path)))
       (if (file-readable-p path)
           (funcall reply (list :content (with-temp-buffer
                                           (insert-file-contents path)
                                           (buffer-string))))
         (funcall reply nil (list :code -32002
                                  :message (format "No such file: %s" path))))))
    ("fs/write_text_file"
     (let ((path (plist-get params :path)))
       (make-directory (file-name-directory path) t)
       (with-temp-file path (insert (plist-get params :content)))
       (funcall reply nil)))
    (_ (funcall reply nil (list :code -32601 :message "Method not found")))))

(defun aside-record--start (spec)
  "Start the agent described by SPEC; return the connection."
  (let* ((override (getenv "ASIDE_RECORD_COMMAND"))
         (command (if override (split-string-shell-command override)
                    (plist-get spec :command)))
         (conn nil) failed)
    (setq conn (aside-acp-start
                :name "record" :command command
                :client-info '(:name "aside" :version "record")
                :client-capabilities '(:fs (:readTextFile t :writeTextFile t)
                                           :terminal :false)
                :on-request #'aside-record--handle-request
                :on-ready #'ignore
                :on-error (lambda (err) (setq failed err))))
    (aside-record--wait (lambda () (or failed (aside-acp-ready conn))) "initialize")
    (when failed (error "Initialize failed: %s" (aside-acp-error-text failed)))
    conn))

(defun aside-record--sync (conn method params)
  "Call METHOD with PARAMS on CONN and wait for its result."
  (aside-acp-request-sync conn method params 300))

(defun aside-record--session (spec dir)
  "Record the prompt script for SPEC in DIR; return the session id."
  (let* ((conn (aside-record--start spec))
         (created (aside-record--sync conn "session/new" (list :cwd dir :mcpServers [])))
         (session (plist-get created :sessionId))
         (options (plist-get created :configOptions)))
    ;; Options depend on each other (Claude's haiku has no effort
    ;; setting), so only set those the agent still offers.
    (pcase-dolist (`(,id . ,value) (plist-get spec :options))
      (when (cl-find id options :key (lambda (o) (plist-get o :id)) :test #'equal)
        (setq options (plist-get (aside-record--sync
                                  conn "session/set_config_option"
                                  (list :sessionId session :configId id :value value))
                                 :configOptions))))
    (dolist (prompt aside-record-prompts)
      (message "record: %s" prompt)
      (aside-record--sync conn "session/prompt"
                          (list :sessionId session
                                :prompt (vector (list :type "text" :text prompt)))))
    (message "record: cancelled turn")
    (let ((started nil) (stopped nil))
      (add-hook 'aside-acp-trace-functions
                (lambda (_c direction line)
                  (when (and (eq direction 'in)
                             (string-search "agent_message_chunk" line))
                    (setq started t))))
      (aside-acp-request conn "session/prompt"
                         (list :sessionId session
                               :prompt (vector (list :type "text"
                                                     :text aside-record-cancel-prompt)))
                         (lambda (_) (setq stopped t))
                         (lambda (_) (setq stopped t)))
      (aside-record--wait (lambda () (or started stopped)) "a streamed chunk")
      (aside-acp-notify conn "session/cancel" (list :sessionId session))
      (aside-record--wait (lambda () stopped) "the cancelled turn to end"))
    (aside-acp-stop conn)
    session))

(defun aside-record--load (spec dir session)
  "Record listing and loading SESSION from a fresh SPEC process in DIR."
  (let* ((conn (aside-record--start spec))
         (caps (plist-get (aside-acp-agent-capabilities conn) :sessionCapabilities)))
    (when (plist-member caps :list)
      (aside-record--sync conn "session/list" (list :cwd dir)))
    (aside-record--sync conn "session/load"
                        (list :sessionId session :cwd dir :mcpServers []))
    (aside-acp-stop conn)))

;;;; Scrubbing

(defun aside-record--scrub-text (line dir)
  "Replace DIR, the home directory and email addresses in LINE."
  (let* ((line (string-replace (directory-file-name dir) "/project" line))
         (line (string-replace (directory-file-name (expand-file-name "~"))
                               "/home/user" line)))
    (replace-regexp-in-string "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]+"
                              "user@example.com" line t t)))

(defun aside-record--trim-options (value)
  "Shorten long option lists inside VALUE, keeping the current choice."
  (cond
   ((and (consp value) (assq 'options value) (assq 'currentValue value))
    (let* ((options (cdr (assq 'options value)))
           (current (cdr (assq 'currentValue value)))
           (keep (seq-take (append options nil) 5)))
      (unless (seq-find (lambda (o) (equal (cdr (assq 'value o)) current)) keep)
        (when-let* ((chosen (seq-find (lambda (o) (equal (cdr (assq 'value o)) current))
                                      options)))
          (setq keep (append keep (list chosen)))))
      (mapcar (lambda (pair)
                (if (eq (car pair) 'options)
                    (cons 'options (vconcat keep))
                  (cons (car pair) (aside-record--trim-options (cdr pair)))))
              value)))
   ((and (consp value) (symbolp (car-safe (car-safe value))))
    (mapcar (lambda (pair)
              (cons (car pair)
                    (pcase (car pair)
                      ('availableCommands (aside-record--trim-commands (cdr pair)))
                      ('availableModels (vconcat (seq-take (append (cdr pair) nil) 5)))
                      (_ (aside-record--trim-options (cdr pair))))))
            value))
   ((vectorp value) (vconcat (mapcar #'aside-record--trim-options value)))
   (t value)))

(defun aside-record--trim-commands (commands)
  "Keep two of COMMANDS with short descriptions; the rest describe the machine."
  (vconcat
   (mapcar (lambda (command)
             (mapcar (lambda (pair)
                       (if (and (eq (car pair) 'description) (stringp (cdr pair)))
                           (cons 'description (truncate-string-to-width (cdr pair) 60))
                         pair))
                     command))
           (seq-take (append commands nil) 2))))

(defun aside-record--keep-p (message)
  "Return nil for MESSAGE kinds that carry account details."
  (not (string-prefix-p "_auth" (or (cdr (assq 'method message)) ""))))

(defun aside-record--write (file dir)
  "Write the log to FILE, scrubbed of DIR and personal details, then clear it."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (dolist (entry (reverse aside-record--log))
        (let ((message (json-parse-string (aside-record--scrub-text (cdr entry) dir)
                                          :object-type 'alist :array-type 'array
                                          :null-object :null :false-object :false)))
          (when (aside-record--keep-p message)
            (insert (decode-coding-string
                     (json-serialize
                      (list (cons 'dir (symbol-name (car entry)))
                            (cons 'msg (aside-record--trim-options message))))
                     'utf-8)
                    "\n"))))))
  (setq aside-record--log nil))

;;;; Entry point

(defun aside-record ()
  "Record the agent named by the first command-line argument."
  (let* ((agent (intern (or (pop command-line-args-left)
                            (error "Usage: aside-record AGENT"))))
         (spec (or (alist-get agent aside-record-agents)
                   (error "Unknown agent %s" agent)))
         (dir (file-name-as-directory
               (make-temp-file (format "aside-%s-" agent) t)))
         (out aside-record--transcripts))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q"))
    (pcase-dolist (`(,name . ,content) (plist-get spec :files))
      (with-temp-file (expand-file-name name dir) (insert content)))
    (add-hook 'aside-acp-trace-functions #'aside-record--trace)
    (make-directory out t)
    (let ((session (condition-case err
                       (aside-record--session spec dir)
                     (error
                      ;; A failure is a real error shape worth testing against.
                      (aside-record--write
                       (expand-file-name (format "%s-error.jsonl" agent) out) dir)
                      (signal (car err) (cdr err))))))
      (aside-record--write (expand-file-name (format "%s-session.jsonl" agent) out) dir)
      (aside-record--load spec dir session)
      (aside-record--write (expand-file-name (format "%s-load.jsonl" agent) out) dir))
    (message "record: %s done; project left in %s" agent dir)))

(provide 'aside-record)
;;; aside-record.el ends here
