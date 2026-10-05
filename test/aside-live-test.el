;;; aside-live-test.el --- aside against real agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The same popup flow the replayed tests use, against the real agents:
;; a reply, a file write (approving any permission asked) and a shell
;; command.  It costs a little quota, so it only runs when asked:
;;
;;   make live AGENTS="opencode claude codex cline"
;;
;; Each agent's program must be on PATH, or named in the environment
;; variable ASIDE_<AGENT>_COMMAND, such as ASIDE_CLAUDE_COMMAND.  Cheap
;; models are chosen where the agent offers them.

;;; Code:

(require 'aside-test)

(defconst aside-live--options
  '((opencode ("model" . "opencode/big-pickle"))
    (claude ("model" . "haiku") ("effort" . "low"))
    (codex ("model" . "gpt-6-luna") ("reasoning_effort" . "low"))
    (cline ("model" . "nex-agi/nex-n2.5-pro:free")))
  "Cheap settings for each agent.")

(defun aside-live--wanted-p (agent)
  "Return non-nil if AGENT should be tested live."
  (member (symbol-name agent) (split-string (or (getenv "ASIDE_LIVE") ""))))

(defun aside-live--spec (agent)
  "Return AGENT's entry for `aside-agents', with any command override."
  (let* ((spec (copy-sequence (alist-get agent aside-agents)))
         (override (getenv (format "ASIDE_%s_COMMAND" (upcase (symbol-name agent))))))
    (if override
        (plist-put spec :command (split-string-shell-command override))
      spec)))

(defun aside-live--finish-approving ()
  "Wait for the turn to end, approving each permission request."
  (let ((deadline (+ (float-time) 300)))
    (while (aside--busy-p)
      (when (> (float-time) deadline)
        (ert-fail "The agent took more than five minutes"))
      (when-let* ((request (car (aside-turn-requests aside--turn))))
        (aside--answer request (cl-find "allow_once"
                                        (plist-get (aside-turn-block-request request) :options)
                                        :key (lambda (o) (plist-get o :kind))
                                        :test #'equal)))
      (accept-process-output nil 0.05))))

(defun aside-live--run (agent)
  "Take AGENT through a reply, a file write and a shell command."
  (let* ((dir (aside-test--project))
         (aside-agents (list (cons agent (aside-live--spec agent))))
         (aside-session-options aside-live--options)
         (aside-default-agent agent)
         (aside-notify nil)
         (aside--connections nil)
         (aside--preferences nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (when (eq agent 'opencode)
      (with-temp-file (expand-file-name "opencode.json" dir)
        (insert "{\"permission\": {\"edit\": \"ask\", \"bash\": \"ask\"}}\n")))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (aside)
          (aside-test--wait (lambda () (memq aside--state '(ready failed))) "the session" 120)
          (should-not aside--problem)
          (aside-test--send (nth 0 aside-test--prompts))
          (aside-live--finish-approving)
          (should-not (aside-turn-error aside--turn))
          (should (string-match-p "pong" (downcase (aside-turn-answer aside--turn))))
          (aside-test--send (nth 1 aside-test--prompts))
          (aside-live--finish-approving)
          (should-not (aside-turn-error aside--turn))
          (should (equal (string-trim (with-temp-buffer
                                        (insert-file-contents (expand-file-name "notes.txt" dir))
                                        (buffer-string)))
                         "hello from aside"))
          (aside-test--send (nth 2 aside-test--prompts))
          (aside-live--finish-approving)
          (should (string-search "aside-42" (aside-turn-answer aside--turn)))
          (message "live %s:\n%s" agent (aside-test--text)))
      (mapc #'kill-buffer (aside--popups))
      (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
      (delete-directory dir t))))

(defmacro aside-live--deftest (agent)
  "Define the live test for AGENT."
  `(ert-deftest ,(intern (format "aside-live-%s" agent)) ()
     ,(format "%s, for real." (aside--agent-name agent))
     :tags '(live)
     (skip-unless (aside-live--wanted-p ',agent))
     (aside-live--run ',agent)))

(aside-live--deftest opencode)
(aside-live--deftest claude)
(aside-live--deftest codex)
(aside-live--deftest cline)

(provide 'aside-live-test)
;;; aside-live-test.el ends here
