;;; aside-test.el --- Tests for aside  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Most of these tests drive the real popup against real agents'
;; recorded sessions, replayed by aside-fake-agent.el: nothing in
;; aside is stubbed, and the agent's side is what OpenCode, Claude Code,
;; Codex and Cline actually sent (see aside-record.el).  The rest check
;; pieces that are easier to pin down on their own.
;;
;; Run with `make test'.  `make live' runs the same flow against the
;; real agents instead; see aside-live-test.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aside)

(defconst aside-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "This directory.")

(defun aside-test--transcript (name)
  "Return the recorded transcript called NAME."
  (expand-file-name (format "transcripts/%s.jsonl" name) aside-test--dir))

(defun aside-test--wait (predicate what &optional timeout)
  "Process input until PREDICATE holds; fail after TIMEOUT seconds naming WHAT."
  (let ((deadline (+ (float-time) (or timeout 15))))
    (while (not (funcall predicate))
      (when (> (float-time) deadline)
        (ert-fail (format "Timed out waiting for %s" what)))
      (accept-process-output nil 0.02))))

(defun aside-test--project ()
  "Return a new empty git project directory."
  (let ((dir (directory-file-name (make-temp-file "aside-test-" t))))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q"))
    dir))

(defmacro aside-test--with-agent (spec &rest body)
  "Run BODY with an agent replaying a transcript, in a fresh project.
SPEC is (AGENT TRANSCRIPT); AGENT is the name it goes by in
`aside-agents'.  BODY runs with `dir' bound to the project directory."
  (declare (indent 1))
  (let ((agent (car spec)) (transcript (cadr spec)))
    `(let* ((dir (aside-test--project))
            (aside-agents
             (list (cons ',agent
                         (list :name (plist-get (alist-get ',agent aside-agents) :name)
                               :login (plist-get (alist-get ',agent aside-agents) :login)
                               :command (list (expand-file-name invocation-name invocation-directory)
                                              "--batch" "-Q" "-l"
                                              (expand-file-name "aside-fake-agent.el"
                                                                aside-test--dir)
                                              "-f" "aside-fake-agent"
                                              (aside-test--transcript ,transcript) dir)))))
            (aside-default-agent ',agent)
            (aside-notify nil)
            (aside--connections nil)
            (aside--waiting nil)
            (aside--last-agent nil)
            (aside--preferences nil)
            (aside--sessions (make-hash-table :test #'equal)))
       (unwind-protect
           (progn ,@body)
         (dolist (buffer (aside--popups))
           (kill-buffer buffer))
         (pcase-dolist (`(,_ . ,conn) aside--connections)
           (aside-acp-stop conn))
         (delete-directory dir t)))))

(defun aside-test--open (dir)
  "Open the popup for DIR and wait until its session is ready."
  (let ((default-directory (file-name-as-directory dir)))
    (aside))
  (aside-test--wait (lambda () (eq aside--state 'ready)) "the session")
  (current-buffer))

(defun aside-test--send (text)
  "Write TEXT as the prompt and send it."
  (goto-char (point-max))
  (insert text)
  (aside-send))

(defun aside-test--finish ()
  "Wait for the current turn to end."
  (aside-test--wait (lambda () (not (aside--busy-p))) "the turn to end"))

(defun aside-test--request ()
  "Wait for a permission request and return its block."
  (aside-test--wait (lambda () (aside-turn-requests aside--turn)) "a permission request")
  (car (aside-turn-requests aside--turn)))

(defun aside-test--text ()
  "Return the popup's text."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun aside-test--should-show (&rest strings)
  "Check that every one of STRINGS appears in the popup."
  (dolist (string strings)
    (should (string-search string (aside-test--text)))))

(defun aside-test--cancel-after-first-words ()
  "Stop the turn once the agent has started answering."
  (aside-test--wait (lambda () (aside-turn-blocks aside--turn)) "the agent to start")
  (aside-cancel)
  (aside-test--finish))

;;;; Whole sessions, replayed

(defconst aside-test--prompts
  '("Reply with exactly the word pong and nothing else. Do not use any tools."
    "Create a file named notes.txt in the current directory containing exactly one line: hello from aside. Then reply with the single word: done."
    "Run the shell command `echo aside-$((40+2))` and reply with exactly what it printed."
    "Count from 1 to 300, one number per line. Do not use any tools.")
  "The prompts the transcripts were recorded with.")

(ert-deftest aside-opencode-session ()
  "OpenCode: replies, a file write and a command it asks permission for, a stop."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (should (equal (aside--option-label "model") "Big Pickle"))
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "── OpenCode · Big Pickle" "pong")
      (should (equal (aside--prompt-text) ""))

      (aside-test--send (nth 1 aside-test--prompts))
      (aside-test--request)
      (aside-test--should-show "? notes.txt" "+hello from aside" "o Allow once" "r Reject")
      ;; Options can be chosen at point too, as a click or RET would.
      (goto-char (point-min))
      (search-forward "Allow once")
      (goto-char (match-beginning 0))
      (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
                  #'aside-answer-at-point))
      (aside-answer-at-point)
      (aside-test--finish)
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name "notes.txt" dir))
                       (buffer-string))
                     "hello from aside\n"))
      (aside-test--should-show "done" "edited notes.txt")
      (should-not (string-search "Allow once" (aside-test--text)))

      (aside-test--send (nth 2 aside-test--prompts))
      (aside-test--request)
      (aside-test--should-show "$ echo aside-$((40+2))")
      (aside-answer "o")
      (aside-test--finish)
      (aside-test--should-show "aside-42" "ran 1 command")

      (aside-test--send (nth 3 aside-test--prompts))
      (aside-test--cancel-after-first-words)
      (aside-test--should-show "cancelled"))))

(ert-deftest aside-claude-session ()
  "Claude Code: asks before writing a file, which it then writes itself."
  (aside-test--with-agent (claude "claude-session")
    (with-current-buffer (aside-test--open dir)
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "── Claude Code" "pong")

      (aside-test--send (nth 1 aside-test--prompts))
      (let ((request (aside-test--request)))
        (should (equal (mapcar #'car (aside-turn-request-keys
                                      (aside-turn-block-request request)))
                       '("o" "a" "r"))))
      (aside-test--should-show "? Write notes.txt" "+hello from aside"
                               "Yes, allow all edits during this session")
      (aside-answer "o")
      (aside-test--finish)
      (aside-test--should-show "done" "edited notes.txt")

      (aside-test--send (nth 2 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "aside-42" "ran 1 command")

      (aside-test--send (nth 3 aside-test--prompts))
      (aside-test--cancel-after-first-words)
      (aside-test--should-show "cancelled"))))

(ert-deftest aside-codex-session ()
  "Codex: separate messages stay separate paragraphs; edits happen through the shell."
  (aside-test--with-agent (codex "codex-session")
    (with-current-buffer (aside-test--open dir)
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "── Codex" "pong")

      (aside-test--send (nth 1 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "requested single line.\n\ndone" "ran 1 command")

      (aside-test--send (nth 2 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "aside-42"))))

(ert-deftest aside-cline-signed-out ()
  "Cline: a signed-out agent's error says how to sign in."
  (aside-test--with-agent (cline "cline-error")
    (with-current-buffer (aside-test--open dir)
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--should-show "Cline requires re-authentication. Sign in with"
                               "cline auth")
      (should-not (string-search ".." (aside-test--text)))
      (should (aside-turn-error aside--turn))
      ;; The prompt can be written again right away.
      (should-not buffer-read-only)
      (should (equal (aside--prompt-text) "")))))

(ert-deftest aside-resume-shows-last-exchange ()
  "Resuming a session shows its last answered exchange, for each agent."
  (pcase-dolist (`(,agent ,prompt ,answer)
                 `((opencode ,(nth 3 aside-test--prompts) "1")
                   (claude ,(nth 3 aside-test--prompts) "1")
                   (codex ,(nth 2 aside-test--prompts) "aside-42")))
    (let* ((dir (aside-test--project))
           (transcript (aside-test--transcript (format "%s-load" agent)))
           (aside-agents
            (list (cons agent (list :name (aside--agent-name agent)
                                    :command (list (expand-file-name invocation-name
                                                                     invocation-directory)
                                                   "--batch" "-Q" "-l"
                                                   (expand-file-name "aside-fake-agent.el"
                                                                     aside-test--dir)
                                                   "-f" "aside-fake-agent" transcript dir)))))
           (aside--connections nil)
           (aside--sessions (make-hash-table :test #'equal)))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (car (all-completions "" collection)))))
            (let ((default-directory (file-name-as-directory dir))
                  (aside-default-agent agent))
              (aside-resume))
            (aside-test--wait (lambda () (eq aside--state 'ready)) "the session to load")
            (aside-test--should-show prompt answer)
            (should (equal (aside--prompt-text) "")))
        (mapc #'kill-buffer (aside--popups))
        (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
        (delete-directory dir t)))))

(ert-deftest aside-region-goes-with-the-next-prompt ()
  "A region selected when opening the popup is sent with the next prompt."
  (aside-test--with-agent (opencode "opencode-session")
    (let ((file (expand-file-name "lib.el" dir))
          sent)
      (with-temp-file file (insert "(defun f ()\n  1)\n(defun g ()\n  2)\n"))
      (with-current-buffer (find-file-noselect file)
        (let ((transient-mark-mode t))
          (goto-char (point-min))
          (forward-line 2)
          (set-mark (point))
          (goto-char (point-max))
          (aside)))
      (set-buffer (aside--project-popup dir))
      (aside-test--wait (lambda () (eq aside--state 'ready)) "the session")
      (should (string-search "lib.el:3-4" (format "%s" header-line-format)))
      (add-hook 'aside-acp-trace-functions
                (lambda (_ direction line)
                  (when (and (eq direction 'out) (string-search "session/prompt" line))
                    (setq sent line))))
      (unwind-protect
          (progn
            (aside-test--send (nth 0 aside-test--prompts))
            (aside-test--finish))
        (setq aside-acp-trace-functions nil)
        (kill-buffer (get-file-buffer file)))
      (should (string-search "\"type\":\"resource\"" sent))
      (should (string-search (format "file://%s#L3-4" file) sent))
      (should (string-search "(defun g ()" sent))
      (should-not header-line-format))))

(ert-deftest aside-toggles-the-project-popup ()
  "Calling `aside' again from the popup hides it; calling it elsewhere brings it back."
  (aside-test--with-agent (opencode "opencode-session")
    (let ((popup (aside-test--open dir)))
      (should (aside-frame-selected-p popup))
      (let ((default-directory (file-name-as-directory dir)))
        (aside))
      (should-not (aside-frame-visible-p popup))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory dir))
        (aside))
      (should (eq (window-buffer (selected-window)) popup))
      (should (aside-frame-visible-p popup)))))

(ert-deftest aside-survives-an-agent-crash ()
  "If the agent dies mid-turn, the turn ends with the reason and the popup stays usable."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--send (nth 1 aside-test--prompts))
      (aside-test--request)
      (delete-process (aside-acp-process aside--conn))
      (aside-test--finish)
      (aside-test--should-show "OpenCode stopped (killed)")
      (should-not (string-search "Allow once" (aside-test--text)))
      (should-not aside-request-mode)
      (should-not buffer-read-only)
      (should-not aside--conn))))

(ert-deftest aside-applies-session-options ()
  "New sessions get the options in `aside-session-options' before the first prompt."
  (aside-test--with-agent (opencode "opencode-session")
    (let ((aside-session-options '((opencode ("model" . "opencode-go/glm-5.3"))
                                   (claude ("model" . "haiku"))))
          (sent nil))
      (add-hook 'aside-acp-trace-functions
                (lambda (_ direction line)
                  (when (and (eq direction 'out) (string-search "set_config_option" line))
                    (push line sent))))
      (unwind-protect
          (aside-test--open dir)
        (setq aside-acp-trace-functions nil))
      (should (= (length sent) 1))
      (should (string-search "\"value\":\"opencode-go/glm-5.3\"" (car sent))))))

;;;; The transport

(ert-deftest aside-acp-reassembles-split-lines ()
  "Messages split across reads, or sharing one, are each dispatched once."
  (let* ((seen nil)
         (conn (aside-acp--make :name "test"
                                :notification-handler
                                (lambda (_conn method params)
                                  (push (cons method (plist-get params :n)) seen))))
         (process (make-process :name "aside-test" :command '("sleep" "30")
                                :buffer (generate-new-buffer " *aside-test*")
                                :noquery t)))
    (unwind-protect
        (progn
          (process-put process 'aside-acp conn)
          (aside-acp--filter process "{\"jsonrpc\":\"2.0\",\"method\":\"a\",\"par")
          (should-not seen)
          (aside-acp--filter process "ams\":{\"n\":1}}\n{\"jsonrpc\":\"2.0\",\"method\":\"b\",\"params\":{\"n\":2}}\n\n{\"jsonrpc\"")
          (aside-acp--filter process ":\"2.0\",\"method\":\"c\",\"params\":{\"n\":3}}\n")
          (should (equal (reverse seen) '(("a" . 1) ("b" . 2) ("c" . 3)))))
      (delete-process process)
      (kill-buffer (process-buffer process)))))

(ert-deftest aside-acp-reports-an-agent-that-wont-start ()
  "An agent that exits at once fails its handshake with a reason."
  (let (problem)
    (aside-acp-start :name "broken" :command '("sh" "-c" "echo nope >&2; exit 3")
                     :on-error (lambda (err) (setq problem (aside-acp-error-text err))))
    (aside-test--wait (lambda () problem) "the failure")
    (should (equal problem "broken stopped (exited abnormally with code 3)"))))

(ert-deftest aside-explains-an-agent-that-wont-start ()
  "A popup whose agent dies at start shows why, with the agent's own words."
  (let* ((dir (aside-test--project))
         (aside-agents '((codex :name "Codex"
                                :command ("sh" "-c" "echo 'Error: no credentials' >&2; exit 1"))))
         (aside-default-agent 'codex)
         (aside--connections nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer (let ((default-directory (file-name-as-directory dir)))
                               (aside)
                               (current-buffer))
          (aside-test--wait (lambda () (eq aside--state 'failed)) "the failure")
          (should (equal aside--problem
                         "Codex stopped (exited abnormally with code 1)\nError: no credentials"))
          (should (string-search "couldn't start" (aside--mode-line))))
      (mapc #'kill-buffer (aside--popups))
      (delete-directory dir t))))

(ert-deftest aside-explains-an-agent-it-cant-find ()
  "A popup whose agent program isn't on `exec-path' says so, and how to fix it."
  (let* ((dir (aside-test--project))
         (aside-agents '((codex :name "Codex" :command ("aside-no-such-program")
                                :install "npm install -g @agentclientprotocol/codex-acp")))
         (aside-default-agent 'codex)
         (aside--connections nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer (let ((default-directory (file-name-as-directory dir)))
                               (aside)
                               (current-buffer))
          (aside-test--wait (lambda () (eq aside--state 'failed)) "the failure")
          (should (equal aside--problem
                         (format-message "Can't find `aside-no-such-program' on `exec-path'. Install Codex (npm install -g @agentclientprotocol/codex-acp), or add its directory to `exec-path'"))))
      (mapc #'kill-buffer (aside--popups))
      (delete-directory dir t))))

(ert-deftest aside-acp-error-text-reads-details ()
  "Error details are shown however the agent nests them."
  (should (equal (aside-acp-error-text '(:code -32603 :message "Internal error"
                                         :data (:details "cline requires re-authentication.")))
                 "Cline requires re-authentication."))
  (should (equal (aside-acp-error-text '(:code 1 :message "Quota" :data (:message "used up")))
                 "Quota: used up"))
  (should (equal (aside-acp-error-text '(:code 1 :message "Bad" :data "worse"))
                 "Bad: worse"))
  (should (equal (aside-acp-error-text '(:code 1 :message "Plain")) "Plain")))

;;;; Files

(ert-deftest aside-reads-files-as-emacs-has-them ()
  "Agents reading a file see unsaved edits, and can ask for some lines."
  (let* ((dir (make-temp-file "aside-files-" t))
         (file (expand-file-name "a.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "one\ntwo\nthree\nfour\n"))
          (should (equal (aside--read-file (list :path file :line 2 :limit 2)) "two\nthree\n"))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (insert "zero\n")
            (should (equal (aside--read-file (list :path file :limit 1)) "zero\n"))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (delete-directory dir t))))

(ert-deftest aside-writes-files-without-clobbering-edits ()
  "Writing updates an unmodified buffer, but never one with unsaved edits."
  (let* ((dir (make-temp-file "aside-files-" t))
         (clean (expand-file-name "clean.txt" dir))
         (dirty (expand-file-name "dirty.txt" dir))
         (fresh (expand-file-name "new/fresh.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file clean (insert "old\n"))
          (with-temp-file dirty (insert "old\n"))
          (let ((clean-buffer (find-file-noselect clean))
                (dirty-buffer (find-file-noselect dirty)))
            (with-current-buffer dirty-buffer (insert "mine "))
            (aside--write-file (list :path clean :content "new\n"))
            (aside--write-file (list :path dirty :content "new\n"))
            (aside--write-file (list :path fresh :content "fresh\n"))
            (with-current-buffer clean-buffer
              (should (equal (buffer-string) "new\n"))
              (should-not (buffer-modified-p))
              (should (verify-visited-file-modtime)))
            (with-current-buffer dirty-buffer
              (should (equal (buffer-string) "mine old\n"))
              (should (buffer-modified-p))
              (set-buffer-modified-p nil))
            (should (file-exists-p fresh))
            (kill-buffer clean-buffer)
            (kill-buffer dirty-buffer)))
      (delete-directory dir t))))

(ert-deftest aside-refreshes-buffers-an-agent-changed ()
  "After a turn, unmodified buffers catch up with their files; others are reported."
  (let* ((dir (directory-file-name (make-temp-file "aside-files-" t)))
         (clean (expand-file-name "clean.txt" dir))
         (dirty (expand-file-name "dirty.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file clean (insert "old\n"))
          (with-temp-file dirty (insert "old\n"))
          (let ((clean-buffer (find-file-noselect clean))
                (dirty-buffer (find-file-noselect dirty)))
            (with-current-buffer dirty-buffer (insert "mine "))
            (sleep-for 0.01)
            (with-temp-file clean (insert "agent\n"))
            (with-temp-file dirty (insert "agent\n"))
            (set-file-times clean (time-add nil 2))
            (set-file-times dirty (time-add nil 2))
            (should (equal (with-temp-buffer
                             (setq aside--root dir)
                             (aside--refresh-buffers))
                           (list dirty)))
            (should (equal (with-current-buffer clean-buffer (buffer-string)) "agent\n"))
            (should (equal (with-current-buffer dirty-buffer (buffer-string)) "mine old\n"))
            (with-current-buffer dirty-buffer (set-buffer-modified-p nil))
            (kill-buffer clean-buffer)
            (kill-buffer dirty-buffer)))
      (delete-directory dir t))))

;;;; How things look

(ert-deftest aside-turn-highlights-markdown ()
  "Answers get highlighted code, inline code and emphasis."
  (with-temp-buffer
    (insert "## Fix\nUse **care** with `x`, *gently*.\n* a bullet *\n```elisp\n(setq x 1)\n```\n")
    (aside-turn-fontify-markdown (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "Fix")
    (should (eq (get-text-property (1- (point)) 'face) 'aside-heading))
    (search-forward "care")
    (should (memq 'bold (ensure-list (get-text-property (1- (point)) 'face))))
    (search-forward "`x")
    (should (eq (get-text-property (1- (point)) 'face) 'aside-code))
    (search-forward "gently")
    (should (memq 'italic (ensure-list (get-text-property (1- (point)) 'face))))
    (search-forward "bullet")
    (should-not (get-text-property (1- (point)) 'face))
    (search-forward "setq")
    (should (get-text-property (1- (point)) 'face))
    (should-not (eq (get-text-property (1- (point)) 'face) 'aside-code))))

(ert-deftest aside-turn-summarises-what-happened ()
  "The summary names edited files, commands, failures and how it ended."
  (let ((turn (aside-turn-create :prompt "p" :started (- (float-time) 75)
                                 :finished (float-time) :stop-reason "end_turn")))
    (aside-turn-update turn '(:sessionUpdate "tool_call" :toolCallId "1" :kind "edit"
                              :status "completed" :locations ((:path "/p/a.el"))))
    (aside-turn-update turn '(:sessionUpdate "tool_call" :toolCallId "2" :kind "execute"
                              :status "failed" :title "make"))
    (let ((summary (substring-no-properties (aside-turn-summary turn "/p"))))
      (should (string-search "edited a.el" summary))
      (should (string-search "ran 1 command" summary))
      (should (string-search "1 failed" summary))
      (should (string-search "1m 15s" summary)))
    (setf (aside-turn-stop-reason turn) "cancelled")
    (should (string-search "cancelled" (aside-turn-summary turn "/p")))))

(ert-deftest aside-mode-line-says-what-is-going-on ()
  "The mode line names the agent and model and shows its state."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (let ((line (substring-no-properties (aside--mode-line))))
        (should (string-match-p "\\` OpenCode · Big Pickle · Build" line)))
      ;; The replay follows the recording, so the first prompt comes first.
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--send (nth 1 aside-test--prompts))
      (aside-test--request)
      (should (string-search "needs your answer" (aside--mode-line)))
      (aside-answer "o")
      (aside-test--finish)
      (should-not (string-search "needs your answer" (aside--mode-line)))
      ;; A % must reach the mode line escaped, in the same face as its number.
      (let* ((aside--usage '(:used 42 :size 200))
             (line (aside--mode-line))
             (at (string-search "21%%" line)))
        (should at)
        (should (equal (get-text-property at 'face line)
                       (get-text-property (+ at 2) 'face line)))))))

(ert-deftest aside-shows-no-line-numbers ()
  "Popups have no line numbers, even when they are on everywhere else."
  (let ((was global-display-line-numbers-mode))
    (global-display-line-numbers-mode 1)
    (unwind-protect
        (aside-test--with-agent (opencode "opencode-session")
          (with-current-buffer (aside-test--open dir)
            (should-not display-line-numbers)))
      (global-display-line-numbers-mode (if was 1 -1)))))

(ert-deftest aside-placeholder-only-in-an-empty-prompt ()
  "The hint shows in an empty prompt and goes away when you type."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (should (overlayp aside--placeholder))
      (should (string-search "Ask OpenCode" (overlay-get aside--placeholder 'after-string)))
      (insert "x")
      (should-not aside--placeholder)
      (delete-char -1)
      (should (overlayp aside--placeholder)))))

(ert-deftest aside-refuses-to-send-nothing-or-twice ()
  "Empty prompts aren't sent, and a second prompt waits for the first."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (should-error (aside-send) :type 'user-error)
      (aside-test--send (nth 0 aside-test--prompts))
      (should-error (aside-send) :type 'user-error)
      (aside-test--finish))))

;;;; Evil

(defvar evil-mode)
(defvar evil-state)
(declare-function evil-mode "evil-core")
(declare-function evil-normal-state "evil-states")
(declare-function evil-ex-completed-binding "evil-ex")
(declare-function evil-write "evil-commands")

(ert-deftest aside-evil-writes-and-quits ()
  "With Evil, :w sends, :wq sends and hides, :q hides, and q hides too."
  (skip-unless (require 'evil nil t))
  (let ((evil-mode-was evil-mode))
    (evil-mode 1)
    (unwind-protect
        (aside-test--with-agent (opencode "opencode-session")
          (with-current-buffer (aside-test--open dir)
            (should (eq evil-state 'insert))
            (should (eq (evil-ex-completed-binding "w") #'aside-send))
            (should (eq (evil-ex-completed-binding "wq") #'aside-send-and-dismiss))
            (should (eq (evil-ex-completed-binding "q") #'aside-dismiss))
            (evil-normal-state)
            (should (eq (key-binding "q") #'aside-dismiss))
            ;; Elsewhere, :w still saves.
            (with-temp-buffer
              (should (eq (evil-ex-completed-binding "w") #'evil-write)))))
      (unless evil-mode-was (evil-mode -1)))))

(ert-deftest aside-evil-answers-permission-with-one-key ()
  "With Evil in normal state, o, a and r answer a permission request."
  (skip-unless (require 'evil nil t))
  (let ((evil-mode-was evil-mode))
    (evil-mode 1)
    (unwind-protect
        (aside-test--with-agent (opencode "opencode-session")
          (with-current-buffer (aside-test--open dir)
            (aside-test--send (nth 0 aside-test--prompts))
            (aside-test--finish)
            (aside-test--send (nth 1 aside-test--prompts))
            (aside-test--request)
            (evil-normal-state)
            (should (eq (key-binding "o") #'aside-answer))
            (should (eq (key-binding "r") #'aside-answer))
            (aside-answer "o")
            (aside-test--finish)
            (should-not (eq (key-binding "o") #'aside-answer))))
      (unless evil-mode-was (evil-mode -1)))))

(provide 'aside-test)
;;; aside-test.el ends here
