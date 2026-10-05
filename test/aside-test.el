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
         (dolist (buffer (buffer-list))
           (when (memq (buffer-local-value 'major-mode buffer) '(aside-mode aside-list-mode))
             (kill-buffer buffer)))
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

(defun aside-test--answer (name)
  "Answer the oldest permission request with its option called NAME.
The cursor goes to the option's line and RET is pressed there."
  (let* ((block (car (aside-turn-requests aside--turn)))
         (option (cl-find name (plist-get (aside-turn-block-request block) :options)
                          :key (lambda (o) (plist-get o :name)) :test #'equal))
         (pos (and option (text-property-any (point-min) (point-max) 'aside-option option))))
    (unless pos
      (ert-fail (format "No option called %s" name)))
    (goto-char pos)
    (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
                #'aside-answer-at-point))
    (aside-answer-at-point)))

(defun aside-test--list ()
  "Return the list of choices on screen; fail if there is none."
  (or (cl-find-if (lambda (buffer)
                    (and (eq (buffer-local-value 'major-mode buffer) 'aside-list-mode)
                         (get-buffer-window buffer t)))
                  (buffer-list))
      (ert-fail "No list of choices is shown")))

(defun aside-test--list-text ()
  "Return the heading and the lines of the list on screen."
  (with-current-buffer (aside-test--list)
    (concat (substring-no-properties header-line-format) "\n"
            (buffer-substring-no-properties (point-min) (point-max))
            (mapconcat (lambda (overlay)
                         (substring-no-properties (or (overlay-get overlay 'after-string) "")))
                       (overlays-in (point-min) (point-max)) ""))))

(defun aside-test--choose (&optional label)
  "Choose LABEL, or else the choice under the cursor, in the list on screen.
Return what the list showed."
  (let ((text (aside-test--list-text)))
    (with-current-buffer (aside-test--list)
      (when label
        (goto-char (point-min))
        (unless (re-search-forward (format "^ *\\(?:[●○*-] \\)?\\(?:[^\n/]*/\\)?%s\\(?: \\|$\\)"
                                           (regexp-quote label))
                                   nil t)
          (ert-fail (format "No choice called %s in:\n%s" label text)))
        (beginning-of-line))
      (aside-list-choose-at-point))
    text))

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
      (aside-test--should-show "? notes.txt" "+hello from aside"
                               "○ Allow once\n    ○ Always allow\n    ○ Reject")
      ;; The cursor waits on the first option, highlighted.
      (should (equal (plist-get (get-text-property (point) 'aside-option) :name) "Allow once"))
      (should (overlay-buffer aside--option-overlay))
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
      (aside-test--answer "Allow once")
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
      (aside-test--request)
      (aside-test--should-show "? Write notes.txt" "+hello from aside"
                               "○ Yes\n    ○ Yes, allow all edits during this session\n    ○ No")
      (aside-test--answer "Yes")
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
          (progn
            (let ((default-directory (file-name-as-directory dir))
                  (aside-default-agent agent))
              (aside-resume))
            ;; The newest session is under the cursor.
            (should (string-search "Resume · 1 session" (aside-test--choose)))
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

;;;; Choosing

(ert-deftest aside-list-takes-the-window-and-gives-it-back ()
  "A list shows in the popup's window, starts on the current choice, and
puts the popup back as it was whether you choose or cancel."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (insert "half a prompt")
      (let ((popup (current-buffer))
            (window (get-buffer-window (current-buffer)))
            chosen)
        (aside--choose "Mode" '(("Build" build) ("Plan" plan "Read only"))
                       :current 'build :then (lambda (value) (setq chosen value)))
        (let ((list (aside-test--list)))
          (should (eq (window-buffer window) list))
          (with-current-buffer list
            (should (looking-at-p "Build"))
            (should (string-search "● Build" (buffer-string)))
            (should (string-search "○ Plan" (buffer-string)))
            (forward-line 1)
            (aside-list-choose-at-point))
          (should-not (buffer-live-p list)))
        (should (eq chosen 'plan))
        (should (eq (window-buffer window) popup))
        (should (equal (aside--prompt-text) "half a prompt"))
        (setq chosen nil)
        (aside--choose "Mode" '(("Build" build) ("Plan" plan))
                       :then (lambda (value) (setq chosen value))
                       :cancel (lambda () (setq chosen 'cancelled)))
        (with-current-buffer (aside-test--list) (aside-list-cancel))
        (should (eq chosen 'cancelled))
        (should (eq (window-buffer window) popup))))))

(ert-deftest aside-offers-only-installed-agents ()
  "The agent list offers agents that can run; one is used without asking."
  (let ((aside-agents '((here :name "Here" :command ("sh"))
                        (there :name "There" :command ("true"))
                        (gone :name "Gone" :command ("aside-no-such-program")
                              :install "npm install -g gone")))
        (aside--last-agent nil)
        offered note chosen)
    (cl-letf (((symbol-function 'aside--choose)
               (lambda (_title choices &rest args)
                 (setq offered (mapcar #'car choices) note (plist-get args :note))
                 (funcall (plist-get args :then) (cadr (car choices))))))
      (aside--read-agent "Agent" nil (lambda (agent) (setq chosen agent)))
      (should (eq chosen 'here))
      (should (equal offered '("Here" "There")))
      (should (equal note "Not installed: Gone")))
    (setq aside-agents '((here :name "Here" :command ("sh"))
                         (gone :name "Gone" :command ("aside-no-such-program")))
          chosen nil)
    (aside--read-agent "Agent" nil (lambda (agent) (setq chosen agent)))
    (should (eq chosen 'here))
    (setq aside-agents '((gone :name "Gone" :command ("aside-no-such-program")
                               :install "npm install -g gone")))
    (should-error (aside--read-agent "Agent" nil #'ignore) :type 'user-error)))

(defun aside-test--watch-options ()
  "Return a function that returns the option changes sent so far, newest first."
  (let ((sent nil))
    (add-hook 'aside-acp-trace-functions
              (lambda (_ direction line)
                (when (and (eq direction 'out) (string-search "set_config_option" line))
                  (push line sent))))
    (lambda () sent)))

(ert-deftest aside-option-list-sets-the-chosen-value ()
  "C-c C-o lists the session's options; choosing Session Mode then Plan sets it."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (let ((sent (aside-test--watch-options)))
        (unwind-protect
            (progn
              (aside-set-option)
              ;; OpenCode names its values in lower case; lists capitalize them.
              (should (string-search "Session Mode" (aside-test--choose "Session Mode")))
              (let ((shown (aside-test--choose "Plan")))
                (should (string-search "● Build" shown))
                (should (string-search "○ Plan" shown))))
          (setq aside-acp-trace-functions nil))
        (should (string-search "\"configId\":\"mode\"" (car (funcall sent))))
        (should (string-search "\"value\":\"plan\"" (car (funcall sent))))))))

(ert-deftest aside-effort-is-a-key-away ()
  "C-c C-e chooses the reasoning effort, or says why there is none."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (let ((err (should-error (aside-select-effort) :type 'user-error)))
        (should (string-search "OpenCode offers no reasoning effort with Big Pickle"
                               (cadr err)))
        (should (string-search "C-c C-m" (cadr err))))))
  (aside-test--with-agent (claude "claude-session")
    (with-current-buffer (aside-test--open dir)
      (should (string-search "Xhigh effort" (aside--mode-line)))
      (let ((sent (aside-test--watch-options)))
        (unwind-protect
            (progn
              (aside-select-effort)
              (should (string-search "Effort" (aside-test--choose "Low"))))
          (setq aside-acp-trace-functions nil))
        (should (string-search "\"configId\":\"effort\"" (car (funcall sent))))
        (should (string-search "\"value\":\"low\"" (car (funcall sent))))))))

(ert-deftest aside-mode-line-parts-can-be-clicked ()
  "The model, effort and mode in the mode line open their choices."
  (aside-test--with-agent (claude "claude-session")
    (with-current-buffer (aside-test--open dir)
      (let ((line (aside--mode-line))
            (event (list 'mouse-1 (posn-at-point (point-min) (get-buffer-window)))))
        (dolist (pair '(("Claude Code" . aside-switch-agent)
                        ("Opus 5.5" . aside-select-model)
                        ("Xhigh effort" . aside-select-effort)
                        ("Manual" . aside-set-option)))
          (let ((map (get-text-property (string-search (car pair) line) 'local-map line))
                ran)
            (cl-letf (((symbol-function (cdr pair))
                       (lambda () (interactive) (setq ran (current-buffer)))))
              ;; Clicked from another window, it still acts on the popup.
              (with-temp-buffer
                (funcall (lookup-key map [mode-line mouse-1]) event)))
            (should (eq ran (current-buffer)))))))))

(ert-deftest aside-empty-prompt-shows-the-keys ()
  "The hint in an empty prompt names the keys that change the session."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (let ((hint (substring-no-properties (overlay-get aside--placeholder 'after-string))))
        (dolist (text '("C-c C-a agent" "C-c C-m model" "C-c C-e effort" "Ask OpenCode"))
          (should (string-search text hint)))))))

;;;; Agents

(defun aside-test--fake (agent transcript dir)
  "Return an `aside-agents' entry for AGENT replaying TRANSCRIPT in DIR."
  (cons agent (list :name (plist-get (alist-get agent aside-agents) :name)
                    :command (list (expand-file-name invocation-name invocation-directory)
                                   "--batch" "-Q" "-l"
                                   (expand-file-name "aside-fake-agent.el" aside-test--dir)
                                   "-f" "aside-fake-agent" (aside-test--transcript transcript)
                                   dir))))

(ert-deftest aside-switches-agent-in-the-popup ()
  "C-c C-a starts the popup over with another agent, and says so in the mode line."
  (let* ((dir (aside-test--project))
         (aside-agents (list (aside-test--fake 'opencode "opencode-session" dir)
                             (aside-test--fake 'claude "claude-session" dir)))
         (aside-default-agent 'opencode)
         (aside--connections nil)
         (aside--last-agent nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer (aside-test--open dir)
          (aside-switch-agent)
          (should (string-search "● OpenCode" (aside-test--choose "Claude Code")))
          (aside-test--wait (lambda () (eq aside--state 'ready)) "Claude's session")
          (should (eq aside--agent 'claude))
          (should (string-search "Claude Code" (buffer-name)))
          (should (string-match-p "\\` Claude Code" (substring-no-properties (aside--mode-line))))
          (should (string-search "Ask Claude Code" (aside-test--hint))))
      (mapc #'kill-buffer (aside--popups))
      (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
      (delete-directory dir t))))

(ert-deftest aside-says-when-it-reuses-the-last-agent ()
  "Opening a popup with the agent used last says so, and how to switch."
  (let* ((one (aside-test--project))
         (two (aside-test--project))
         (aside-agents (list (aside-test--fake 'opencode "opencode-session" one)
                             (aside-test--fake 'claude "claude-session" one)))
         (aside-default-agent nil)
         (aside--connections nil)
         (aside--last-agent nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (let ((default-directory (file-name-as-directory one))) (aside))
          ;; Nothing used before, so it asks, in the popup.
          (aside-test--choose "OpenCode")
          (should (eq aside--last-agent 'opencode))
          (let ((default-directory (file-name-as-directory two))) (aside))
          (should (string-search "OpenCode, as last time; C-c C-a switches agent"
                                 (aside-test--last-message))))
      (mapc #'kill-buffer (aside--popups))
      (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
      (delete-directory one t)
      (delete-directory two t))))

;;;; Resuming

(ert-deftest aside-resume-loads-into-the-popup-in-use ()
  "Resuming from a popup loads the session there; no second popup opens."
  (let* ((dir (aside-test--project))
         (aside-agents (list (aside-test--fake 'claude "claude-load" dir)))
         (aside-default-agent 'claude)
         (aside-notify nil)
         (aside--connections nil)
         (aside--last-agent nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (let ((popup (let ((default-directory (file-name-as-directory dir)))
                       (aside)
                       (current-buffer))))
          ;; The recording has no new session to open; this popup fails
          ;; to start one, which is beside the point here.
          (aside-test--wait (lambda () (memq aside--state '(failed ready))) "the popup")
          (with-current-buffer popup (aside-resume))
          (aside-test--choose)
          (aside-test--wait (lambda () (with-current-buffer popup (eq aside--state 'ready)))
                            "the session to load")
          (should (equal (aside--popups) (list popup)))
          (with-current-buffer popup
            (aside-test--should-show (nth 3 aside-test--prompts))
            (should (equal (aside--prompt-text) ""))))
      (dolist (buffer (aside--popups)) (kill-buffer buffer))
      (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
      (delete-directory dir t))))

(ert-deftest aside-resume-leaves-no-popup-when-it-cant ()
  "A popup opened only to list sessions goes away when there are none to
list: the agent can't list them, or none were made in this project."
  (let* ((dir (aside-test--project))
         (elsewhere (aside-test--project))
         (aside--connections nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (pcase-dolist (`(,agent ,transcript ,complaint)
                       '((cline "cline-error" "list its sessions")
                         (claude "claude-load" "No earlier Claude Code sessions")))
          (let ((aside-agents (list (aside-test--fake agent transcript elsewhere)))
                (aside-default-agent agent))
            (with-temp-buffer
              (setq default-directory (file-name-as-directory dir))
              (should (string-search complaint (cadr (should-error (aside-resume))))))
            (should-not (aside--popups))))
      (dolist (buffer (aside--popups)) (kill-buffer buffer))
      (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
      (delete-directory dir t)
      (delete-directory elsewhere t))))

;;;; Hints

(defun aside-test--hint ()
  "Return the hint in the empty prompt, without properties."
  (substring-no-properties (overlay-get aside--placeholder 'after-string)))

(defun aside-test--last-message ()
  "Return the last line logged in *Messages*."
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (string-trim (buffer-substring-no-properties (line-beginning-position 0) (point-max))))))

(ert-deftest aside-hint-fits-the-moment ()
  "The empty prompt invites a first question, then a follow-up or a new session."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (should (string-search "Ask OpenCode" (aside-test--hint)))
      (should (string-search "C-c ? all keys" (aside-test--hint)))
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (should (string-search "Follow up with OpenCode" (aside-test--hint)))
      (should (string-search "C-c C-n new session" (aside-test--hint))))))

(ert-deftest aside-hint-keeps-the-reason-a-session-failed ()
  "When the agent won't start, the empty prompt says why and how to retry."
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
          (should (string-search "Codex stopped" (aside-test--hint)))
          (should (string-search "Error: no credentials" (aside-test--hint)))
          (should (string-search "C-c C-c retries" (aside-test--hint))))
      (mapc #'kill-buffer (aside--popups))
      (delete-directory dir t))))

(ert-deftest aside-keys-menu-lists-and-runs-the-keys ()
  "C-c ? lists what you can do, with each key, and does what you pick."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (let (shown ran)
        (cl-letf (((symbol-function 'aside-cancel) (lambda () (interactive) (setq ran t))))
          (aside-keys)
          (setq shown (aside-test--choose "Hide the popup")))
        (dolist (text '("Model" "C-c C-m" "Effort" "C-c C-e" "New session" "C-c C-n"
                        "Resume a session" "Hide the popup" "C-c C-k"
                        "Select a region before"))
          (should (string-search text shown)))
        (should ran)))))

(ert-deftest aside-says-how-to-stop-and-that-hidden-work-goes-on ()
  "While working, the mode line says how to stop; hiding says it continues."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--send (nth 1 aside-test--prompts))
      (aside-test--request)
      (aside-test--answer "Allow once")
      (setq aside--queued "next")
      (should (string-search "C-c C-k stops" (substring-no-properties (aside--mode-line-status))))
      (aside-dismiss)
      (should (string-search "OpenCode keeps working" (aside-test--last-message)))
      (setq aside--queued nil)
      (aside-test--finish))))

(ert-deftest aside-summary-links-edited-files ()
  "Files named in the summary open, in the frame the popup came from."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (aside-test--send (nth 1 aside-test--prompts))
      (aside-test--request)
      (aside-test--answer "Allow once")
      (aside-test--finish)
      (goto-char (point-min))
      (search-forward "edited notes.txt")
      (goto-char (match-end 0))
      (backward-char 2)
      (should (equal (get-text-property (point) 'aside-file)
                     (expand-file-name "notes.txt" dir)))
      (should (eq (lookup-key (get-text-property (point) 'keymap) (kbd "RET")) #'aside-visit-file))
      (let ((popup (current-buffer)))
        (aside-visit-file)
        (should (equal buffer-file-name (expand-file-name "notes.txt" dir)))
        (kill-buffer)
        (set-buffer popup)))))

(ert-deftest aside-warns-once-when-the-context-is-nearly-full ()
  "At 80% of the context window, aside says so once and suggests a new session."
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (aside--update '(:sessionUpdate "usage_update" :used 170000 :size 200000))
      (should (string-search "85% of this session" (aside-test--last-message)))
      (should (string-search "C-c C-n starts a new session" (aside-test--last-message)))
      (message "something else")
      (aside--update '(:sessionUpdate "usage_update" :used 180000 :size 200000))
      (should (equal (aside-test--last-message) "something else"))
      (should (eq (get-text-property 0 'face (aside--usage-text)) 'aside-request)))))

(ert-deftest aside-marks-free-models-and-says-what-a-model-means-for-effort ()
  "Free models are marked; after a model change, aside says if it has effort."
  (should (equal (aside--value-note '(:name "Nex N2.5 Pro" :value "nex-agi/nex-n2.5-pro:free"))
                 "free"))
  (should (equal (aside--value-note '(:name "Space Bunny" :value "opencode/space-bunny-free"))
                 "free"))
  (should-not (aside--value-note '(:name "Space Bunny Free" :value "opencode/space-bunny-free")))
  (should-not (aside--value-note '(:name "Big Pickle" :value "opencode/big-pickle")))
  (should (equal (aside--value-note '(:name "Plan" :value "plan" :description "Plans only"))
                 "Plans only"))
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (aside-select-model)
      (should (string-search "Model · 6 models" (aside-test--choose "Big Pickle")))
      (aside-test--wait (lambda () (string-prefix-p "Model:" (aside-test--last-message)))
                        "the confirmation")
      (should (equal (aside-test--last-message) "Model: Big Pickle · no effort setting")))))

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
                         "Codex stopped (exited abnormally with code 1). Send again to restart it.\nError: no credentials"))
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
      (aside-test--answer "Allow once")
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
(declare-function evil-insert-state "evil-states")
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

(defmacro aside-test--with-evil (&rest body)
  "Run BODY with Evil on, if Evil can be loaded; skip the test if not."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'evil nil t))
     (let ((evil-mode-was evil-mode))
       (evil-mode 1)
       (unwind-protect (progn ,@body)
         (unless evil-mode-was (evil-mode -1))))))

(ert-deftest aside-evil-moves-to-a-permission-option ()
  "With Evil, a request puts the cursor on its first option in normal state;
j moves to the next, RET chooses it, and insert state comes back after."
  (aside-test--with-evil
    (aside-test--with-agent (opencode "opencode-session")
      (with-current-buffer (aside-test--open dir)
        (aside-test--send (nth 0 aside-test--prompts))
        (aside-test--finish)
        (evil-insert-state)
        (aside-test--send (nth 1 aside-test--prompts))
        (aside-test--request)
        (should (eq evil-state 'normal))
        (should (equal (plist-get (get-text-property (point) 'aside-option) :name)
                       "Allow once"))
        (execute-kbd-macro "j")
        (should (equal (plist-get (get-text-property (point) 'aside-option) :name)
                       "Always allow"))
        (execute-kbd-macro (kbd "RET"))
        (should-not (aside-turn-requests aside--turn))
        (should (eq evil-state 'insert))
        (aside-test--finish)))))

(ert-deftest aside-evil-moves-in-lists ()
  "With Evil, a list is in motion state: j, k, G and gg move, RET chooses, q cancels."
  (aside-test--with-evil
    (aside-test--with-agent (opencode "opencode-session")
      (with-current-buffer (aside-test--open dir)
        (let (chosen)
          (aside--choose "Pick" '(("One" 1) ("Two" 2) ("Three" 3) ("Four" 4))
                         :then (lambda (value) (setq chosen value)))
          (with-current-buffer (aside-test--list)
            (should (eq evil-state 'motion))
            (execute-kbd-macro "G")
            (should (looking-at-p "Four"))
            (execute-kbd-macro "gg")
            (should (looking-at-p "One"))
            (execute-kbd-macro "jj")
            (should (looking-at-p "Three"))
            (execute-kbd-macro (kbd "RET")))
          (should (eq chosen 3))
          (aside--choose "Pick" '(("One" 1) ("Two" 2))
                         :then (lambda (value) (setq chosen value))
                         :cancel (lambda () (setq chosen 'cancelled)))
          (with-current-buffer (aside-test--list)
            (execute-kbd-macro "q"))
          (should (eq chosen 'cancelled)))))))

(provide 'aside-test)
;;; aside-test.el ends here
