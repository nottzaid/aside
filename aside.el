;;; aside.el --- Ask a coding agent something, in a popup  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;; Author: nottzaid
;; Version: 0.4.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/nottzaid/aside

;; This file is not part of GNU Emacs.

;;; Commentary:

;; aside opens a small popup where you write a prompt for a coding
;; agent and watch it work: OpenCode, Claude Code, Codex, Cline, or any
;; other agent that speaks the Agent Client Protocol.  While the agent
;; works, the popup shows its reasoning, the tools it runs and any
;; permission it asks for; when it finishes, all that gives way to its
;; answer and a line saying what it changed.  Write below the answer to
;; follow up.
;;
;; M-x aside opens the current project's popup, or hides it when it is
;; in front.  M-x aside-resume picks up an earlier session.  In the
;; popup, C-c C-c (or Evil's :w) sends, and C-c C-k stops the agent or
;; puts the popup away.  See the README for the rest.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'aside-acp)
(require 'aside-turn)
(require 'aside-frame)
(require 'aside-list)

(defconst aside-version "0.4.1"
  "The version of aside.")

(defgroup aside nil
  "Ask coding agents things in a popup."
  :group 'tools
  :prefix "aside-")

;;;; Options

(defcustom aside-agents
  '((opencode :name "OpenCode" :command ("opencode" "acp")
              :login "opencode auth login"
              :install "see https://opencode.ai")
    (claude :name "Claude Code" :command ("claude-agent-acp")
            :login "claude, then /login"
            :install "npm install -g @agentclientprotocol/claude-agent-acp")
    (codex :name "Codex" :command ("codex-acp")
           :login "codex login"
           :install "npm install -g @agentclientprotocol/codex-acp")
    (cline :name "Cline" :command ("cline" "--acp")
           :login "cline auth"
           :install "npm install -g cline"))
  "The agents aside can talk to.
Each entry is (AGENT . PROPERTIES): AGENT is a symbol and PROPERTIES a
plist with these keys:

  :name     how popups name the agent
  :command  the program and arguments that start it speaking ACP
  :login    the command that signs in to it, offered when it asks
  :install  how to install it, offered when it is missing

Any program that speaks the Agent Client Protocol can be added."
  :type '(alist :key-type symbol :value-type (plist :value-type sexp)))

(defcustom aside-session-options nil
  "Options to set in every new session, by agent.
Each entry is (AGENT . OPTIONS), where OPTIONS is an alist of option ids
and values, such as ((claude (\"model\" . \"haiku\"))).  In a popup,
\\[aside-set-option] lists the options an agent offers."
  :type '(alist :key-type symbol
                :value-type (alist :key-type string :value-type sexp)))

(defcustom aside-default-agent nil
  "The agent for new popups; nil means the one you used last."
  :type '(choice (const :tag "The last one used" nil) symbol))

(defcustom aside-reveal-on-request t
  "Whether a hidden popup comes back when its agent asks permission."
  :type 'boolean)

(defcustom aside-notify t
  "Whether to send a desktop notification when a hidden popup's agent is done."
  :type 'boolean)

;;;; Faces

(defface aside-mode-line-agent '((t :inherit bold))
  "The agent's name in the mode line."
  :group 'aside)

;;;; Choosing

;; Every question aside asks shows all its answers, in a list that
;; takes the popup's window until you choose; see aside-list.el.

(defun aside--choose (title choices &rest args)
  "Show CHOICES, headed TITLE, in place of this popup until you choose.
ARGS are as for `aside-list-choose'.  The popup comes into view first
if it isn't."
  (unless (eq (window-buffer (selected-window)) (current-buffer))
    (aside--show (current-buffer)))
  (apply #'aside-list-choose title choices args))

;;;; State

(defvar aside--connections nil
  "Alist of (AGENT . CONNECTION) for running agents.")

(defvar aside--waiting nil
  "Alist of (AGENT . CALLBACKS) for agents still starting.
Each callback is a cons of an on-ready and an on-error function.")

(defvar aside--sessions (make-hash-table :test #'equal)
  "Map from session id to the popup buffer showing it.")

(defvar aside--last-agent nil
  "The agent used most recently.")

(defvar aside--preferences nil
  "Alist of (AGENT . OPTIONS): option values you chose, reused for new sessions.")

(defvar aside--ticker nil
  "Timer animating busy popups.")

(defvar-local aside--agent nil "The agent behind this popup.")
(defvar-local aside--conn nil "The connection to this popup's agent.")
(defvar-local aside--session nil "This popup's session id.")
(defvar-local aside--root nil "The project directory this popup works in.")
(defvar-local aside--origin-frame nil "The frame the popup was last opened from.")
(defvar-local aside--state nil
  "Where this popup's session stands.
One of `starting', `loading', `reviving', `ready' or `failed'.")
(defvar-local aside--problem nil "Why the session failed, as text.")
(defvar-local aside--options nil "The session's configuration options.")
(defvar-local aside--usage nil "The latest usage report from the agent.")
(defvar-local aside--context-warned nil "Non-nil once you were told the context is nearly full.")
(defvar-local aside--turn nil "The current or most recent turn.")
(defvar-local aside--queued nil "Prompt text waiting for the session to be ready.")
(defvar-local aside--history nil "Turns replayed while loading a session, newest first.")
(defvar-local aside--context nil "Regions to send with the next prompt.")
(defvar-local aside--prompt-start nil "Where the prompt being written begins.")
(defvar-local aside--live-start nil "Where the agent's reply begins during a turn.")
(defvar-local aside--status-block nil "The placeholder shown before the agent replies.")
(defvar-local aside--prompt-overlay nil "Overlay marking the prompt being written.")
(defvar-local aside--placeholder nil "Overlay showing the hint in an empty prompt.")

;;;; Agents and connections

(defun aside--spec (agent)
  "Return the properties of AGENT from `aside-agents'."
  (or (alist-get agent aside-agents)
      (user-error "No agent called `%s' in `aside-agents'" agent)))

(defun aside--agent-name (agent)
  "Return AGENT's display name."
  (or (plist-get (alist-get agent aside-agents) :name)
      (capitalize (symbol-name agent))))

(defun aside--installed-p (agent)
  "Return non-nil if AGENT's program can be found."
  (executable-find (car (plist-get (aside--spec agent) :command))))

(defun aside--installed-agents ()
  "Return the agents whose programs can be found; signal if none can."
  (or (cl-remove-if-not #'aside--installed-p (mapcar #'car aside-agents))
      (user-error "No agent found on `exec-path'.  Install one: %s"
                  (mapconcat (lambda (agent)
                               (format "%s (%s)" (aside--agent-name agent)
                                       (plist-get (aside--spec agent) :install)))
                             (mapcar #'car aside-agents) "; "))))

(defun aside--read-agent (title current then &optional cancel)
  "Ask which agent to use, in a list headed TITLE, and call THEN with it.
CURRENT, or else the agent used last, is marked.  Only agents whose
programs can be found are offered; when just one can, THEN gets it
without asking.  CANCEL is called if you choose none."
  (let* ((installed (aside--installed-agents))
         (missing (cl-remove-if (lambda (agent) (memq agent installed))
                                (mapcar #'car aside-agents))))
    (if (null (cdr installed))
        (funcall then (car installed))
      (aside--choose title
                     (mapcar (lambda (agent) (list (aside--agent-name agent) agent))
                             installed)
                     :current (or current aside--last-agent)
                     :note (and missing (format "Not installed: %s"
                                                (mapconcat #'aside--agent-name missing ", ")))
                     :then then
                     :cancel cancel))))

(defun aside--known-agent ()
  "Return the agent new popups use without asking, or nil to ask."
  (or aside-default-agent aside--last-agent))

(defun aside--client-info ()
  "Return what aside tells agents about itself."
  (list :name "aside" :title "aside" :version aside-version))

(defun aside--connect (agent on-ready on-error)
  "Call ON-READY with a ready connection to AGENT, starting it if needed.
ON-ERROR is called with a description if that fails."
  (let ((conn (alist-get agent aside--connections)))
    (cond
     ((and (aside-acp-live-p conn) (aside-acp-ready conn))
      (funcall on-ready conn))
     ((aside-acp-live-p conn)
      (push (cons on-ready on-error) (alist-get agent aside--waiting)))
     ((not (aside--installed-p agent))
      (funcall on-error
               (format-message
                (concat "Can't find `%s' on `exec-path'. Install %s (%s), "
                        "or add its directory to `exec-path'")
                (car (plist-get (aside--spec agent) :command)) (aside--agent-name agent)
                (plist-get (aside--spec agent) :install))))
     (t
      (setf (alist-get agent aside--waiting) (list (cons on-ready on-error)))
      (condition-case err
          (setf (alist-get agent aside--connections)
                (aside-acp-start
                 :name (symbol-name agent)
                 :command (plist-get (aside--spec agent) :command)
                 :client-info (aside--client-info)
                 :client-capabilities '(:fs (:readTextFile t :writeTextFile t)
                                        :terminal :false)
                 :on-request #'aside--on-request
                 :on-notification #'aside--on-notification
                 :on-exit (lambda (conn why) (aside--on-exit agent conn why))
                 :on-ready (lambda (conn) (aside--release agent conn nil))
                 :on-error (lambda (err)
                             (aside--release agent nil (aside-acp-error-text err)))))
        (error (aside--release agent nil (error-message-string err))))))))

(defun aside--release (agent conn problem)
  "Hand CONN, or PROBLEM, to everything waiting for AGENT."
  (let ((callbacks (reverse (alist-get agent aside--waiting))))
    (setf (alist-get agent aside--waiting nil t) nil)
    (dolist (callback callbacks)
      (if conn
          (funcall (car callback) conn)
        (funcall (cdr callback) problem)))))

(defun aside--agent-of (conn)
  "Return the agent CONN talks to."
  (car (rassq conn aside--connections)))

(defun aside--on-exit (agent conn why)
  "Clean up after AGENT's process CONN ended, as WHY says."
  (when (eq (alist-get agent aside--connections) conn)
    (setf (alist-get agent aside--connections nil t) nil))
  (let* ((detail (aside-acp-stderr-tail conn 3))
         (problem (concat (format "%s stopped (%s). Send again to restart it."
                                  (aside--agent-name agent) why)
                          (if (and detail (not (string-empty-p detail)))
                              (concat "\n" detail) ""))))
    (aside--release agent nil problem)
    (dolist (buffer (aside--popups))
      (with-current-buffer buffer
        (when (eq aside--conn conn)
          (setq aside--conn nil)
          (when (aside-turn-running-p aside--turn)
            (aside--finish-turn nil problem)))))))

(defun aside--capability (conn &rest path)
  "Return CONN's agent capability at PATH, or nil if it lacks it.
ACP announces many capabilities as empty objects, which count."
  (let ((value (aside-acp-agent-capabilities conn))
        (present t))
    (dolist (key path)
      (setq present (and present (plist-member value key))
            value (plist-get value key)))
    (and present (not (eq value :false)) (or value t))))

;;;; Popups

(defun aside--popups ()
  "Return all live popup buffers, most recently used first."
  (cl-remove-if-not (lambda (buffer)
                      (eq (buffer-local-value 'major-mode buffer) 'aside-mode))
                    (buffer-list)))

(defun aside--project-root ()
  "Return the directory a popup opened from here should work in."
  (directory-file-name
   (expand-file-name (if-let* ((project (project-current)))
                         (project-root project)
                       default-directory))))

(defun aside--project-popup (root)
  "Return the most recently used popup working in ROOT."
  (cl-find root (aside--popups) :key (lambda (b) (buffer-local-value 'aside--root b))
           :test #'equal))

(defun aside--frame-title ()
  "Return the title for this popup's frame."
  (format "%s · %s" aside-frame-title (file-name-nondirectory aside--root)))

(defun aside--create (agent root)
  "Return a new popup buffer for AGENT working in ROOT."
  (let ((buffer (generate-new-buffer
                 (format "*aside: %s (%s)*" (file-name-nondirectory root)
                         (aside--agent-name agent)))))
    (with-current-buffer buffer
      (aside-mode)
      (setq aside--agent agent
            aside--root root
            default-directory (file-name-as-directory root))
      (aside--compose))
    buffer))

(defun aside--show (buffer)
  "Show BUFFER's popup, select it and make it current."
  (aside-frame-show buffer (with-current-buffer buffer (aside--frame-title)))
  (set-buffer buffer)
  (goto-char (point-max)))

;;;; The prompt

(defun aside--key-hints (pairs)
  "Return PAIRS of (KEY . WHAT) as a line of key hints."
  (mapconcat (lambda (pair)
               (concat (propertize (car pair) 'face 'aside-key)
                       (propertize (concat " " (cdr pair)) 'face 'aside-placeholder)))
             pairs
             (propertize (concat " " (aside-turn-glyph 'dot) " ") 'face 'aside-placeholder)))

(defun aside--placeholder-text ()
  "Return the hint shown in an empty prompt.
Before the first prompt it says how to ask and which keys change the
session; after an answer, how to follow up or start over.  When the
session couldn't be opened, it says why and how to retry."
  (let* ((evil (bound-and-true-p evil-local-mode))
         (send (if evil ":w" "C-c C-c"))
         (dot (concat " " (aside-turn-glyph 'dot) " "))
         (name (aside--agent-name aside--agent))
         (failed (and (eq aside--state 'failed) aside--problem (null aside--turn)))
         (answered (and aside--turn (aside-turn-finished aside--turn))))
    (concat
     (if failed
         ;; Each line of the reason beside its own bar, which keeps its
         ;; face; long lines wrap under the text, not under the bar.
         (let ((lines (split-string (concat (aside-turn-glyph 'failed) " " aside--problem)
                                    "\n")))
           (concat (propertize (car lines) 'face 'aside-failed 'cursor t
                               'wrap-prefix (aside--prompt-bar))
                   (mapconcat (lambda (line)
                                (concat "\n" (aside--prompt-bar)
                                        (propertize line 'face 'aside-failed
                                                    'wrap-prefix (aside--prompt-bar))))
                              (cdr lines) "")))
       (concat (propertize (format (if answered "Follow up with %s" "Ask %s") name)
                           'face 'aside-placeholder 'cursor t)
               (propertize (format "   %s sends%s%s hides" send dot (if evil ":q" "C-c C-k"))
                           'face 'aside-placeholder)))
     "\n" (aside--prompt-bar)
     (aside--key-hints
      (cond (failed `((,send . "retries") ("C-c ?" . "all keys")))
            (answered '(("C-c C-n" . "new session") ("C-c C-a" . "agent")
                        ("C-c C-m" . "model") ("C-c ?" . "all keys")))
            (t '(("C-c C-a" . "agent") ("C-c C-m" . "model") ("C-c C-e" . "effort")
                 ("C-c ?" . "all keys"))))))))

(when (fboundp 'define-fringe-bitmap)
  ;; One row, repeated to fill each line's whole height, and exactly as
  ;; wide as the bar, so no pixel shows the theme's fringe colour.
  (define-fringe-bitmap 'aside-bar [#b111] nil 3 '(center t)))

(defun aside--prompt-bar ()
  "Return the bar drawn beside your prompt.
In a popup frame the bar is drawn in the fringe, which fills each line
to its full height whatever is on it, so the lines of a prompt join into
one unbroken bar.  Elsewhere it is a thin stretch of colour, or a
box-drawing character on a text terminal."
  (if (aside-frame-has-fringe-p)
      (concat (propertize " " 'display '(left-fringe aside-bar aside-prompt-bar-fringe))
              (propertize " " 'display '(space :width 1)))
    (aside-turn-title-bar)))

(defun aside--compose ()
  "Set up an empty prompt at the end of the buffer."
  (let ((end (point-max)))
    (setq aside--prompt-start (copy-marker end))
    (if (overlayp aside--prompt-overlay)
        (move-overlay aside--prompt-overlay end end)
      (setq aside--prompt-overlay (make-overlay end end nil nil t)))
    (overlay-put aside--prompt-overlay 'line-prefix (aside--prompt-bar))
    (overlay-put aside--prompt-overlay 'wrap-prefix (aside--prompt-bar))
    (aside--update-placeholder)))

(defun aside--prompt-text ()
  "Return the prompt being written, trimmed."
  (string-trim (buffer-substring-no-properties aside--prompt-start (point-max))))

(defun aside--update-placeholder (&rest _)
  "Show the hint when the prompt is empty, and hide it otherwise."
  (when (markerp aside--prompt-start)
    (let ((empty (and (= aside--prompt-start (point-max))
                      (not (aside-turn-running-p aside--turn)))))
      (cond
       ((and empty (not (overlayp aside--placeholder)))
        (setq aside--placeholder (make-overlay (point-max) (point-max)))
        (overlay-put aside--placeholder 'before-string (aside--prompt-bar))
        (overlay-put aside--placeholder 'after-string (aside--placeholder-text)))
       ((and empty (overlayp aside--placeholder))
        (move-overlay aside--placeholder (point-max) (point-max))
        (overlay-put aside--placeholder 'before-string (aside--prompt-bar))
        (overlay-put aside--placeholder 'after-string (aside--placeholder-text)))
       ((and (not empty) (overlayp aside--placeholder))
        (delete-overlay aside--placeholder)
        (setq aside--placeholder nil))))))

;;;; Context

(defun aside--region-context ()
  "Return the active region as context to send, and deactivate it."
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (last (if (and (> end start) (eq (char-before end) ?\n)) (1- end) end)))
      (prog1 (list :path (and buffer-file-name (expand-file-name buffer-file-name))
                   :name (buffer-name)
                   :from (line-number-at-pos start t)
                   :to (line-number-at-pos last t)
                   :text (buffer-substring-no-properties start end))
        (deactivate-mark)))))

(defun aside--context-label (context)
  "Return a short name for CONTEXT."
  (let ((name (if-let* ((path (plist-get context :path)))
                  (file-relative-name path aside--root)
                (plist-get context :name)))
        (from (plist-get context :from))
        (to (plist-get context :to)))
    (if (= from to) (format "%s:%d" name from) (format "%s:%d-%d" name from to))))

(defun aside--update-header ()
  "Show the context waiting to be sent, if any, in the header line."
  (setq header-line-format
        (when aside--context
          (concat " " (propertize "with" 'face 'aside-summary) " "
                  (mapconcat (lambda (c) (propertize (aside--context-label c) 'face 'aside-code))
                             (reverse aside--context) ", ")
                  "   " (propertize "C-c C-x drops it" 'face 'aside-summary)))))

(defun aside--add-context (context)
  "Attach CONTEXT to the next prompt."
  (push context aside--context)
  (aside--update-header))

(defun aside-clear-context ()
  "Stop sending the attached regions with the next prompt."
  (interactive nil aside-mode)
  (setq aside--context nil)
  (aside--update-header)
  (message "Context dropped"))

(defun aside--prompt-blocks (text)
  "Return the ACP prompt for TEXT and the attached context."
  (let ((embedded (eq t (aside--capability aside--conn :promptCapabilities :embeddedContext))))
    (vconcat
     (list (list :type "text" :text text))
     (mapcar
      (lambda (context)
        (let ((uri (if-let* ((path (plist-get context :path)))
                       (format "file://%s#L%d-%d" path (plist-get context :from)
                               (plist-get context :to))
                     (format "untitled:%s" (plist-get context :name)))))
          (if embedded
              (list :type "resource"
                    :resource (list :uri uri :mimeType "text/plain"
                                    :text (plist-get context :text)))
            (list :type "text"
                  :text (format "%s:\n```\n%s\n```" (aside--context-label context)
                                (string-trim-right (plist-get context :text)))))))
      (reverse aside--context)))))

;;;; Sessions

(defun aside--open-session (&optional session-id)
  "Connect this popup to its agent and open a session.
Create one, or resume SESSION-ID quietly when given."
  (let ((buffer (current-buffer)))
    (setq aside--state (if session-id 'reviving 'starting)
          aside--problem nil
          aside--last-agent aside--agent)
    (aside--update-placeholder)
    (aside--tick-soon)
    (aside--connect
     aside--agent
     (lambda (conn)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq aside--conn conn)
           (if session-id
               (aside--revive conn session-id)
             (aside-acp-request conn "session/new"
                                (list :cwd aside--root :mcpServers [])
                                (lambda (result) (aside--opened buffer result))
                                (lambda (err) (aside--failed buffer (aside-acp-error-text err))))))))
     (lambda (problem) (aside--failed buffer problem)))))

(defun aside--revive (conn session-id)
  "Pick SESSION-ID up again on CONN without replaying it."
  (let ((buffer (current-buffer))
        (params (list :sessionId session-id :cwd aside--root :mcpServers [])))
    (puthash session-id buffer aside--sessions)
    (cond
     ((aside--capability conn :sessionCapabilities :resume)
      (aside-acp-request conn "session/resume" params
                         (lambda (result) (aside--opened buffer result session-id))
                         (lambda (err) (aside--failed buffer (aside-acp-error-text err)))))
     ((eq t (aside--capability conn :loadSession))
      (aside-acp-request conn "session/load" params
                         (lambda (result) (aside--opened buffer result session-id))
                         (lambda (err) (aside--failed buffer (aside-acp-error-text err)))))
     (t (setq aside--session nil)
        (aside-acp-request conn "session/new" (list :cwd aside--root :mcpServers [])
                           (lambda (result) (aside--opened buffer result))
                           (lambda (err) (aside--failed buffer (aside-acp-error-text err))))))))

(defun aside--opened (buffer result &optional session-id)
  "Record the session described by RESULT in BUFFER, then apply preferences.
SESSION-ID names a session that was resumed rather than created."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((id (or session-id (plist-get result :sessionId))))
        (setq aside--session id)
        (puthash id buffer aside--sessions)
        (when (plist-member result :configOptions)
          (setq aside--options (plist-get result :configOptions)))
        (aside--apply-preferences
         (if session-id nil (aside--wanted-options))
         (lambda ()
           (setq aside--state 'ready)
           (aside--update-placeholder)
           (force-mode-line-update)
           (aside--flush)))))))

(defun aside--wanted-options ()
  "Return the options to set in a new session, as (ID . VALUE) pairs."
  (let ((wanted (copy-alist (alist-get aside--agent aside-session-options))))
    (pcase-dolist (`(,id . ,value) (alist-get aside--agent aside--preferences))
      (setf (alist-get id wanted nil nil #'equal) value))
    wanted))

(defun aside--apply-preferences (wanted then)
  "Set each of WANTED that the session offers and lacks, then call THEN.
Options can depend on each other, so they are set one at a time."
  (let* ((buffer (current-buffer))
         (pair (cl-find-if (lambda (pair)
                             (let ((option (aside--option (car pair))))
                               (and option
                                    (not (equal (plist-get option :currentValue) (cdr pair))))))
                           wanted)))
    (if (null pair)
        (funcall then)
      (aside--set-option-value
       (car pair) (cdr pair)
       (lambda ()
         (with-current-buffer buffer
           (aside--apply-preferences (remove pair wanted) then)))))))

(defun aside--failed (buffer problem)
  "Note in BUFFER that its session couldn't be opened, because of PROBLEM."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq aside--state 'failed
            aside--problem (aside--explain problem))
      (if (or aside--queued (aside-turn-running-p aside--turn))
          (aside--finish-turn nil aside--problem)
        (message "aside: %s" aside--problem))
      (setq aside--queued nil)
      (aside--update-placeholder)
      (force-mode-line-update))))

(defun aside--explain (problem)
  "Return PROBLEM with advice on signing in, when that is what it's about."
  (let ((login (plist-get (aside--spec aside--agent) :login)))
    (if (and login (string-match-p "auth\\|log ?in\\|sign ?in\\|credential" problem))
        (format-message "%s. Sign in with `%s' in a terminal, then send again."
                        (string-trim-right problem "[.[:space:]]+") login)
      problem)))

(defun aside--option (id)
  "Return the session option ID, or the first option in category ID."
  (or (cl-find id aside--options :key (lambda (o) (plist-get o :id)) :test #'equal)
      (cl-find id aside--options :key (lambda (o) (plist-get o :category)) :test #'equal)))

(defun aside--capitalized (name)
  "Return NAME starting with a capital letter."
  (if (string-match-p "\\`[[:lower:]]" name)
      (concat (upcase (substring name 0 1)) (substring name 1))
    name))

(defun aside--tidy-name (name)
  "Return NAME without a provider prefix, starting with a capital.
OpenCode, for one, calls its models \"OpenCode Zen/Big Pickle\" and
its modes \"build\"."
  (aside--capitalized (car (last (split-string name "/" t " ")))))

(defun aside--option-label (id)
  "Return a short name for the current value of the option ID.
On-off options are named only when they are on."
  (when-let* ((option (aside--option id)))
    (let ((value (plist-get option :currentValue)))
      (cond ((eq value t) (concat (plist-get option :name) " on"))
            ((memq value '(nil :false)) nil)
            (t (aside--value-name option))))))

(defun aside--value-name (option)
  "Return a short name for OPTION's current value."
  (let* ((value (plist-get option :currentValue))
         (choice (cl-find value (plist-get option :options)
                          :key (lambda (o) (plist-get o :value)) :test #'equal)))
    (cond (choice (aside--tidy-name (plist-get choice :name)))
          ((eq value t) "On")
          ((memq value '(nil :false)) "Off")
          (t (aside--tidy-name (format "%s" value))))))

(defun aside--set-option-value (id value &optional then)
  "Set the session option ID to VALUE, then call THEN."
  (let ((buffer (current-buffer)))
    (aside-acp-request
     aside--conn "session/set_config_option"
     (list :sessionId aside--session :configId id :value value)
     (lambda (result)
       (with-current-buffer buffer
         (when (plist-member result :configOptions)
           (setq aside--options (plist-get result :configOptions)))
         (force-mode-line-update)
         (when then (funcall then))))
     (lambda (err)
       (message "aside: couldn't set %s: %s" id (aside-acp-error-text err))
       (when then (with-current-buffer buffer (funcall then)))))))

;;;; Sending

(defun aside--busy-p ()
  "Return non-nil while this popup's agent is working."
  (or (aside-turn-running-p aside--turn) aside--queued))

(defun aside-send ()
  "Send the prompt you wrote to the agent."
  (interactive nil aside-mode)
  (when (aside--busy-p)
    (user-error "%s is still working; %s stops it" (aside--agent-name aside--agent)
                (substitute-command-keys "\\[aside-cancel]")))
  (let ((text (aside--prompt-text)))
    (when (string-empty-p text)
      (user-error "Write something to send first"))
    (setq aside--queued text)
    (aside--begin-turn text)
    (if (and (eq aside--state 'ready) (aside-acp-live-p aside--conn))
        (aside--flush)
      (unless (memq aside--state '(starting reviving))
        (aside--open-session aside--session)))))

(defun aside--flush ()
  "Send the queued prompt, if there is one."
  (when-let* ((text aside--queued))
    (let ((buffer (current-buffer))
          (blocks (aside--prompt-blocks text)))
      (setq aside--queued nil
            aside--context nil)
      (aside--update-header)
      (aside--status (format "Waiting for %s" (aside--agent-name aside--agent)))
      (aside-acp-request
       aside--conn "session/prompt"
       (list :sessionId aside--session :prompt blocks)
       (lambda (result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (aside--finish-turn (plist-get result :stopReason)))))
       (lambda (err)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (aside-turn-running-p aside--turn)
               (aside--finish-turn nil (aside--explain (aside-acp-error-text err)))))))))))

(defun aside--label ()
  "Return the agent and model, for the divider."
  (string-join (delq nil (list (aside--agent-name aside--agent) (aside--option-label "model")))
               " · "))

(defun aside--begin-turn (text)
  "Clear the popup down to the prompt TEXT and open the agent's reply."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when (overlayp aside--placeholder)
      (delete-overlay aside--placeholder)
      (setq aside--placeholder nil))
    (delete-overlay aside--prompt-overlay)
    (insert (propertize text 'line-prefix (aside--prompt-bar) 'wrap-prefix (aside--prompt-bar))
            "\n\n" (aside-turn-divider (aside--label)))
    (setq aside--live-start (point-marker)
          aside--turn (aside-turn-create :prompt text)
          aside--status-block nil
          buffer-read-only t)
    (aside--status (if (eq aside--state 'ready)
                       (format "Waiting for %s" (aside--agent-name aside--agent))
                     (format "Starting %s" (aside--agent-name aside--agent))))
    (goto-char (point-max))
    (aside--tick-soon)))

(defun aside--status (text)
  "Show TEXT with a spinner until the agent replies."
  (when (aside-turn-running-p aside--turn)
    (if aside--status-block
        (setf (aside-turn-block-title aside--status-block) text)
      (setq aside--status-block (aside-turn-block-create :kind 'status :title text)))
    (when (or (aside-turn-block-marker aside--status-block)
              (null (aside-turn-blocks aside--turn)))
      (aside--draw-status))))

(defun aside--draw-status ()
  "Draw the waiting line at the end of the reply."
  (let ((inhibit-read-only t)
        (block aside--status-block)
        (text (concat "  " (propertize (aside-turn-spinner) 'face 'aside-running) " "
                      (propertize (aside-turn-block-title aside--status-block)
                                  'face 'aside-summary)
                      "\n")))
    (save-excursion
      (if-let* ((start (aside-turn-block-marker block)))
          (progn (delete-region start (point-max)) (goto-char start))
        (goto-char (point-max))
        (setf (aside-turn-block-marker block) (point-marker)))
      (insert text))))

(defun aside--clear-status ()
  "Remove the waiting line once the agent has replied."
  (when-let* ((block aside--status-block)
              (start (aside-turn-block-marker block)))
    (let ((inhibit-read-only t))
      (delete-region start (point-max)))
    (set-marker start nil))
  (setq aside--status-block nil))

;;;; Drawing the reply

(defun aside--following-windows ()
  "Return the windows showing this popup with point at its end."
  (cl-remove-if-not (lambda (window) (>= (window-point window) (point-max)))
                    (get-buffer-window-list nil nil t)))

(defun aside--follow (windows)
  "Keep WINDOWS at the end of the popup."
  (dolist (window windows)
    (set-window-point window (point-max)))
  (when (memq (selected-window) windows)
    (goto-char (point-max))))

(defun aside--width ()
  "Return the width available for one line of the reply."
  (if-let* ((window (get-buffer-window nil t)))
      (window-max-chars-per-line window)
    70))

(defun aside--next-marker (block)
  "Return where the block after BLOCK begins, if it has been drawn."
  (cl-loop for next in (cdr (memq block (aside-turn-blocks aside--turn)))
           thereis (aside-turn-block-marker next)))

(defun aside--draw (block &optional chunk)
  "Draw BLOCK, or just add CHUNK to the end of it when that's enough."
  (let ((inhibit-read-only t)
        (windows (aside--following-windows)))
    (aside--clear-status)
    (save-excursion
      (if (and chunk (aside-turn-block-marker block)
               (eq (aside-turn-block-kind block) 'message)
               (null (aside--next-marker block)))
          (progn (goto-char (1- (point-max))) (insert chunk))
        (aside--redraw block)))
    (aside--follow windows)))

(defun aside--redraw (block)
  "Replace BLOCK's text with how it should look now."
  (let* ((blocks (aside-turn-blocks aside--turn))
         (previous (cadr (memq block (reverse blocks))))
         (body (aside-turn-block-string block t aside--root (aside--width)))
         (text (if (string-empty-p body) "" (concat (aside-turn-separator previous block) body)))
         (next (aside--next-marker block))
         (start (aside-turn-block-marker block)))
    (if start
        (progn (delete-region start (or next (point-max)))
               (goto-char start))
      (goto-char (or next (point-max)))
      (setf (aside-turn-block-marker block) (point-marker)))
    (insert text)
    (when next (set-marker next (point)))))

(defun aside--erase (block)
  "Remove BLOCK from the turn and the popup."
  (when-let* ((start (aside-turn-block-marker block)))
    (let ((inhibit-read-only t))
      (delete-region start (or (aside--next-marker block) (point-max))))
    (set-marker start nil))
  (aside-turn-remove aside--turn block))

(defun aside--finish-turn (stop-reason &optional problem)
  "End the turn and show its answer.
STOP-REASON is why the agent stopped; PROBLEM, if any, why it failed."
  (let ((turn aside--turn)
        (windows (aside--following-windows))
        (inhibit-read-only t))
    (setf (aside-turn-finished turn) (float-time)
          (aside-turn-stop-reason turn) stop-reason
          (aside-turn-error turn) problem)
    (dolist (block (aside-turn-requests turn))
      (aside--answer block nil))
    (setq aside--queued nil
          aside--status-block nil)
    (delete-region aside--live-start (point-max))
    (goto-char (point-max))
    (aside--insert-outcome turn (aside--refresh-buffers))
    (add-text-properties (point-min) (point-max)
                         '(read-only t front-sticky (read-only) rear-nonsticky t))
    (setq buffer-read-only nil)
    (aside--compose)
    (aside--follow windows)
    (aside-request-mode -1)
    (force-mode-line-update)
    (aside--notify-done turn)))

(defun aside--insert-outcome (turn &optional unsaved)
  "Insert TURN's answer, any error, and what it did.
UNSAVED lists buffers whose files changed under unsaved edits."
  (let ((answer (aside-turn-answer turn))
        (start (point)))
    (unless (string-empty-p answer)
      (insert answer "\n")
      (aside-turn-fontify-markdown start (point)))
    (when-let* ((problem (aside-turn-error turn)))
      (unless (string-empty-p answer) (insert "\n"))
      (insert (propertize (concat (aside-turn-glyph 'failed) " " problem) 'face 'aside-failed
                          'wrap-prefix "  ")
              "\n"))
    (when-let* ((summary (aside-turn-summary turn aside--root)))
      (insert "\n" summary "\n"))
    (dolist (file unsaved)
      (insert (propertize (format "%s %s changed, but its buffer has unsaved edits\n"
                                  (aside-turn-glyph 'request)
                                  (file-relative-name file aside--root))
                          'face 'aside-request)))
    (insert "\n")))

(defun aside--refresh-buffers ()
  "Revert unmodified buffers whose files changed during the turn.
Return the files of modified buffers that were left alone."
  (let ((root (file-name-as-directory aside--root))
        unsaved)
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (when (and (string-prefix-p root (expand-file-name file))
                   (not (verify-visited-file-modtime buffer))
                   (file-exists-p file))
          (if (buffer-modified-p buffer)
              (push file unsaved)
            (with-current-buffer buffer
              (ignore-errors (revert-buffer t t t)))))))
    unsaved))

(defun aside--notify-done (turn)
  "Tell you TURN is over when its popup isn't on screen."
  (unless (aside-frame-visible-p (current-buffer))
    (let ((summary (format "%s %s in %s" (aside--agent-name aside--agent)
                           (if (aside-turn-error turn) "failed" "finished")
                           (file-name-nondirectory aside--root))))
      (if (and aside-notify (require 'notifications nil t))
          (let ((buffer (current-buffer)))
            (ignore-errors
              (notifications-notify
               :title summary
               :body (truncate-string-to-width (aside-turn-answer turn) 200 nil nil "…")
               :app-name "Emacs"
               ;; Clicking the notification brings the popup back.
               :actions '("default" "Show")
               :on-action (lambda (_id _key)
                            (when (buffer-live-p buffer) (aside--show buffer))))))
        (message "%s" summary)))))

;;;; Hearing from agents

(defun aside--on-notification (_conn method params)
  "Handle the notification METHOD with PARAMS from an agent."
  (when (equal method "session/update")
    (when-let* ((buffer (gethash (plist-get params :sessionId) aside--sessions))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (aside--update (plist-get params :update))))))

(defun aside--update (update)
  "Apply the session UPDATE to this popup."
  (pcase (plist-get update :sessionUpdate)
    ("config_option_update"
     (setq aside--options (plist-get update :configOptions))
     (force-mode-line-update))
    ("current_mode_update"
     (when-let* ((option (aside--option "mode")))
       (setq aside--options
             (cons (plist-put (copy-sequence option) :currentValue
                              (plist-get update :currentModeId))
                   (remq option aside--options))))
     (force-mode-line-update))
    ("usage_update"
     (setq aside--usage update)
     (let ((share (aside--context-share)))
       (when (and share (>= share 80) (not aside--context-warned))
         (setq aside--context-warned t)
         (message "%s has used %d%% of this session's context; %s starts a new session"
                  (aside--agent-name aside--agent) share (aside--key-text 'aside-new-session))))
     (force-mode-line-update))
    ((or "available_commands_update" "session_info_update") nil)
    (_
     (cond
      ((eq aside--state 'loading) (aside--remember-history update))
      ((aside-turn-running-p aside--turn)
       (when-let* ((block (aside-turn-update aside--turn update)))
         (aside--draw block (and (equal (plist-get update :sessionUpdate)
                                        "agent_message_chunk")
                                 (plist-get (plist-get update :content) :text)))))))))

(defun aside--on-request (_conn method params reply)
  "Answer the agent's request METHOD with PARAMS through REPLY."
  (pcase method
    ("session/request_permission" (aside--on-permission params reply))
    ("fs/read_text_file" (funcall reply (list :content (aside--read-file params))))
    ("fs/write_text_file" (aside--write-file params) (funcall reply nil))
    (_ (funcall reply nil (list :code -32601 :message (format "aside can't %s" method))))))

;;;; Permission

;; A request shows each option on a line of its own.  The cursor goes
;; to the first; move as anywhere else and choose with RET or a click.

(defvar-local aside--option-overlay nil "Highlights the option under the cursor.")
(defvar-local aside--evil-state-before nil
  "Evil's state before a request took the cursor, to go back to after.")

(defvar evil-state)
(declare-function evil-change-state "evil-core")

(define-minor-mode aside-request-mode
  "Highlight the permission option under the cursor while a request waits."
  :lighter nil
  (if aside-request-mode
      (add-hook 'post-command-hook #'aside--highlight-option nil t)
    (remove-hook 'post-command-hook #'aside--highlight-option t)
    (when (overlayp aside--option-overlay)
      (delete-overlay aside--option-overlay))
    (when aside--evil-state-before
      (when (bound-and-true-p evil-local-mode)
        (evil-change-state aside--evil-state-before))
      (setq aside--evil-state-before nil))))

(defun aside--highlight-option ()
  "Highlight the permission option on the line at point, if any."
  (if (not (get-text-property (point) 'aside-option))
      (when (overlayp aside--option-overlay)
        (delete-overlay aside--option-overlay))
    (unless (overlayp aside--option-overlay)
      (setq aside--option-overlay (make-overlay 1 1))
      (overlay-put aside--option-overlay 'face 'aside-choice-row))
    (move-overlay aside--option-overlay (line-beginning-position)
                  (min (1+ (line-end-position)) (point-max)))))

(defun aside--point-to-request ()
  "Put the cursor on the first option of the oldest unanswered request.
With Evil in insert state, switch to normal state, so moving works."
  (when-let* ((block (car (aside-turn-requests aside--turn)))
              (start (aside-turn-block-marker block))
              (pos (text-property-not-all start (point-max) 'aside-option nil)))
    (when (and (bound-and-true-p evil-local-mode) (eq evil-state 'insert))
      (setq aside--evil-state-before 'insert)
      (evil-change-state 'normal))
    (goto-char pos)
    (skip-chars-forward " ")
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window (point)))
    (aside--highlight-option)))

(defun aside--on-permission (params reply)
  "Show the permission request in PARAMS in its popup; REPLY answers it."
  (let ((buffer (gethash (plist-get params :sessionId) aside--sessions)))
    (if (not (and (buffer-live-p buffer)
                  (aside-turn-running-p (buffer-local-value 'aside--turn buffer))))
        (funcall reply (list :outcome (list :outcome "cancelled")))
      (with-current-buffer buffer
        (let ((block (aside-turn-add-request aside--turn (append params (list :reply reply)))))
          (aside--draw block)
          (aside-request-mode 1)
          (when (eq block (car (aside-turn-requests aside--turn)))
            (aside--point-to-request)))
        (force-mode-line-update)
        (cond
         ((aside-frame-selected-p buffer))
         (aside-reveal-on-request
          (aside-frame-reveal buffer (aside--frame-title)))
         (t (message "%s is waiting for your permission in %s"
                     (aside--agent-name aside--agent) (buffer-name buffer))))))))

(defun aside--answer (block option)
  "Answer the request in BLOCK with OPTION, a plist from the agent.
Nil cancels it."
  (funcall (plist-get (aside-turn-block-request block) :reply)
           (list :outcome (if option
                              (list :outcome "selected" :optionId (plist-get option :optionId))
                            (list :outcome "cancelled"))))
  (aside--erase block)
  (if (aside-turn-requests aside--turn)
      (aside--point-to-request)
    (aside-request-mode -1)
    (goto-char (point-max))
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window (point-max))))
  (force-mode-line-update))

(defun aside-answer-at-point (&optional event)
  "Answer the permission request with the option at point, or clicked in EVENT."
  (interactive (list last-nonmenu-event) aside-mode)
  (let* ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point)))
         (option (or (get-text-property pos 'aside-option)
                     (user-error "No option here")))
         (block (cl-find-if (lambda (block)
                              (memq option (plist-get (aside-turn-block-request block) :options)))
                            (aside-turn-requests aside--turn))))
    (unless block
      (user-error "That request was already answered"))
    (aside--answer block option)))

(defun aside-visit-file (&optional event)
  "Open the file named at point, or clicked in EVENT.
It opens in the frame the popup was opened from, so the popup stays
small."
  (interactive (list last-nonmenu-event) aside-mode)
  (let* ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point)))
         (file (or (get-text-property pos 'aside-file) (user-error "No file here")))
         (frame aside--origin-frame))
    (if (and (frame-live-p frame) (eq (frame-visible-p frame) t))
        (progn (select-frame-set-input-focus frame)
               (find-file file))
      (find-file-other-window file))))

;;;; Files

(defun aside--read-file (params)
  "Return the text of the file in PARAMS, as Emacs has it."
  (let* ((path (plist-get params :path))
         (line (plist-get params :line))
         (limit (plist-get params :limit))
         (buffer (find-buffer-visiting path)))
    (with-temp-buffer
      (if buffer
          (insert (with-current-buffer buffer
                    (save-restriction (widen) (buffer-substring-no-properties
                                               (point-min) (point-max)))))
        (insert-file-contents path))
      (goto-char (point-min))
      (let ((start (if line (progn (forward-line (1- line)) (point)) (point-min))))
        (when limit (forward-line limit))
        (buffer-substring-no-properties start (if limit (point) (point-max)))))))

(defun aside--write-file (params)
  "Write the file in PARAMS, keeping a buffer visiting it in step.
A buffer with unsaved edits is left alone; the file changes anyway,
and the end of the turn says so."
  (let* ((path (plist-get params :path))
         (content (plist-get params :content))
         (buffer (find-buffer-visiting path)))
    (unless (file-name-absolute-p path)
      (error "Not an absolute path: %s" path))
    (make-directory (file-name-directory path) t)
    (if (and buffer (not (buffer-modified-p buffer))
             (verify-visited-file-modtime buffer))
        ;; Edit the buffer, then save it as is: point, marks and undo
        ;; survive, and no save hooks reformat what the agent wrote.
        (with-current-buffer buffer
          (let ((source (generate-new-buffer " *aside write*"))
                (inhibit-read-only t)
                (inhibit-message t))
            (unwind-protect
                (save-restriction
                  (widen)
                  (with-current-buffer source (insert content))
                  (if (>= emacs-major-version 31)
                      (replace-region-contents (point-min) (point-max) source 1)
                    (with-no-warnings (replace-buffer-contents source 1)))
                  (write-region nil nil path nil t))
              (kill-buffer source))))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region content nil path nil 'silent)))
    (when-let* ((popup (gethash (plist-get params :sessionId) aside--sessions))
                ((buffer-live-p popup)))
      (with-current-buffer popup
        (when aside--turn
          (push path (aside-turn-written aside--turn)))))))

;;;; Resuming

(defun aside--remember-history (update)
  "Collect UPDATE, replayed while loading a session."
  (let ((turn (car aside--history)))
    (if (equal (plist-get update :sessionUpdate) "user_message_chunk")
        (let ((text (aside-turn--chunk-text (plist-get update :content))))
          (when (or (null turn) (aside-turn-blocks turn))
            (setq turn (aside-turn-create :prompt "" :started nil))
            (push turn aside--history))
          (when text
            (setf (aside-turn-prompt turn) (concat (aside-turn-prompt turn) text))))
      (unless turn
        (setq turn (aside-turn-create :prompt "" :started nil))
        (push turn aside--history))
      (aside-turn-update turn update))))

(defun aside--show-history ()
  "Show the last answered turn of the loaded session."
  (let ((turn (cl-find-if (lambda (turn) (not (string-empty-p (aside-turn-answer turn))))
                          aside--history))
        (inhibit-read-only t))
    (erase-buffer)
    (when turn
      (setf (aside-turn-finished turn) t
            (aside-turn-stop-reason turn) "end_turn")
      (insert (propertize (string-trim (aside-turn-prompt turn))
                          'line-prefix (aside--prompt-bar) 'wrap-prefix (aside--prompt-bar))
              "\n\n" (aside-turn-divider (aside--label)))
      (aside--insert-outcome turn)
      (add-text-properties (point-min) (point-max)
                           '(read-only t front-sticky (read-only) rear-nonsticky t)))
    (setq aside--turn turn
          aside--history nil)
    (aside--compose)))

(defun aside--load (session-id)
  "Load SESSION-ID into this popup and show its last exchange."
  (let ((buffer (current-buffer)))
    (setq aside--state 'loading
          aside--session session-id
          aside--last-agent aside--agent)
    (puthash session-id buffer aside--sessions)
    (aside--tick-soon)
    (aside-acp-request
     aside--conn "session/load"
     (list :sessionId session-id :cwd aside--root :mcpServers [])
     (lambda (result)
       (with-current-buffer buffer
         (when (plist-member result :configOptions)
           (setq aside--options (plist-get result :configOptions)))
         (setq aside--state 'ready)
         (aside--show-history)
         (force-mode-line-update)))
     (lambda (err) (aside--failed buffer (aside-acp-error-text err))))))

(defun aside--connect-now (agent)
  "Return a ready connection to AGENT, waiting for it to start."
  (let (conn problem)
    (aside--connect agent (lambda (c) (setq conn c)) (lambda (p) (setq problem p)))
    (with-local-quit
      (let ((reporter (make-progress-reporter
                       (format "Starting %s..." (aside--agent-name agent)))))
        (while (not (or conn problem))
          (accept-process-output nil 0.05)
          (progress-reporter-update reporter))
        (progress-reporter-done reporter)))
    (or conn (user-error "%s" (or problem "Stopped")))))

(defun aside--relative-time (iso)
  "Return how long ago the ISO 8601 time ISO was."
  (if-let* ((time (ignore-errors (date-to-time iso))))
      (let ((seconds (float-time (time-subtract nil time))))
        (cond ((< seconds 60) "just now")
              ((< seconds 3600) (format "%d min ago" (/ seconds 60)))
              ((< seconds 86400) (format "%d h ago" (/ seconds 3600)))
              (t (format "%d d ago" (/ seconds 86400)))))
    ""))

(defun aside--read-session (conn root then &optional cancel)
  "Ask which of ROOT's sessions on CONN to resume, and call THEN with its id.
The most recent comes first; this popup's own is marked.  CANCEL is
called if you choose none."
  (let* ((sessions (plist-get (aside-acp-request-sync conn "session/list" (list :cwd root))
                              :sessions))
         (sessions (cl-remove-if-not (lambda (s) (equal (plist-get s :cwd) root)) sessions))
         (seen (make-hash-table :test #'equal))
         (choices
          (mapcar (lambda (session)
                    (let* ((title (truncate-string-to-width
                                   (string-trim (or (plist-get session :title) "Untitled"))
                                   50 nil nil (aside-turn-glyph 'more)))
                           (n (cl-incf (gethash title seen 0))))
                      (list (if (> n 1) (format "%s <%d>" title n) title)
                            (plist-get session :sessionId)
                            (aside--relative-time (plist-get session :updatedAt)))))
                  sessions)))
    (unless choices
      (user-error "No earlier %s sessions in %s" (aside--agent-name (aside--agent-of conn))
                  (abbreviate-file-name root)))
    (aside--choose "Resume" choices :current aside--session :start (cadr (car choices))
                   :noun "sessions" :then then :cancel cancel)))

;;;; The mode line

(defun aside--mode-line-status ()
  "Return the state shown at the right of the mode line."
  (let ((name (aside--agent-name aside--agent)))
    (cond
     ((and aside--turn (aside-turn-requests aside--turn))
      (propertize (concat (aside-turn-glyph 'request) " needs your answer") 'face 'aside-request))
     ((aside--busy-p)
      (concat (propertize (aside-turn-spinner) 'face 'aside-running) " "
              (propertize (format "working %s %s %s stops"
                                  (aside-turn--duration aside--turn) (aside-turn-glyph 'dot)
                                  (aside--key-text 'aside-cancel))
                          'face 'aside-summary)))
     ((eq aside--state 'starting)
      (concat (propertize (aside-turn-spinner) 'face 'aside-running) " "
              (propertize (format "starting %s" name) 'face 'aside-summary)))
     ((eq aside--state 'loading)
      (concat (propertize (aside-turn-spinner) 'face 'aside-running) " "
              (propertize "loading" 'face 'aside-summary)))
     ((eq aside--state 'failed)
      (propertize (concat (aside-turn-glyph 'failed) " couldn't start") 'face 'aside-failed))
     (t (aside--usage-text)))))

(defun aside--context-share ()
  "Return the percentage of the context window the session uses, or nil."
  (let ((used (plist-get aside--usage :used))
        (size (plist-get aside--usage :size)))
    (and (numberp used) (numberp size) (> size 0)
         (round (* 100.0 (/ (float used) size))))))

(defun aside--usage-text ()
  "Return how much of the context window the session uses, and its cost.
From 80% the share stands out, as the agent will soon have to forget."
  (let ((share (aside--context-share))
        (cost (plist-get (plist-get aside--usage :cost) :amount)))
    (concat
     (and share (propertize (format "%d%% of context" share)
                            'face (if (>= share 80) 'aside-request 'aside-summary)))
     (and (numberp cost) (> cost 0)
          (propertize (format "  $%.2f" cost) 'face 'aside-summary)))))

(defun aside--mode-line-button (text command help)
  "Return TEXT for the mode line, running COMMAND on a click; HELP explains it."
  (propertize (truncate-string-to-width text 28 nil nil (aside-turn-glyph 'more))
              'mouse-face 'mode-line-highlight
              'help-echo help
              'local-map (make-mode-line-mouse-map
                          'mouse-1 (lambda (event)
                                     (interactive "e")
                                     ;; Act on the popup clicked, which
                                     ;; need not be the selected window.
                                     (select-window (posn-window (event-start event)))
                                     (call-interactively command)))))

(defun aside--mode-line ()
  "Return the popup's mode line.
The model, effort and mode it names can be clicked to change them."
  (let* ((model (aside--option-label "model"))
         (effort (aside--option-label "thought_level"))
         (mode (aside--option-label "mode"))
         (parts
          (delq nil
                (list (and model (aside--mode-line-button
                                  model #'aside-select-model "mouse-1: choose the model"))
                      (and effort (aside--mode-line-button
                                   (concat effort " effort") #'aside-select-effort
                                   "mouse-1: choose the reasoning effort"))
                      (and mode (aside--mode-line-button
                                 mode #'aside-set-option "mouse-1: change an option")))))
         (left (concat " " (aside--mode-line-button
                            (propertize (aside--agent-name aside--agent) 'face 'aside-mode-line-agent)
                            #'aside-switch-agent "mouse-1: switch to another agent")
                       (mapconcat (lambda (part) (concat " " (aside-turn-glyph 'dot) " " part))
                                  parts "")))
         (right (aside--mode-line-status)))
    (concat (aside--mode-line-escape left)
            (propertize " " 'display `(space :align-to (- right ,(1+ (string-width right)))))
            (aside--mode-line-escape right))))

(defun aside--mode-line-escape (string)
  "Escape the % signs in STRING, which the mode line reads as directives.
Each run of text keeps its properties."
  (let ((pos 0) (runs nil))
    (while (< pos (length string))
      (let ((next (or (next-property-change pos string) (length string))))
        (push (apply #'propertize
                     (string-replace "%" "%%" (substring-no-properties string pos next))
                     (text-properties-at pos string))
              runs)
        (setq pos next)))
    (apply #'concat (nreverse runs))))

(defun aside--tick ()
  "Animate busy popups; stop when none are."
  (let ((busy nil))
    (dolist (buffer (aside--popups))
      (with-current-buffer buffer
        (when (or (aside--busy-p) (memq aside--state '(starting loading reviving)))
          (setq busy t)
          (when (aside-turn-running-p aside--turn)
            (let ((windows (aside--following-windows)))
              (when (and aside--status-block (aside-turn-block-marker aside--status-block))
                (aside--draw-status))
              (dolist (block (aside-turn-blocks aside--turn))
                (when (and (eq (aside-turn-block-kind block) 'tool)
                           (equal (aside-turn-block-status block) "in_progress"))
                  (let ((inhibit-read-only t))
                    (save-excursion (aside--redraw block)))))
              (aside--follow windows)))
          (force-mode-line-update))))
    (unless busy
      (cancel-timer aside--ticker)
      (setq aside--ticker nil))))

(defun aside--tick-soon ()
  "Start animating busy popups."
  (unless aside--ticker
    (setq aside--ticker (run-with-timer 0.1 0.1 #'aside--tick))))

;;;; The popup's major mode

(defvar-keymap aside-mode-map
  :doc "Keys in an aside popup."
  "C-c C-c" #'aside-send
  "C-c C-k" #'aside-cancel
  "C-c C-m" #'aside-select-model
  "C-c C-e" #'aside-select-effort
  "C-c C-o" #'aside-set-option
  "C-c C-n" #'aside-new-session
  "C-c C-a" #'aside-switch-agent
  "C-c C-r" #'aside-resume
  "C-c C-x" #'aside-clear-context
  "C-c ?" #'aside-keys)

(keymap-set aside-turn-file-map "<mouse-1>" #'aside-visit-file)
(keymap-set aside-turn-file-map "RET" #'aside-visit-file)
(keymap-set aside-turn-option-map "<mouse-1>" #'aside-answer-at-point)
(keymap-set aside-turn-option-map "RET" #'aside-answer-at-point)

(defvar evil-ex-commands)
(declare-function evil-ex-define-cmd "evil-ex")
(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core")
(declare-function notifications-notify "notifications")
(declare-function evil-normalize-keymaps "evil-core")

(define-derived-mode aside-mode text-mode "aside"
  "Write a prompt for a coding agent and read its answer.
\\<aside-mode-map>
Send with \\[aside-send] (or :w with Evil).  While the agent works,
\\[aside-cancel] stops it; otherwise it puts the popup away, as does
:q.  :wq sends and puts the popup away; you get a notification when
the agent is done.  When the agent asks permission, the cursor goes to
its options: move to one and press RET, or click it.

\\{aside-mode-map}"
  ;; The fringe holds only the prompt bar: no wrap arrows, and the
  ;; same background as the text, whatever the theme gives fringes.
  (setq-local fringe-indicator-alist
              (cons '(continuation nil nil) fringe-indicator-alist))
  (face-remap-add-relative 'fringe '(:inherit default))
  (setq-local word-wrap t
              truncate-lines nil
              ;; Even with `global-display-line-numbers-mode', a popup has none.
              display-line-numbers-type nil
              mode-line-format '(:eval (aside--mode-line))
              header-line-format nil)
  (add-hook 'after-change-functions #'aside--update-placeholder nil t)
  (add-hook 'kill-buffer-hook #'aside--on-kill nil t)
  (when (boundp 'evil-ex-commands)
    (setq-local evil-ex-commands (copy-alist evil-ex-commands))
    (evil-ex-define-cmd "w[rite]" #'aside-send)
    (evil-ex-define-cmd "wq" #'aside-send-and-dismiss)
    (evil-ex-define-cmd "x[it]" #'aside-send-and-dismiss)
    (evil-ex-define-cmd "q[uit]" #'aside-dismiss)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'aside-mode 'insert)
  (evil-define-key* 'normal aside-mode-map "q" #'aside-dismiss))

(defun aside--on-kill ()
  "Stop this popup's work when its buffer is killed."
  (when (and aside--conn aside--session (aside-acp-live-p aside--conn))
    (when (aside--busy-p)
      (aside-acp-notify aside--conn "session/cancel" (list :sessionId aside--session)))
    (when (aside--capability aside--conn :sessionCapabilities :close)
      (aside-acp-request aside--conn "session/close" (list :sessionId aside--session)
                         #'ignore #'ignore)))
  (when aside--turn
    (dolist (block (aside-turn-requests aside--turn))
      (funcall (plist-get (aside-turn-block-request block) :reply)
               (list :outcome (list :outcome "cancelled")))))
  (when aside--session
    (remhash aside--session aside--sessions))
  (aside-frame-delete (current-buffer)))

;;;; Commands

(defun aside--popup-buffer ()
  "Return the popup buffer the selected window shows, or signal."
  (if (derived-mode-p 'aside-mode)
      (current-buffer)
    (user-error "Not in an aside popup")))

;;;###autoload
(defun aside (&optional choose-agent)
  "Open the current project's popup, or hide it if it's in front.
With an active region, attach the region to the next prompt.  With a
prefix argument CHOOSE-AGENT, start a new session and ask which agent
it should use."
  (interactive "P")
  (let* ((context (aside--region-context))
         (root (aside--project-root))
         (buffer (aside--project-popup root))
         (known (aside--known-agent))
         (new (null buffer)))
    (if (and buffer (not context) (not choose-agent) (aside-frame-selected-p buffer))
        (aside-dismiss)
      (when buffer
        (with-current-buffer buffer
          (when choose-agent (aside--check-idle))))
      (when new
        (setq buffer (aside--create (or known (car (aside--installed-agents))) root)))
      (when context
        (with-current-buffer buffer (aside--add-context context)))
      (unless (frame-parameter nil 'aside-buffer)
        (with-current-buffer buffer (setq aside--origin-frame (selected-frame))))
      (aside--show buffer)
      (cond
       ((or choose-agent (not known))
        (aside--read-agent "Agent" (and (not new) aside--agent)
                           #'aside--start-over
                           ;; A popup opened only to ask goes away again.
                           (and new (lambda () (kill-buffer buffer)))))
       (new
        (aside--open-session)
        (unless aside-default-agent
          (message "%s, as last time; %s switches agent" (aside--agent-name known)
                   (aside--key-text 'aside-switch-agent))))))))

;;;###autoload
(defun aside-resume (&optional choose-agent)
  "Resume one of the current project's earlier sessions, in its popup.
The popup's current session can be resumed later in turn.  With a
prefix argument CHOOSE-AGENT, ask which agent's sessions to list."
  (interactive "P")
  (let* ((in-popup (derived-mode-p 'aside-mode))
         (root (if in-popup aside--root (aside--project-root)))
         (buffer (if in-popup (current-buffer) (aside--project-popup root)))
         (known (aside--known-agent))
         (new (null buffer)))
    (if buffer
        (with-current-buffer buffer (aside--check-idle))
      (setq buffer (aside--create (or known (car (aside--installed-agents))) root)))
    (unless (frame-parameter nil 'aside-buffer)
      (with-current-buffer buffer (setq aside--origin-frame (selected-frame))))
    (aside--show buffer)
    (let ((cancel (and new (lambda () (kill-buffer buffer)))))
      (if (or choose-agent (not known))
          (aside--read-agent "Resume a session of" aside--agent
                             (lambda (agent) (aside--resume-from agent cancel))
                             cancel)
        (aside--resume-from aside--agent cancel)))))

(defun aside--resume-from (agent cancel)
  "List AGENT's sessions in this popup's project, and load the one chosen.
CANCEL is called if none is, or if they can't be listed."
  (condition-case err
      (let ((conn (aside--connect-now agent)))
        (unless (aside--capability conn :sessionCapabilities :list)
          (user-error "%s can't list its sessions" (aside--agent-name agent)))
        (aside--read-session
         conn aside--root
         (lambda (id)
           (let ((existing (gethash id aside--sessions)))
             (cond
              ((eq existing (current-buffer))
               (message "This is that session"))
              ((buffer-live-p existing)
               (when cancel (funcall cancel))
               (aside--show existing))
              (t (aside--start-over agent id conn)))))
         cancel))
    ((error quit)
     (when cancel (funcall cancel))
     (signal (car err) (cdr err)))))

;;;###autoload
(defun aside-toggle ()
  "Hide the popup you're in, or bring back the last one used."
  (interactive)
  (cond
   ((derived-mode-p 'aside-mode) (aside-dismiss))
   ((car (aside--popups)) (aside--show (car (aside--popups))))
   (t (call-interactively #'aside))))

(defun aside-dismiss ()
  "Put the popup away; the agent keeps working."
  (interactive nil aside-mode)
  (let ((busy (aside--busy-p))
        (name (aside--agent-name aside--agent)))
    (aside-frame-hide (aside--popup-buffer))
    (when busy
      (message "%s keeps working; %s brings the popup back" name
               (substitute-command-keys "\\<global-map>\\[aside-toggle]")))))

(defun aside-send-and-dismiss ()
  "Send the prompt and put the popup away."
  (interactive nil aside-mode)
  (aside-send)
  (aside-dismiss))

(defun aside-cancel ()
  "Stop the agent's current turn, or put the popup away if it's idle."
  (interactive nil aside-mode)
  (cond
   ((and (aside-turn-running-p aside--turn) aside--queued)
    (aside--finish-turn "cancelled"))
   ((aside-turn-running-p aside--turn)
    (aside-acp-notify aside--conn "session/cancel" (list :sessionId aside--session))
    (dolist (block (aside-turn-requests aside--turn))
      (aside--answer block nil))
    (aside--status "Stopping"))
   (t (aside-dismiss))))

(defun aside--check-idle ()
  "Signal if this popup's agent is working."
  (when (aside--busy-p)
    (user-error "%s is still working; %s stops it" (aside--agent-name aside--agent)
                (aside--key-text 'aside-cancel))))

(defun aside-new-session (&optional choose-agent)
  "Start over in this popup with a new session.
With a prefix argument CHOOSE-AGENT, ask which agent to use.  The old
session can still be resumed."
  (interactive "P" aside-mode)
  (aside--check-idle)
  (if choose-agent
      (aside--read-agent "Agent" aside--agent #'aside--start-over)
    (aside--start-over aside--agent)))

(defun aside-switch-agent ()
  "Start a new session in this popup with another agent.
The old session can still be resumed."
  (interactive nil aside-mode)
  (aside--check-idle)
  (unless (cdr (aside--installed-agents))
    (user-error "%s is the only agent installed" (aside--agent-name aside--agent)))
  (aside--read-agent "Agent" aside--agent
                     (lambda (agent)
                       (if (eq agent aside--agent)
                           (message "%s it is" (aside--agent-name agent))
                         (aside--start-over agent)))))

(defun aside--start-over (agent &optional session-id conn)
  "Start over in this popup with AGENT, in a new session.
With SESSION-ID, load that session through CONN instead."
  (aside--check-idle)
  (let ((inhibit-read-only t))
    (when aside--session (remhash aside--session aside--sessions))
    (unless (eq agent aside--agent)
      (setq aside--agent agent
            aside--conn nil)
      (rename-buffer (generate-new-buffer-name
                      (format "*aside: %s (%s)*" (file-name-nondirectory aside--root)
                              (aside--agent-name agent)))))
    (setq aside--session nil aside--turn nil aside--options nil aside--usage nil
          aside--context-warned nil aside--state nil aside--problem nil)
    (erase-buffer)
    (aside--compose)
    (if session-id
        (progn (setq aside--conn conn)
               (aside--load session-id))
      (aside--open-session))
    (force-mode-line-update)))

(defun aside--remember (id value)
  "Use VALUE for option ID in this agent's new sessions too."
  (setf (alist-get id (alist-get aside--agent aside--preferences) nil nil #'equal) value))

(defun aside--choose-value (option)
  "Ask for a new value of OPTION and set it; switch it if it is on-off."
  (if (equal (plist-get option :type) "boolean")
      (aside--set-chosen-value option (if (eq (plist-get option :currentValue) t) :false t))
    (aside--choose (plist-get option :name)
                   (mapcar (lambda (o)
                             (list (aside--choice-label (plist-get o :name))
                                   (plist-get o :value)
                                   (aside--value-note o)))
                           (plist-get option :options))
                   :current (plist-get option :currentValue)
                   :noun (and (equal (plist-get option :category) "model") "models")
                   :then (lambda (value) (aside--set-chosen-value option value)))))

(defun aside--set-chosen-value (option value)
  "Set OPTION to VALUE, remember it for new sessions and say so."
  (let ((id (plist-get option :id))
        (name (plist-get option :name)))
    (aside--remember id value)
    (aside--set-option-value
     id value
     (lambda ()
       (let ((chosen (aside--value-name (or (aside--option id) option)))
             (effort (aside--option "thought_level")))
         (if (equal (plist-get option :category) "model")
             ;; A model may or may not reason; say which, so the effort
             ;; setting appearing or vanishing is no surprise.
             (message "%s: %s %s %s" name chosen (aside-turn-glyph 'dot)
                      (if effort
                          (format "%s effort (%s changes it)" (aside--value-name effort)
                                  (aside--key-text 'aside-select-effort))
                        "no effort setting"))
           (message "%s: %s" name chosen)))))))

(defun aside--choice-label (name)
  "Return NAME for a list of choices: capitalized, any provider prefix dimmed.
OpenCode, for one, names its models \"OpenCode Zen/Big Pickle\"."
  (let ((name (aside--capitalized name)))
    (if (string-match "\\`\\(.*/\\)[^/]+\\'" name)
        (concat (propertize (match-string 1 name) 'face 'aside-summary)
                (substring name (match-end 1)))
      name)))

(defun aside--value-note (value)
  "Return the note for the option VALUE: its description, and if it is free."
  (let ((free (and (string-match-p "\\bfree\\b" (format "%s" (plist-get value :value)))
                   ;; No need to say so when the name already does.
                   (not (string-match-p "\\bfree\\b" (format "%s" (plist-get value :name)))))))
    (when-let* ((notes (delq nil (list (plist-get value :description) (and free "free")))))
      (string-join notes (concat " " (aside-turn-glyph 'dot) " ")))))

(defun aside--require-session ()
  "Signal unless this popup's session is open."
  (unless (and (eq aside--state 'ready) aside--options)
    (user-error "%s has no session options yet" (aside--agent-name aside--agent))))

(defun aside-select-model ()
  "Choose the model this session uses."
  (interactive nil aside-mode)
  (aside--require-session)
  (aside--choose-value (or (aside--option "model")
                           (user-error "%s doesn't offer a choice of model"
                                       (aside--agent-name aside--agent)))))

(defun aside-select-effort ()
  "Choose how hard the model reasons, when the model offers a choice."
  (interactive nil aside-mode)
  (aside--require-session)
  (aside--choose-value
   (or (aside--option "thought_level")
       ;; Spelled out: Emacs would write C-c C-m as "C-c RET".
       (user-error "%s offers no reasoning effort with %s.  Models that reason \
do; choose one with C-c C-m"
                   (aside--agent-name aside--agent)
                   (or (aside--option-label "model") "this model")))))

(defun aside--key-text (command)
  "Return the popup's key for COMMAND, as people read it."
  (if-let* ((keys (where-is-internal command aside-mode-map t)))
      ;; Emacs writes C-m as RET, which hides the mnemonic.
      (string-replace "C-c RET" "C-c C-m" (key-description keys))
    ""))

(defun aside-keys ()
  "List what you can do in the popup, with each key, and do the one chosen."
  (interactive nil aside-mode)
  (let ((actions
         (delq nil
               (list '("Agent" aside-switch-agent)
                     '("Model" aside-select-model)
                     '("Effort" aside-select-effort)
                     '("Options" aside-set-option)
                     '("New session" aside-new-session)
                     '("Resume a session" aside-resume)
                     (and aside--context '("Drop the attached regions" aside-clear-context))
                     (list (if (aside--busy-p) "Stop the agent" "Hide the popup")
                           'aside-cancel)))))
    (aside--choose "Keys"
                   (mapcar (lambda (action)
                             (list (car action) (cadr action) (aside--key-text (cadr action))))
                           actions)
                   :note (format "Select a region before %s to send it with the next prompt."
                                 (substitute-command-keys "\\<global-map>\\[aside]"))
                   :then #'call-interactively)))

(defun aside-set-option ()
  "Change one of the session's options, such as its mode or reasoning effort."
  (interactive nil aside-mode)
  (aside--require-session)
  (aside--choose "Option"
                 (mapcar (lambda (option)
                           (list (aside--capitalized (plist-get option :name)) option
                                 (aside--value-name option)))
                         aside--options)
                 :then #'aside--choose-value))

;;;###autoload
(defun aside-stop-agents ()
  "Stop every running agent process."
  (interactive)
  (pcase-dolist (`(,_ . ,conn) aside--connections)
    (aside-acp-stop conn))
  (message "Agents stopped"))

(provide 'aside)
;;; aside.el ends here
