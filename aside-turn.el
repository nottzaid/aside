;;; aside-turn.el --- What one exchange with an agent looks like  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;; Author: nottzaid
;; URL: https://github.com/nottzaid/aside

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A turn is one prompt and everything the agent does about it: its
;; thoughts, tool calls, plan, permission requests and answer.  This
;; file keeps a turn as data, updated from ACP `session/update'
;; notifications, and turns that data into text.  It owns no buffers
;; or markers; aside.el decides where the text goes.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'diff)
(require 'diff-mode)

(defcustom aside-show-thoughts 'brief
  "How much of the agent's reasoning to show while it works.
`brief' shows the latest line, `full' all of it, nil nothing."
  :type '(choice (const :tag "Latest line" brief)
                 (const :tag "Everything" full)
                 (const :tag "Nothing" nil))
  :group 'aside)

(defcustom aside-diff-preview-lines 8
  "How many lines of a proposed edit to show with a permission request."
  :type 'natnum
  :group 'aside)

(defcustom aside-output-lines 12
  "How many lines of a tool's output the full history shows at first.
The rest opens when you press RET on, or click, the line saying how
many more there are."
  :type 'natnum
  :group 'aside)

;;;; Faces

(defface aside-prompt-bar '((t :inherit font-lock-keyword-face))
  "The bar beside your prompt; its foreground is the bar's colour."
  :group 'aside)

(defface aside-prompt-bar-fringe '((t :inherit (aside-prompt-bar default)))
  "The prompt bar drawn in the fringe.
Its background is the text's, whatever colour the theme gives fringes."
  :group 'aside)

(defface aside-placeholder '((t :inherit shadow :slant italic))
  "The hint shown in an empty prompt."
  :group 'aside)

(defface aside-divider '((t :inherit shadow))
  "The rule between your prompt and the agent's reply."
  :group 'aside)

(defface aside-divider-label '((t :inherit (bold shadow)))
  "The agent and model named in the divider."
  :group 'aside)

(defface aside-thought '((t :inherit shadow :slant italic))
  "The agent's reasoning."
  :group 'aside)

(defface aside-tool '((t :inherit default))
  "A tool call's title."
  :group 'aside)

(defface aside-running '((t :inherit font-lock-keyword-face))
  "Marks work in progress."
  :group 'aside)

(defface aside-done '((t :inherit success))
  "Marks finished work."
  :group 'aside)

(defface aside-failed '((t :inherit error))
  "Marks failed work."
  :group 'aside)

(defface aside-request '((t :inherit (warning bold)))
  "A request waiting for your decision."
  :group 'aside)

(defface aside-key '((t :inherit help-key-binding))
  "A key you can press."
  :group 'aside)

(defface aside-summary '((t :inherit shadow))
  "The line saying what the agent did."
  :group 'aside)

(defface aside-output '((t :inherit shadow))
  "What a tool printed, in the full history."
  :group 'aside)

(defface aside-choice-row '((t :inherit hl-line))
  "The choice under the cursor."
  :group 'aside)

(defface aside-file-link '((t :underline t))
  "A file name that opens the file when clicked."
  :group 'aside)

(defvar aside-turn-file-map (make-sparse-keymap)
  "Keymap on file names in a turn's summary; aside.el binds it.")

(defface aside-code '((t :inherit font-lock-constant-face))
  "Inline code in an answer."
  :group 'aside)

(defface aside-markup '((t :inherit shadow))
  "Markdown punctuation in an answer."
  :group 'aside)

(defface aside-heading '((t :inherit bold))
  "Headings in an answer."
  :group 'aside)

;;;; Glyphs

(defconst aside-turn--glyphs
  '((done "✓" "+") (failed "✗" "x") (pending "○" "-") (thought "✻" "*")
    (request "?" "?") (cancelled "⊘" "/") (todo "☐" "[ ]") (doing "◐" "[~]")
    (finished "☑" "[x]") (more "…" "...") (bar "▎" "|") (dot "·" "-")
    (selected "●" "*") (unselected "○" "-") (arrow "→" "->"))
  "Glyphs by name, each with an ASCII fallback.")

(defconst aside-turn--spinner '("◐" "◓" "◑" "◒")
  "Frames of the progress spinner.")

(defun aside-turn-glyph (name)
  "Return the glyph called NAME, or its ASCII fallback if it can't be shown."
  (let ((entry (alist-get name aside-turn--glyphs)))
    (if (char-displayable-p (string-to-char (car entry)))
        (car entry)
      (cadr entry))))

(defun aside-turn-title-bar ()
  "Return the bar to set before a heading.
On graphical displays it is a thin stretch of colour, elsewhere a
box-drawing character."
  (if (display-graphic-p)
      (concat (propertize " " 'display '(space :width 0.25)
                          'face '(:inherit aside-prompt-bar :inverse-video t))
              (propertize " " 'display '(space :width 1.75)))
    (propertize (concat (aside-turn-glyph 'bar) " ") 'face 'aside-prompt-bar)))

(defun aside-turn-spinner ()
  "Return the spinner frame for the current moment."
  (nth (mod (floor (* (float-time) 8)) 4)
       (if (char-displayable-p ?◐) aside-turn--spinner '("|" "/" "-" "\\"))))

;;;; Data

(cl-defstruct (aside-turn (:constructor aside-turn-create)
                          (:copier nil))
  "One prompt and the agent's work on it.
LABEL names the agent and model that answered it."
  prompt blocks
  (started (float-time))
  finished stop-reason error
  written label)

(cl-defstruct (aside-turn-block (:constructor aside-turn-block-create)
                                (:copier nil))
  "One visible piece of a turn.
A permission request, once answered, becomes a `decision' block whose
ANSWER is the name of the option chosen."
  kind id chunks title status tool-kind locations content raw-input
  raw-output entries request answer marker)

(defun aside-turn--find (turn kind id)
  "Return TURN's block of KIND with ID."
  (cl-find-if (lambda (block)
                (and (eq (aside-turn-block-kind block) kind)
                     (equal (aside-turn-block-id block) id)))
              (aside-turn-blocks turn)))

(defun aside-turn--add (turn block)
  "Append BLOCK to TURN and return it."
  (setf (aside-turn-blocks turn) (nconc (aside-turn-blocks turn) (list block)))
  block)

(defun aside-turn-running-p (turn)
  "Return non-nil while TURN has not finished."
  (and turn (not (aside-turn-finished turn))))

(defun aside-turn--chunk-text (content)
  "Return the text of the ACP content block CONTENT."
  (pcase (plist-get content :type)
    ("text" (plist-get content :text))
    ("resource_link" (format "[%s]" (or (plist-get content :title)
                                        (plist-get content :name)
                                        (plist-get content :uri))))
    ("image" "[image]")
    (_ nil)))

(defun aside-turn-update (turn update)
  "Apply the ACP session UPDATE to TURN.
Return the block whose text changed, or nil if nothing visible did."
  (pcase (plist-get update :sessionUpdate)
    ((and (or "agent_message_chunk" "agent_thought_chunk") kind)
     (when-let* ((text (aside-turn--chunk-text (plist-get update :content)))
                 ((not (string-empty-p text))))
       (let* ((kind (if (equal kind "agent_message_chunk") 'message 'thought))
              (id (and (eq kind 'message) (plist-get update :messageId)))
              (last (car (last (aside-turn-blocks turn))))
              (block (if (and last (eq (aside-turn-block-kind last) kind)
                              (equal (aside-turn-block-id last) id))
                         last
                       (aside-turn--add turn (aside-turn-block-create
                                              :kind kind :id id)))))
         (push text (aside-turn-block-chunks block))
         block)))
    ((or "tool_call" "tool_call_update")
     (let* ((id (plist-get update :toolCallId))
            (block (or (aside-turn--find turn 'tool id)
                       (aside-turn--add turn (aside-turn-block-create
                                              :kind 'tool :id id :status "pending")))))
       (aside-turn--merge-tool block update)
       block))
    ("plan"
     (let ((block (or (aside-turn--find turn 'plan nil)
                      (aside-turn--add turn (aside-turn-block-create :kind 'plan)))))
       (setf (aside-turn-block-entries block) (plist-get update :entries))
       block))))

(defun aside-turn--merge-tool (block update)
  "Copy the tool fields present in UPDATE into BLOCK."
  (cl-macrolet ((take (key slot)
                  `(when (plist-member update ,key)
                     (setf (,slot block) (plist-get update ,key)))))
    (take :title aside-turn-block-title)
    (take :status aside-turn-block-status)
    (take :kind aside-turn-block-tool-kind)
    (take :locations aside-turn-block-locations)
    (take :content aside-turn-block-content)
    (take :rawInput aside-turn-block-raw-input)
    (take :rawOutput aside-turn-block-raw-output)))

(defun aside-turn-add-request (turn request)
  "Add the permission REQUEST, a plist from the agent, to TURN."
  (aside-turn--add turn (aside-turn-block-create :kind 'request :request request)))

(defun aside-turn-remove (turn block)
  "Remove BLOCK from TURN."
  (setf (aside-turn-blocks turn) (delq block (aside-turn-blocks turn))))

(defun aside-turn-requests (turn)
  "Return TURN's unanswered permission requests, oldest first."
  (cl-remove-if-not (lambda (block) (eq (aside-turn-block-kind block) 'request))
                    (aside-turn-blocks turn)))

(defun aside-turn--text (block)
  "Return the text accumulated in BLOCK."
  (apply #'concat (reverse (aside-turn-block-chunks block))))

(defun aside-turn-answer (turn)
  "Return TURN's answer: everything the agent said, without its thoughts."
  (string-trim
   (mapconcat #'identity
              (delete "" (mapcar (lambda (block)
                                   (if (eq (aside-turn-block-kind block) 'message)
                                       (string-trim (aside-turn--text block))
                                     ""))
                                 (aside-turn-blocks turn)))
              "\n\n")))

;;;; Paths

(defun aside-turn--relative (path root)
  "Return PATH relative to ROOT when it lies inside it."
  (if (and root (stringp path) (file-name-absolute-p path)
           (string-prefix-p (file-name-as-directory root) path))
      (file-relative-name path root)
    path))

(defun aside-turn--relativize (text root)
  "Shorten absolute paths under ROOT inside TEXT."
  (if (and root (stringp text))
      (string-replace (file-name-as-directory root) "" text)
    text))

(defun aside-turn-edited-files (turn)
  "Return the files TURN changed, as absolute paths."
  (let ((files (copy-sequence (aside-turn-written turn))))
    (dolist (block (aside-turn-blocks turn))
      (when (and (eq (aside-turn-block-kind block) 'tool)
                 (member (aside-turn-block-tool-kind block) '("edit" "delete" "move"))
                 (equal (aside-turn-block-status block) "completed"))
        (dolist (location (aside-turn-block-locations block))
          (push (plist-get location :path) files))
        (dolist (content (aside-turn-block-content block))
          (when (equal (plist-get content :type) "diff")
            (push (plist-get content :path) files)))))
    (delete-dups (cl-remove-if-not
                  (lambda (file) (and (stringp file) (not (directory-name-p file))))
                  files))))

;;;; Drawing blocks

(defun aside-turn-divider (label)
  "Return the rule that opens the agent's reply, naming LABEL."
  (concat (propertize (concat "── " label " ") 'face 'aside-divider-label)
          (propertize " " 'face '(:inherit aside-divider :strike-through t)
                      'display '(space :align-to right))
          "\n"))

(defun aside-turn-block-string (block running root width &optional full)
  "Return the text for BLOCK.
RUNNING is non-nil while the turn is in progress; ROOT shortens paths;
WIDTH limits one-line summaries.  FULL shows everything: whole
thoughts, what each tool was given and printed, and the answers to
permission requests."
  (pcase (aside-turn-block-kind block)
    ('message (let ((text (aside-turn--text block)))
                (if (string-empty-p text) "" (concat (string-trim-left text "\n+") "\n"))))
    ('thought (aside-turn--thought-string block width full))
    ('tool (concat (aside-turn--tool-string block running root)
                   (and full (aside-turn--tool-detail block root))))
    ('plan (aside-turn--plan-string block))
    ('request (aside-turn--request-string block root))
    ('decision (if full (aside-turn--decision-string block root) ""))
    (_ "")))

(defun aside-turn-separator (previous block)
  "Return the spacing to put between PREVIOUS and BLOCK.
Paragraphs of the answer and the indented lines of work between them
are set apart by a blank line."
  (if (and previous
           (or (eq (aside-turn-block-kind block) 'message)
               (eq (aside-turn-block-kind previous) 'message)))
      "\n"
    ""))

(defun aside-turn--thought-string (block width &optional full)
  "Return the thought BLOCK as configured by `aside-show-thoughts', within WIDTH.
FULL shows all of it whatever the setting."
  (let ((text (string-trim (aside-turn--text block))))
    (cond
     ((string-empty-p text) "")
     ((or full (eq aside-show-thoughts 'full))
      ;; Lines after the first sit under it, whether they wrap or not.
      (concat "  " (propertize (aside-turn-glyph 'thought) 'face 'aside-thought) " "
              (propertize text 'face 'aside-thought 'line-prefix "    " 'wrap-prefix "    ")
              "\n"))
     ((null aside-show-thoughts) "")
     (t
      (let ((line (car (last (split-string text "\n+" t "[ \t]+")))))
        (concat "  " (propertize (aside-turn-glyph 'thought) 'face 'aside-thought) " "
                (propertize (truncate-string-to-width
                             line (max 10 (- width 6)) nil nil (aside-turn-glyph 'more))
                            'face 'aside-thought)
                "\n"))))))

(defun aside-turn--status-glyph (status running)
  "Return the glyph for a tool STATUS, animated while RUNNING."
  (pcase status
    ("completed" (propertize (aside-turn-glyph 'done) 'face 'aside-done))
    ("failed" (propertize (aside-turn-glyph 'failed) 'face 'aside-failed))
    ("in_progress" (if running
                       (propertize (aside-turn-spinner) 'face 'aside-running)
                     (propertize (aside-turn-glyph 'cancelled) 'face 'aside-summary)))
    (_ (propertize (aside-turn-glyph 'pending) 'face 'aside-summary))))

(defun aside-turn--tool-title (block root)
  "Return a short, readable title for the tool BLOCK.
Paths under ROOT are shortened."
  (let* ((title (string-trim (or (aside-turn-block-title block) "")))
         (title (car (split-string title "\n")))
         (title (aside-turn--relativize title root)))
    (if (string-empty-p (or title ""))
        (capitalize (or (aside-turn-block-tool-kind block) "tool"))
      title)))

(defun aside-turn--tool-string (block running root)
  "Return the one-line view of the tool BLOCK.
RUNNING animates it while the turn is in progress; ROOT shortens paths."
  (concat "  " (aside-turn--status-glyph (aside-turn-block-status block) running) " "
          (propertize (aside-turn--tool-title block root) 'face 'aside-tool)
          "\n"))

(defun aside-turn--command (block)
  "Return the shell command the tool BLOCK ran, if it ran one."
  (let ((command (plist-get (aside-turn-block-raw-input block) :command)))
    (cond ((and (stringp command) (not (string-blank-p command))) command)
          ((and (or (consp command) (and (vectorp command) (> (length command) 0)))
                (cl-every #'stringp command))
           (mapconcat #'shell-quote-argument command " ")))))

(defun aside-turn--unfence (text)
  "Return TEXT without a Markdown code fence around all of it."
  (if (string-match "\\`[ \t]*```[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?[ \t]*```[ \t]*\\'" text)
      (match-string 1 text)
    text))

(defun aside-turn--tool-output (block root)
  "Return what the tool BLOCK printed or changed, as lines, or nil.
Agents share output as text, as diffs, or only in their raw report."
  (let ((lines nil))
    (dolist (item (aside-turn-block-content block))
      (pcase (plist-get item :type)
        ("content"
         (when-let* ((text (aside-turn--chunk-text (plist-get item :content))))
           (setq lines (append lines (split-string (aside-turn--unfence (string-trim-right text))
                                                   "\n")))))
        ("diff"
         (setq lines
               (append lines
                       (list (propertize (aside-turn--relative (plist-get item :path) root)
                                         'face 'aside-summary))
                       (mapcar (lambda (line)
                                 (propertize line 'face (if (string-prefix-p "+" line)
                                                            'diff-indicator-added
                                                          'diff-indicator-removed)))
                               (ignore-errors
                                 (aside-turn--diff-lines (plist-get item :oldText)
                                                         (plist-get item :newText)))))))))
    (unless lines
      (let* ((raw (aside-turn-block-raw-output block))
             (text (cond ((stringp raw) raw)
                         ((and (consp raw) (stringp (plist-get raw :output)))
                          (plist-get raw :output)))))
        (when (and text (not (string-blank-p text)))
          (setq lines (split-string (aside-turn--unfence (string-trim-right text)) "\n")))))
    lines))

(defvar aside-turn-fold-map (make-sparse-keymap)
  "Keymap on the line that opens folded output; aside.el binds it.")

(defun aside-turn-fold (lines indent face)
  "Return LINES, each after INDENT and in FACE, folded after `aside-output-lines'.
The line standing for the rest opens it, through `aside-turn-unfold'."
  (let* ((format-line (lambda (line)
                        (concat indent (propertize line 'face (or (get-text-property 0 'face line)
                                                                  face))
                                "\n")))
         (shown (seq-take lines aside-output-lines))
         (rest (nthcdr (length shown) lines)))
    (concat
     (mapconcat format-line shown "")
     (when rest
       (propertize (concat indent
                           (propertize (format "%s %d more line%s" (aside-turn-glyph 'more)
                                               (length rest) (if (cdr rest) "s" ""))
                                       'face 'aside-summary)
                           "\n")
                   'aside-fold (mapconcat format-line rest "")
                   'keymap aside-turn-fold-map
                   'mouse-face 'highlight
                   'help-echo "Click or press RET to show the rest")))))

(defun aside-turn-unfold (pos)
  "Replace the folded line at POS with the lines it stands for."
  (when-let* ((rest (get-text-property pos 'aside-fold)))
    (let* ((start (or (previous-single-property-change (1+ pos) 'aside-fold) (point-min)))
           (end (or (next-single-property-change pos 'aside-fold) (point-max)))
           (read-only (get-text-property start 'read-only))
           (inhibit-read-only t))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert rest)
        (when read-only
          (add-text-properties start (point)
                               '(read-only t front-sticky (read-only) rear-nonsticky t)))))))

(defun aside-turn--tool-detail (block root)
  "Return what the tool BLOCK was given and what it printed, for the full history."
  (let ((command (aside-turn--command block))
        (output (aside-turn--tool-output block root)))
    (concat
     (and command
          (propertize (concat "    $ " (aside-turn--relativize command root) "\n")
                      'face 'aside-code 'wrap-prefix "      "))
     (and output (aside-turn-fold output "    " 'aside-output)))))

(defun aside-turn--plan-string (block)
  "Return the plan BLOCK as a checklist."
  (mapconcat
   (lambda (entry)
     (pcase (plist-get entry :status)
       ("completed" (concat "  " (propertize (aside-turn-glyph 'finished) 'face 'aside-done)
                            " " (propertize (plist-get entry :content) 'face 'aside-summary) "\n"))
       ("in_progress" (concat "  " (propertize (aside-turn-glyph 'doing) 'face 'aside-running)
                              " " (plist-get entry :content) "\n"))
       (_ (concat "  " (propertize (aside-turn-glyph 'todo) 'face 'aside-summary)
                  " " (plist-get entry :content) "\n"))))
   (aside-turn-block-entries block) ""))

;;;; Permission requests

(defvar aside-turn-option-map (make-sparse-keymap)
  "Keymap on each option of a permission request; aside.el binds it.")

(defun aside-turn--diff-lines (old new)
  "Return the changed lines between the strings OLD and NEW, as a list."
  (let ((a (make-temp-file "aside-old")) (b (make-temp-file "aside-new")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (or old "") nil a nil 'silent)
            (write-region (or new "") nil b nil 'silent))
          (with-temp-buffer
            (call-process diff-command nil t nil "-U0" a b)
            (cl-loop for line in (split-string (buffer-string) "\n" t)
                     when (and (string-match-p "\\`[-+]" line)
                               (not (string-match-p "\\`\\(---\\|\\+\\+\\+\\) " line)))
                     collect line)))
      (delete-file a)
      (delete-file b))))

(defun aside-turn--request-preview (tool root)
  "Return preview lines for the tool call TOOL awaiting permission.
Paths under ROOT are shortened."
  (let* ((diff (cl-find "diff" (plist-get tool :content)
                        :key (lambda (c) (plist-get c :type)) :test #'equal))
         (command (plist-get (plist-get tool :rawInput) :command)))
    (cond
     (diff
      (let* ((lines (ignore-errors (aside-turn--diff-lines (plist-get diff :oldText)
                                                           (plist-get diff :newText))))
             (shown (seq-take lines aside-diff-preview-lines))
             (hidden (- (length lines) (length shown))))
        (append
         (mapcar (lambda (line)
                   (propertize line 'face (if (string-prefix-p "+" line)
                                              'diff-indicator-added
                                            'diff-indicator-removed)))
                 shown)
         (when (> hidden 0)
           (list (propertize (format "%s %d more line%s" (aside-turn-glyph 'more)
                                     hidden (if (= hidden 1) "" "s"))
                             'face 'aside-summary))))))
     ((stringp command)
      (list (propertize (concat "$ " (aside-turn--relativize command root))
                        'face 'aside-code))))))

(defun aside-turn-request-title (request root)
  "Return the one-line title of the permission REQUEST, paths under ROOT shortened."
  (let* ((tool (plist-get request :toolCall))
         (title (aside-turn--relativize
                 (or (plist-get tool :title) (plist-get tool :kind) "Permission")
                 root)))
    (car (split-string (string-trim title) "\n"))))

(defun aside-turn--request-string (block root)
  "Return the permission request BLOCK, with paths under ROOT shortened.
Each option is a line of its own; RET on it, or a click, chooses it."
  (let ((request (aside-turn-block-request block)))
    (concat
     "  " (propertize (aside-turn-glyph 'request) 'face 'aside-request) " "
     (propertize (aside-turn-request-title request root) 'face 'aside-request) "\n"
     (mapconcat (lambda (line)
                  (propertize (concat "    " line "\n") 'wrap-prefix "      "))
                (aside-turn--request-preview (plist-get request :toolCall) root) "")
     (mapconcat (lambda (option)
                  ;; The whole line, so RET works wherever the cursor is on it.
                  (propertize (concat "    " (propertize (aside-turn-glyph 'unselected)
                                                         'face 'aside-summary)
                                      " " (plist-get option :name) "\n")
                              'aside-option option
                              'keymap aside-turn-option-map
                              'mouse-face 'highlight
                              'help-echo "Click or press RET to choose this"))
                (plist-get request :options) ""))))

(defun aside-turn--decision-string (block root)
  "Return the answered request BLOCK, for the full history."
  (concat "  " (propertize (aside-turn-glyph 'request) 'face 'aside-summary) " "
          (propertize (concat (aside-turn-request-title (aside-turn-block-request block) root)
                              " " (aside-turn-glyph 'arrow) " "
                              (or (aside-turn-block-answer block) "no answer"))
                      'face 'aside-summary)
          "\n"))

;;;; The finished turn

(defun aside-turn--duration (turn)
  "Return how long TURN took, as text."
  (let ((seconds (round (- (or (aside-turn-finished turn) (float-time))
                           (aside-turn-started turn)))))
    (if (< seconds 60)
        (format "%ds" seconds)
      (format "%dm %02ds" (/ seconds 60) (% seconds 60)))))

(defun aside-turn-summary (turn root)
  "Return the line saying what TURN did, naming files relative to ROOT.
Return nil when there is nothing worth saying, as when it failed at once."
  (let* ((tools (cl-remove-if-not (lambda (b) (eq (aside-turn-block-kind b) 'tool))
                                  (aside-turn-blocks turn)))
         (files (aside-turn-edited-files turn))
         (commands (cl-count "execute" tools :key #'aside-turn-block-tool-kind :test #'equal))
         (failed (cl-count "failed" tools :key #'aside-turn-block-status :test #'equal))
         (stop (aside-turn-stop-reason turn))
         (work
          (delq nil
                (list
                 (cond ((null files) nil)
                       ((<= (length files) 2)
                        (concat "edited "
                                (mapconcat (lambda (file)
                                             (aside-turn--file-link file root))
                                           files ", ")))
                       (t (format "edited %d files" (length files))))
                 (and (> commands 0)
                      (format "ran %d command%s" commands (if (= commands 1) "" "s")))
                 (and (> failed 0)
                      (propertize (format "%d failed" failed) 'face 'aside-failed))
                 (pcase stop
                   ("cancelled" "cancelled")
                   ("max_tokens" "stopped at the token limit")
                   ("max_turn_requests" "stopped at the request limit")
                   ("refusal" "refused")))))
         (parts (append work
                        (and (numberp (aside-turn-started turn))
                             (numberp (aside-turn-finished turn))
                             (list (aside-turn--duration turn)))))
         (glyph (cond ((aside-turn-error turn)
                       (propertize (aside-turn-glyph 'failed) 'face 'aside-failed))
                      ((equal stop "cancelled") (aside-turn-glyph 'cancelled))
                      (t (propertize (aside-turn-glyph 'done) 'face 'aside-done)))))
    (unless (or (null parts) (and (aside-turn-error turn) (null work)))
      (let ((text (string-join parts (concat " " (aside-turn-glyph 'dot) " "))))
        ;; Appended, so the faces of failures and file names show through.
        (add-face-text-property 0 (length text) 'aside-summary t text)
        (concat glyph " " text)))))

(defun aside-turn--file-link (file root)
  "Return FILE, shortened under ROOT, as a link that opens it."
  (propertize (aside-turn--relative file root)
              'face 'aside-file-link
              'aside-file file
              'mouse-face 'highlight
              'help-echo "Click or press RET to open this file"
              'keymap aside-turn-file-map))

;;;; Markdown

(defcustom aside-code-languages
  '(("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
    ("sh" . sh-mode) ("bash" . sh-mode) ("shell" . sh-mode) ("zsh" . sh-mode)
    ("console" . sh-mode) ("js" . js-mode) ("javascript" . js-mode)
    ("ts" . typescript-ts-mode) ("typescript" . typescript-ts-mode)
    ("py" . python-mode) ("python" . python-mode) ("rb" . ruby-mode)
    ("rs" . rust-ts-mode) ("rust" . rust-ts-mode) ("go" . go-ts-mode)
    ("c++" . c++-mode) ("cpp" . c++-mode) ("yml" . yaml-ts-mode)
    ("json" . js-json-mode) ("diff" . diff-mode) ("patch" . diff-mode))
  "Major modes for fenced code blocks, by the language named after the fence.
Languages not listed here use `LANGUAGE-mode' when it exists."
  :type '(alist :key-type string :value-type function)
  :group 'aside)

(defun aside-turn--code-mode (language)
  "Return the major mode to highlight LANGUAGE with, or nil."
  (let ((mode (or (cdr (assoc (downcase language) aside-code-languages))
                  (intern-soft (concat (downcase language) "-mode")))))
    (and mode (fboundp mode) mode)))

(defun aside-turn--highlight-code (start end mode)
  "Highlight the text between START and END as MODE would."
  (let ((code (buffer-substring-no-properties start end))
        (faces nil))
    (with-temp-buffer
      (insert code)
      (delay-mode-hooks (ignore-errors (funcall mode)))
      (ignore-errors (font-lock-ensure))
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face (push (list (1- pos) (1- next) face) faces))
            (setq pos next)))))
    (pcase-dolist (`(,from ,to ,face) faces)
      (put-text-property (+ start from) (+ start to) 'face face))))

(defun aside-turn-fontify-markdown (start end)
  "Highlight the Markdown in the answer between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*```\\([[:alnum:]+#-]*\\).*\n" nil t)
        (let ((fence-start (match-beginning 0))
              (body (point))
              (mode (aside-turn--code-mode (match-string 1))))
          (put-text-property fence-start body 'face 'aside-markup)
          (when (re-search-forward "^[ \t]*```[ \t]*$" nil 'move)
            (put-text-property (match-beginning 0) (match-end 0) 'face 'aside-markup)
            (when mode (aside-turn--highlight-code body (match-beginning 0) mode))
            (put-text-property fence-start (point) 'aside-code-block t))))
      (aside-turn--fontify-inline))))

(defun aside-turn--outside-code-p (pos)
  "Return non-nil if POS is not inside a fenced code block."
  (not (get-text-property pos 'aside-code-block)))

(defun aside-turn--fontify-inline ()
  "Highlight headings, emphasis and inline code outside code blocks."
  (goto-char (point-min))
  (while (re-search-forward "^\\(#+ \\)\\(.+\\)$" nil t)
    (when (aside-turn--outside-code-p (match-beginning 0))
      (put-text-property (match-beginning 1) (match-end 1) 'face 'aside-markup)
      (put-text-property (match-beginning 2) (match-end 2) 'face 'aside-heading)))
  (goto-char (point-min))
  (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" nil t)
    (when (aside-turn--outside-code-p (match-beginning 0))
      (put-text-property (match-beginning 0) (match-beginning 1) 'face 'aside-markup)
      (put-text-property (match-end 1) (match-end 0) 'face 'aside-markup)
      (add-face-text-property (match-beginning 1) (match-end 1) 'bold)))
  (goto-char (point-min))
  (while (re-search-forward "\\(?:^\\|[^*[:alnum:]]\\)\\(\\*\\)\\([^*[:space:]]\\(?:[^*\n]*[^*[:space:]]\\)?\\)\\(\\*\\)\\(?:[^*[:alnum:]]\\|$\\)"
                            nil t)
    (when (aside-turn--outside-code-p (match-beginning 2))
      (put-text-property (match-beginning 1) (match-end 1) 'face 'aside-markup)
      (put-text-property (match-beginning 3) (match-end 3) 'face 'aside-markup)
      (add-face-text-property (match-beginning 2) (match-end 2) 'italic))
    (goto-char (match-end 3)))
  (goto-char (point-min))
  (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
    (when (aside-turn--outside-code-p (match-beginning 0))
      (put-text-property (match-beginning 0) (match-beginning 1) 'face 'aside-markup)
      (put-text-property (match-end 1) (match-end 0) 'face 'aside-markup)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'aside-code))))

(provide 'aside-turn)
;;; aside-turn.el ends here
