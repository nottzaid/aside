;;; aside-list.el --- Choosing from a list in the popup  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;; Author: nottzaid
;; URL: https://github.com/nottzaid/aside

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Whenever aside asks you to choose, the choices take the popup's
;; window for a moment: an agent, a model, a session, an option.  The
;; list is an ordinary read-only buffer, so you move in it as in any
;; other: j and k, gg and G, C-d and C-u, / to search with Evil;
;; C-n, C-p and C-s without.  RET or a click chooses; q, ESC or C-g
;; puts the list away.  Either way the popup comes back as it was.

;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'aside-turn)
(require 'aside-frame)

(defface aside-choice-title '((t :inherit bold))
  "The heading of a list of choices."
  :group 'aside)

(defface aside-choice-current '((t :inherit (bold font-lock-keyword-face)))
  "The choice in use."
  :group 'aside)

(defvar-local aside-list--choices nil "The choices offered, as (LABEL VALUE NOTE).")
(defvar-local aside-list--then nil "Function called with the value chosen.")
(defvar-local aside-list--cancel nil "Function called when nothing is chosen.")
(defvar-local aside-list--origin nil "The buffer the list took the window from.")
(defvar-local aside-list--view nil "Where the origin's window was: (POINT . START).")
(defvar-local aside-list--done nil "Non-nil once the list has been answered.")

(defun aside-list--hints ()
  "Return the keys that work in a list, for its mode line."
  (let ((evil (bound-and-true-p evil-local-mode))
        (dot (propertize (concat " " (aside-turn-glyph 'dot) " ") 'face 'aside-placeholder)))
    (concat " "
            (mapconcat (lambda (pair)
                         (concat (propertize (car pair) 'face 'aside-key)
                                 (propertize (concat " " (cdr pair)) 'face 'aside-placeholder)))
                       (if evil
                           '(("j k" . "move") ("RET" . "chooses") ("/" . "searches")
                             ("q" . "cancels"))
                         '(("C-n C-p" . "move") ("RET" . "chooses") ("C-s" . "searches")
                           ("q" . "cancels")))
                       dot))))

(defun aside-list--row (choice width marks current)
  "Return the line for CHOICE, whose label is padded to WIDTH.
MARKS adds a column marking the choice whose value is CURRENT."
  (pcase-let* ((`(,label ,value ,note) choice)
               (chosen (and marks (equal value current)))
               (label-column (if marks 4 2)))
    (concat "  "
            (cond ((not marks) "")
                  (chosen (propertize (concat (aside-turn-glyph 'selected) " ")
                                      'face 'aside-choice-current))
                  (t (propertize (concat (aside-turn-glyph 'unselected) " ")
                                 'face 'aside-summary)))
            ;; Appended, so faces already in the label show through.
            (let ((label (copy-sequence label)))
              (add-face-text-property 0 (length label)
                                      (if chosen 'aside-choice-current 'default) t label)
              label)
            (when note
              (concat (propertize " " 'display `(space :align-to ,(+ label-column width 3)))
                      (propertize note 'face 'aside-summary)))
            "\n")))

(defun aside-list--insert (choices current note)
  "Insert CHOICES, marking CURRENT among them, then NOTE.
The last choice ends the buffer, so moving to the last line lands on it;
NOTE is shown below it, where the cursor can't go."
  (let* ((width (apply #'max (mapcar (lambda (choice) (string-width (car choice))) choices)))
         (marks (and (cl-find current choices :key #'cadr :test #'equal) t))
         (index 0))
    (dolist (choice choices)
      (insert (propertize (aside-list--row choice width marks current) 'aside-index index))
      (cl-incf index))
    (delete-char -1)
    (when note
      (overlay-put (make-overlay (point-max) (point-max)) 'after-string
                   (concat "\n\n" (propertize (concat "  " note) 'face 'aside-summary
                                               'wrap-prefix "  "))))))

(defun aside-list--index (&optional pos)
  "Return the index of the choice on the line at POS, or nil."
  (save-excursion
    (when pos (goto-char pos))
    (get-text-property (line-beginning-position) 'aside-index)))

(defun aside-list--goto (index)
  "Put point on the label of choice INDEX."
  (goto-char (point-min))
  (forward-line index)
  (skip-chars-forward " ●○*-" (line-end-position)))

(defun aside-list-choose (title choices &rest args)
  "Show CHOICES in the selected window, and act on the one chosen.
Each choice is a list (LABEL VALUE NOTE), where NOTE may be nil.
TITLE heads the list.  ARGS are keywords:

  :current  the value in use; it is marked, and the cursor starts on it
  :start    the value the cursor starts on instead
  :noun     what the choices are, to count them in the heading
  :note     text to show below the choices
  :then     function called with the value chosen
  :cancel   function called if you choose nothing

THEN and CANCEL run with the window's former buffer current, once it
is back in the window."
  (unless choices
    (user-error "There is nothing to choose from"))
  (let* ((window (selected-window))
         (origin (window-buffer window))
         (current (plist-get args :current))
         (start (if (plist-member args :start) (plist-get args :start) current))
         (noun (plist-get args :noun))
         (buffer (generate-new-buffer (format "*aside: %s*" (downcase title)))))
    (with-current-buffer buffer
      (aside-list-mode)
      (setq aside-list--choices choices
            aside-list--then (plist-get args :then)
            aside-list--cancel (plist-get args :cancel)
            aside-list--origin origin
            aside-list--view (cons (window-point window) (window-start window))
            default-directory (buffer-local-value 'default-directory origin)
            header-line-format
            (concat " " (aside-turn-title-bar)
                    (propertize title 'face 'aside-choice-title)
                    (and noun (propertize (format " %s %d %s" (aside-turn-glyph 'dot)
                                                  (length choices)
                                                  (if (cdr choices) noun
                                                    (string-remove-suffix "s" noun)))
                                          'face 'aside-summary))))
      (let ((inhibit-read-only t))
        (aside-list--insert choices current (plist-get args :note)))
      (aside-list--goto (or (cl-position start choices :key #'cadr :test #'equal) 0)))
    (aside-frame-swap window buffer)
    (set-window-point window (with-current-buffer buffer (point)))
    (set-buffer buffer)
    (hl-line-highlight)
    buffer))

(defun aside-list--close ()
  "Put the list away and give its window back to the buffer it took it from."
  (let ((window (get-buffer-window (current-buffer) t))
        (origin aside-list--origin)
        (view aside-list--view))
    (setq aside-list--done t)
    (when (and window (buffer-live-p origin))
      (aside-frame-swap window origin)
      (set-window-start window (cdr view) t)
      (set-window-point window (car view)))
    (kill-buffer (current-buffer))
    (when (buffer-live-p origin)
      (set-buffer origin))))

(defun aside-list-choose-at-point (&optional event)
  "Choose the choice at point, or the one clicked in EVENT."
  (interactive (list last-nonmenu-event) aside-list-mode)
  (when (mouse-event-p event)
    (select-window (posn-window (event-start event)))
    (goto-char (posn-point (event-start event))))
  (let ((index (or (aside-list--index) (user-error "Move to a choice first")))
        (then aside-list--then))
    (let ((value (cadr (nth index aside-list--choices))))
      (aside-list--close)
      (when then (funcall then value)))))

(defun aside-list-cancel ()
  "Put the list away without choosing."
  (interactive nil aside-list-mode)
  (let ((cancel aside-list--cancel))
    (aside-list--close)
    (when cancel (funcall cancel))))

(defun aside-list--on-kill ()
  "Give the window back when the list is killed without an answer."
  (unless aside-list--done
    (setq aside-list--done t)
    (let ((window (get-buffer-window (current-buffer) t))
          (cancel aside-list--cancel)
          (origin aside-list--origin))
      (when (and window (buffer-live-p origin))
        (aside-frame-swap window origin))
      (when (and cancel (buffer-live-p origin))
        (with-current-buffer origin (funcall cancel))))))

(defvar-keymap aside-list-mode-map
  :doc "Keys in a list of choices."
  "RET" #'aside-list-choose-at-point
  "<mouse-1>" #'aside-list-choose-at-point
  "q" #'aside-list-cancel
  "<escape>" #'aside-list-cancel
  "C-g" #'aside-list-cancel)

(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core")

(define-derived-mode aside-list-mode special-mode "aside-list"
  "Choose one of the lines with RET or a click; q puts the list away.
Move as in any buffer.
\\{aside-list-mode-map}"
  (setq-local truncate-lines t
              display-line-numbers-type nil
              display-line-numbers nil
              mode-line-format '(:eval (aside-list--hints))
              hl-line-face 'aside-choice-row)
  ;; The heading reads as the list's first line, not as a bar.
  (face-remap-set-base 'header-line '(:inherit default))
  (setq-local fringe-indicator-alist (cons '(truncation nil nil) fringe-indicator-alist))
  (face-remap-add-relative 'fringe '(:inherit default))
  (hl-line-mode 1)
  (add-hook 'kill-buffer-hook #'aside-list--on-kill nil t))

(with-eval-after-load 'evil
  ;; Motion state: Evil's moves and searches work, and keys that edit don't.
  (evil-set-initial-state 'aside-list-mode 'motion)
  (evil-define-key* 'motion aside-list-mode-map
    (kbd "RET") #'aside-list-choose-at-point
    "q" #'aside-list-cancel
    [escape] #'aside-list-cancel))

(provide 'aside-list)
;;; aside-list.el ends here
