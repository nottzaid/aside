;;; aside-frame.el --- Where aside popups appear  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;; Author: nottzaid
;; URL: https://github.com/nottzaid/emacs-oc

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A popup lives in a small frame of its own, or in a window of the
;; current frame on a text terminal.  Hiding a frame makes it
;; invisible rather than deleting it, so showing it again is instant
;; and keeps its window state.  Popup frames have predictable titles,
;; so a tiling window manager can be told to float them.

;;; Code:

(require 'cl-lib)

(defcustom aside-display 'frame
  "Where popups appear.
`frame' gives each popup a small frame of its own; `window' uses a
window at the bottom of the selected frame.  Text terminals always
use `window'."
  :type '(choice (const :tag "A frame of its own" frame)
                 (const :tag "A window in the current frame" window))
  :group 'aside)

(defcustom aside-frame-title "aside"
  "First word of every popup frame's title.
Titles read \"aside · PROJECT\"; match \"^aside\" in a window
manager rule to float popups."
  :type 'string
  :group 'aside)

(defcustom aside-frame-parameters
  '((width . 78) (height . 22)
    (minibuffer . t) (unsplittable . t)
    (tool-bar-lines . 0) (menu-bar-lines . 0) (tab-bar-lines . 0)
    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
    (internal-border-width . 14) (left-fringe . 0) (right-fringe . 0))
  "Frame parameters for popup frames."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'aside)

(defcustom aside-window-height 0.35
  "Height of the popup window when `aside-display' is `window'."
  :type 'number
  :group 'aside)

(defun aside-frame--use-frame-p ()
  "Return non-nil if popups should get frames of their own."
  (and (eq aside-display 'frame) (display-graphic-p)))

(defun aside-frame-of (buffer)
  "Return the live popup frame showing BUFFER, visible or not."
  (cl-find-if (lambda (frame)
                (eq (frame-parameter frame 'aside-buffer) buffer))
              (frame-list)))

(defun aside-frame-visible-p (buffer)
  "Return non-nil if BUFFER's popup can be seen."
  (if-let* ((frame (aside-frame-of buffer)))
      (eq (frame-visible-p frame) t)
    (get-buffer-window buffer 'visible)))

(defun aside-frame-selected-p (buffer)
  "Return non-nil if BUFFER's popup is the one in use."
  (and (eq (window-buffer (selected-window)) buffer)
       (aside-frame-visible-p buffer)))

(defun aside-frame--make (buffer title)
  "Make a popup frame titled TITLE showing BUFFER."
  (let* ((frame (make-frame `((name . ,title)
                              (aside-buffer . ,buffer)
                              ,@aside-frame-parameters)))
         (window (frame-root-window frame)))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    frame))

(defun aside-frame-show (buffer title)
  "Show BUFFER's popup, titled TITLE, and select it."
  (if (aside-frame--use-frame-p)
      (let ((frame (or (aside-frame-of buffer) (aside-frame--make buffer title))))
        (make-frame-visible frame)
        (raise-frame frame)
        (select-frame-set-input-focus frame)
        (select-window (frame-root-window frame)))
    (select-window
     (display-buffer buffer `((display-buffer-reuse-window
                               display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . ,aside-window-height))))))

(defun aside-frame-reveal (buffer title)
  "Bring BUFFER's popup, titled TITLE, into view if it is hidden."
  (unless (aside-frame-visible-p buffer)
    (aside-frame-show buffer title)))

(defun aside-frame--visible-graphic-frames ()
  "Return how many graphical frames are visible."
  (cl-count-if (lambda (frame)
                 (and (display-graphic-p frame) (eq (frame-visible-p frame) t)))
               (frame-list)))

(defun aside-frame-hide (buffer)
  "Hide BUFFER's popup without losing it."
  (if-let* ((frame (aside-frame-of buffer)))
      (if (<= (aside-frame--visible-graphic-frames) 1)
          (user-error "This is the only visible frame; aside can't hide it")
        (make-frame-invisible frame t))
    (when-let* ((window (get-buffer-window buffer)))
      (if (window-parameter window 'window-side)
          (delete-window window)
        (quit-window nil window)))))

(defun aside-frame-delete (buffer)
  "Delete BUFFER's popup frame, if it has one."
  (when-let* ((frame (aside-frame-of buffer)))
    (if (and (eq (frame-visible-p frame) t)
             (<= (aside-frame--visible-graphic-frames) 1))
        (set-frame-parameter frame 'aside-buffer nil)
      (delete-frame frame t))))

(provide 'aside-frame)
;;; aside-frame.el ends here
