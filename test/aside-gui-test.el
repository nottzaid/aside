;;; aside-gui-test.el --- Popup frames, on a real display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch Emacs has no frames to show, so these tests need a graphical
;; Emacs.  `make gui' runs them on a virtual X display.

;;; Code:

(require 'aside-test)

(defun aside-gui--wait-visible (frame visible)
  "Wait until FRAME's visibility is VISIBLE."
  (aside-test--wait (lambda () (eq (eq (frame-visible-p frame) t) visible))
                    (if visible "the frame to show" "the frame to hide") 5))

(ert-deftest aside-gui-frame-hides-and-comes-back ()
  "A popup frame hides without being deleted, and comes back as it was."
  (skip-unless (display-graphic-p))
  (aside-test--with-agent (opencode "opencode-session")
    (let* ((popup (aside-test--open dir))
           (frame (aside-frame-of popup)))
      (should (frame-live-p frame))
      (should (eq (selected-frame) frame))
      (should (equal (frame-parameter frame 'name)
                     (format "aside · %s" (file-name-nondirectory dir))))
      (with-current-buffer popup (insert "half a prompt"))
      (with-current-buffer popup (aside-dismiss))
      (aside-gui--wait-visible frame nil)
      (with-temp-buffer (aside-toggle))
      (aside-gui--wait-visible frame t)
      (should (eq (aside-frame-of popup) frame))
      (should (equal (with-current-buffer popup (aside--prompt-text)) "half a prompt"))
      (kill-buffer popup)
      (should-not (frame-live-p frame)))))

(ert-deftest aside-gui-permission-brings-a-hidden-popup-back ()
  "Sending with :wq hides the popup; a permission request shows it again."
  (skip-unless (display-graphic-p))
  (aside-test--with-agent (opencode "opencode-session")
    (let* ((popup (aside-test--open dir))
           (frame (aside-frame-of popup)))
      (aside-test--send (nth 0 aside-test--prompts))
      (aside-test--finish)
      (goto-char (point-max))
      (insert (nth 1 aside-test--prompts))
      (aside-send-and-dismiss)
      (aside-gui--wait-visible frame nil)
      (with-current-buffer popup (aside-test--request))
      (aside-gui--wait-visible frame t)
      (with-current-buffer popup
        (aside-answer "o")
        (aside-test--finish)))))

(ert-deftest aside-gui-prompt-bar-is-unbroken ()
  "On a graphical display the prompt bar is a stretch of colour, not a glyph.
A stretch fills its line, so the bar runs unbroken down a long prompt."
  (skip-unless (display-graphic-p))
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (insert "a prompt long enough to wrap onto several lines of the popup")
      (let ((bar (overlay-get aside--prompt-overlay 'line-prefix)))
        (should (equal (get-text-property 0 'display bar) '(space :width 0.25)))
        (should (memq :inverse-video (get-text-property 0 'face bar))))))
  ;; Text terminals can't draw a quarter of a column, so they keep the glyph.
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (should (string-prefix-p (aside-turn-glyph 'bar)
                             (substring-no-properties (aside--prompt-bar))))))

(defun aside-gui-run ()
  "Run these tests, print the results and exit with their status."
  (let ((stats (ert-run-tests-batch "\\`aside-gui-")))
    (with-current-buffer "*Messages*"
      (princ (buffer-string) #'external-debugging-output))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'aside-gui-test)
;;; aside-gui-test.el ends here
