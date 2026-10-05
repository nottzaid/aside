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
        ;; The cursor waits on the first option; RET chooses it.
        (should (equal (plist-get (get-text-property (window-point (frame-root-window frame))
                                                     'aside-option)
                                  :name)
                       "Allow once"))
        (with-selected-window (frame-root-window frame)
          (execute-kbd-macro (kbd "RET")))
        (should-not (aside-turn-requests aside--turn))
        (aside-test--finish)))))

(ert-deftest aside-gui-prompt-bar-is-unbroken ()
  "In a popup frame the prompt bar is drawn in the fringe.
A fringe bitmap fills each line to its full height, so the bar runs
unbroken even past keycaps or symbols from taller fonts."
  (skip-unless (display-graphic-p))
  (aside-test--with-agent (opencode "opencode-session")
    (with-current-buffer (aside-test--open dir)
      (should (> (or (car (window-fringes)) 0) 0))
      (let ((bar (overlay-get aside--prompt-overlay 'line-prefix)))
        (should (equal (get-text-property 0 'display bar)
                       '(left-fringe aside-bar aside-prompt-bar-fringe))))
      ;; The hint's second line carries its own piece of the bar.
      (should (string-search "aside-bar" (format "%S" (overlay-get aside--placeholder
                                                                   'after-string))))))
  ;; Without a fringe, a thin stretch of colour; on a terminal, a glyph.
  (let ((aside-display 'window))
    (should (equal (get-text-property 0 'display (aside--prompt-bar)) '(space :width 0.25))))
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
    (should (string-prefix-p (aside-turn-glyph 'bar)
                             (substring-no-properties (aside--prompt-bar))))))

(ert-deftest aside-gui-list-takes-the-popup-window ()
  "A list of choices shows in the popup's own frame, answers real keys,
and gives the frame back to the popup as it was."
  (skip-unless (display-graphic-p))
  (aside-test--with-agent (opencode "opencode-session")
    (let* ((popup (aside-test--open dir))
           (frame (aside-frame-of popup))
           (frames (length (frame-list)))
           chosen)
      (with-current-buffer popup (insert "half a prompt"))
      (with-current-buffer popup
        (aside--choose "Mode" '(("Build" build) ("Plan" plan "Read only")) :current 'build
                       :then (lambda (value) (setq chosen value))))
      (should (= (length (frame-list)) frames))
      (should (eq (window-buffer (frame-root-window frame)) (aside-test--list)))
      (with-selected-window (frame-root-window frame)
        (execute-kbd-macro (kbd "C-n RET")))
      (should (eq chosen 'plan))
      (should (eq (window-buffer (frame-root-window frame)) popup))
      (should (equal (with-current-buffer popup (aside--prompt-text)) "half a prompt")))))

(ert-deftest aside-gui-resume-moves-the-popup-that-has-the-session ()
  "When another popup has the chosen session open, it moves into this
popup's frame, its own frame goes, and no second frame stays up."
  (skip-unless (display-graphic-p))
  (let* ((dir (aside-test--project))
         (aside-agents (list (aside-test--fake 'claude "claude-load" dir)))
         (aside-default-agent 'claude)
         (aside-notify nil)
         (aside--connections nil)
         (aside--last-agent nil)
         (aside--sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (let* ((here (let ((default-directory (file-name-as-directory dir)))
                       (aside)
                       (current-buffer)))
               (frame (aside-frame-of here))
               (other (aside--create 'claude dir)))
          (aside-test--wait (lambda () (with-current-buffer here
                                         (memq aside--state '(failed ready))))
                            "the popup")
          (aside--show other)
          (let ((other-frame (aside-frame-of other)))
            (aside--show here)
            (aside-test--resume-into here other)
            (should-not (frame-live-p other-frame)))
          (should-not (buffer-live-p here))
          (should (eq (aside-frame-of other) frame))
          (should (eq (window-buffer (frame-root-window frame)) other))
          (should (eq (selected-frame) frame)))
      (dolist (buffer (aside--popups)) (kill-buffer buffer))
      (pcase-dolist (`(,_ . ,conn) aside--connections) (aside-acp-stop conn))
      (delete-directory dir t))))

(defun aside-gui-run ()
  "Run these tests, print the results and exit with their status."
  (let ((stats (ert-run-tests-batch "\\`aside-gui-")))
    (with-current-buffer "*Messages*"
      (princ (buffer-string) #'external-debugging-output))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'aside-gui-test)
;;; aside-gui-test.el ends here
