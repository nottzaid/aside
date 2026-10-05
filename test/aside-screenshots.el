;;; aside-screenshots.el --- Draw the README's screenshots  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nottzaid
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Stages two moments of an exchange in a real popup frame and saves
;; them as PNG files under docs/: the agent at work, asking permission,
;; and its finished answer.  Run it with `make screenshots', which
;; starts a graphical Emacs on a virtual X display, so nothing appears
;; on your screen.  Look at the images after changing how popups look.

;;; Code:

(require 'aside)

(declare-function x-export-frames "xfns.c")

(defconst aside-shots--docs
  (expand-file-name "../docs/" (file-name-directory (or load-file-name buffer-file-name)))
  "Where the screenshots go.")

(defun aside-shots--save (name)
  "Save the selected frame as docs/NAME.png."
  (message nil)
  (redisplay t)
  (sit-for 0.2)
  (redisplay t)
  (let ((coding-system-for-write 'binary))
    (with-temp-file (expand-file-name (concat name ".png") aside-shots--docs)
      (set-buffer-multibyte nil)
      (insert (x-export-frames (selected-frame) 'png)))))

(defun aside-shots ()
  "Draw the screenshots, then exit."
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'default nil :height 140)
  (make-directory aside-shots--docs t)
  (let ((buffer (with-current-buffer (get-buffer-create "*aside: tally (Claude Code)*")
                  (aside-mode)
                  (setq aside--agent 'claude
                        aside--root "/home/you/src/tally"
                        aside--state 'ready
                        aside--usage '(:used 41800 :size 200000)
                        aside--options
                        '((:id "mode" :category "mode" :currentValue "default"
                               :options ((:value "default" :name "Manual")))
                          (:id "model" :category "model" :currentValue "sonnet"
                               :options ((:value "sonnet" :name "Sonnet 5")))))
                  (aside--compose)
                  (current-buffer))))
    (aside--show buffer)
    (insert "`tally report` stops one day short at the end of a month. Find out why and fix it.")
    (setq aside--queued nil)
    (aside--begin-turn (aside--prompt-text))
    (dolist (update
             '((:sessionUpdate "agent_thought_chunk"
                :content (:type "text" :text "The report probably builds its range of days with an exclusive end. I should read report.py first."))
               (:sessionUpdate "tool_call" :toolCallId "1" :title "Read report.py" :kind "read" :status "completed")
               (:sessionUpdate "tool_call" :toolCallId "2" :title "grep -n \"range(\" report.py" :kind "search" :status "completed")))
      (aside--update update))
    (aside--draw
     (aside-turn-add-request
      aside--turn
      (list :reply #'ignore
            :toolCall (list :title "Edit report.py" :kind "edit"
                            :content (list (list :type "diff" :path "/home/you/src/tally/report.py"
                                                 :oldText "    for day in range(start.day, end.day):\n"
                                                 :newText "    for day in range(start.day, end.day + 1):\n")))
            :options '((:optionId "allow" :kind "allow_once" :name "Yes")
                       (:optionId "always" :kind "allow_always" :name "Yes, and don't ask again")
                       (:optionId "reject" :kind "reject_once" :name "No")))))
    (aside-request-mode 1)
    (aside-shots--save "working")
    (aside--answer (car (aside-turn-requests aside--turn)) "allow")
    (aside--update '(:sessionUpdate "tool_call" :toolCallId "3" :title "Edit report.py" :kind "edit" :status "completed"
                     :locations ((:path "/home/you/src/tally/report.py"))))
    (aside--update '(:sessionUpdate "tool_call" :toolCallId "4" :title "pytest" :kind "execute" :status "completed"))
    (aside--update
     '(:sessionUpdate "agent_message_chunk" :messageId "m"
       :content (:type "text" :text "The loop in `report.py` used `range(start.day, end.day)`, which stops *before* its end. It now includes the last day:\n\n```python\nfor day in range(start.day, end.day + 1):\n    totals[day] = sum_day(entries, day)\n```\n\nThe tests pass, and the March report now **ends on the 31st**.")))
    (setf (aside-turn-started aside--turn) (- (float-time) 38))
    (aside--finish-turn "end_turn")
    (aside-shots--save "answer"))
  (kill-emacs 0))

(provide 'aside-screenshots)
;;; aside-screenshots.el ends here
