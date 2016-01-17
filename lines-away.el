;;; lines-away.el --- display number of lines away from point in the left margin -*- lexical-binding: t -*-

;; Author: Siavash Sajjadi <sia.s.saj@gmail.com>
;; Maintainer: sia.s.saj@gmail.com
;; Keywords: convenience
;; Version: 0.9x

;; lines-away.el draws extremely heavily from linum.el 0.9x by Markus Triska
;; which is released under GNU GPL 3

;; This file is part of lines-away.

;; lines-away is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lines-away is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lines-away.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Display number of lines away from point for each line in the current buffer.
;;
;; Toggle display of line numbers with M-x lines-away-mode.  To enable
;; line numbering in all buffers, use M-x global-lines-away-mode.

;;; Code:

(defconst lines-away-version "0.9x")

(defvar lines-away-overlays nil "Overlays used in this buffer.")
(defvar lines-away-available nil "Overlays available for reuse.")
(defvar lines-away-before-numbering-hook nil
  "Functions run in each buffer before line numbering starts.")

(mapc #'make-variable-buffer-local '(lines-away-overlays lines-away-available))

(defgroup lines-away nil
  "Show line numbers in the left margin."
  :group 'convenience)

(defcustom lines-away-format 'dynamic
  "Format used to display line numbers.
Either a format string like \"%7d\", `dynamic' to adapt the width
as needed, or a function that is called with a line number as its
argument and should evaluate to a string to be shown on that line.
See also `lines-away-before-numbering-hook'."
  :group 'lines-away
  :type '(choice (string :tag "Format string")
                 (const :tag "Dynamic width" dynamic)
                 (function :tag "Function")))

(defface lines-away
  '((t :inherit (shadow default)))
  "Face for displaying line numbers in the display margin."
  :group 'lines-away)

(defcustom lines-away-eager t
  "Whether line numbers should be updated after each command.
The conservative setting `nil' might miss some buffer changes,
and you have to scroll or press \\[recenter-top-bottom] to update the numbers."
  :group 'lines-away
  :type 'boolean)

(defcustom lines-away-delay nil
  "Delay updates to give Emacs a chance for other changes."
  :group 'lines-away
  :type 'boolean)

;;;###autoload
(define-minor-mode lines-away-mode
  "Toggle display of line numbers in the left margin (Lines-Away mode).
With a prefix argument ARG, enable Lines-Away mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Lines-Away mode is a buffer-local minor mode."
  :lighter ""                           ; for desktop.el
  (if lines-away-mode
      (progn
        (if lines-away-eager
            (add-hook 'post-command-hook (if lines-away-delay
                                             'lines-away-schedule
                                           'lines-away-update-current) nil t)
          (add-hook 'after-change-functions 'lines-away-after-change nil t))
        (add-hook 'window-scroll-functions 'lines-away-after-scroll nil t)
        ;; Using both window-size-change-functions and
        ;; window-configuration-change-hook seems redundant. --Stef
        ;; (add-hook 'window-size-change-functions 'lines-away-after-size nil t)
        (add-hook 'change-major-mode-hook 'lines-away-delete-overlays nil t)
        (add-hook 'window-configuration-change-hook
                  ;; FIXME: If the buffer is shown in N windows, this
                  ;; will be called N times rather than once.  We should use
                  ;; something like lines-away-update-window instead.
                  'lines-away-update-current nil t)
        (lines-away-update-current))
    (remove-hook 'post-command-hook 'lines-away-update-current t)
    (remove-hook 'post-command-hook 'lines-away-schedule t)
    ;; (remove-hook 'window-size-change-functions 'lines-away-after-size t)
    (remove-hook 'window-scroll-functions 'lines-away-after-scroll t)
    (remove-hook 'after-change-functions 'lines-away-after-change t)
    (remove-hook 'window-configuration-change-hook 'lines-away-update-current t)
    (remove-hook 'change-major-mode-hook 'lines-away-delete-overlays t)
    (lines-away-delete-overlays)))

;;;###autoload
(define-globalized-minor-mode global-lines-away-mode lines-away-mode lines-away-on)

(defun lines-away-on ()
  (unless (minibufferp)
    (lines-away-mode 1)))

(defun lines-away-delete-overlays ()
  "Delete all overlays displaying line numbers for this buffer."
  (mapc #'delete-overlay lines-away-overlays)
  (setq lines-away-overlays nil)
  (dolist (w (get-buffer-window-list (current-buffer) nil t))
    (set-window-margins w 0 (cdr (window-margins w)))))

(defun lines-away-update-current ()
  "Update line numbers for the current buffer."
  (lines-away-update (current-buffer)))

(defun lines-away-update (buffer)
  "Update line numbers for all windows displaying BUFFER."
  (with-current-buffer buffer
    (when lines-away-mode
      (setq lines-away-available lines-away-overlays)
      (setq lines-away-overlays nil)
      (save-excursion
        (mapc #'lines-away-update-window
              (get-buffer-window-list buffer nil 'visible)))
      (mapc #'delete-overlay lines-away-available)
      (setq lines-away-available nil))))

(defun lines-away-update-window (win)
  "Update line numbers for the portion visible in window WIN."

  (let ((original-line (progn
                (goto-char (window-point win))
                (line-number-at-pos)))
        (line (progn
                (goto-char (window-start win))
                (line-number-at-pos)))
        (limit (window-end win t))
        (fmt (cond ((stringp lines-away-format) lines-away-format)
                   ((eq lines-away-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat "%" (number-to-string w) "d")))))
        (width 0))
    (run-hooks 'lines-away-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (< (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt (abs (- original-line line))line) 'face 'lines-away)
                    (funcall lines-away-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
				 (overlay-get o 'lines-away-str) str)
                            (unless (memq o lines-away-overlays)
                              (push o lines-away-overlays))
                            (setq lines-away-available (delq o lines-away-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null lines-away-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop lines-away-available) (point) (point)))))
            (push ov lines-away-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'lines-away-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (set-window-margins win width (cdr (window-margins win)))))

(defun lines-away-after-change (beg end _len)
  ;; update overlays on deletions, and after newlines are inserted
  (when (or (= beg end)
            (= end (point-max))
            (string-match-p "\n" (buffer-substring-no-properties beg end)))
    (lines-away-update-current)))

(defun lines-away-after-scroll (win _start)
  (lines-away-update (window-buffer win)))

;; (defun lines-away-after-size (frame)
;;   (lines-away-after-config))

(defun lines-away-schedule ()
  ;; schedule an update; the delay gives Emacs a chance for display changes
  (run-with-idle-timer 0 nil #'lines-away-update-current))

;; (defun lines-away-after-config ()
;;   (walk-windows (lambda (w) (lines-away-update (window-buffer w))) nil 'visible))

(defun lines-away-unload-function ()
  "Unload the Lines-Away library."
  (global-lines-away-mode -1)
  ;; continue standard unloading
  nil)

(provide 'lines-away)

;;; lines-away.el ends here
