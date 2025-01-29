;;; bbdb-extras.el --- Additional functionality for bbdb -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/bbdb-extras.el
;; Version: 0.2
;; Package-Requires: ((bbdb "3.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Additional functionality for `bbdb'.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)

;;;; Patched functions

;; open URL directly, without prompting for confirmation
(defun bbdb-extras-bbdb-browse-url (records &optional which)
  "Brwose URLs stored in the `url' field of RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Prefix WHICH specifies which URL in field `url' is used (starting from 0).
Default is the first URL."
  (interactive (list (bbdb-get-records "Visit (URL): ")
                     (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (unless which (setq which 0))
  (dolist (record (bbdb-record-list records))
    (let ((url (bbdb-record-xfield-split record 'url)))
      (when url
        (setq url (nth which url))
        (unless (string= "" url)
          (browse-url url))))))

(advice-add 'bbdb-browse-url :override #'bbdb-extras-bbdb-browse-url)

;; do not split windows
(defun bbdb-extras-bbdb-pop-up-window (&optional select horiz-p)
  "Display *BBDB* buffer by popping up a new window.
Finds the largest window on the screen, splits it, displaying the
*BBDB* buffer in the bottom `bbdb-pop-up-window-size' lines (unless
the *BBDB* buffer is already visible, in which case do nothing.)
Select this window if SELECT is non-nil.

If `bbdb-mua-pop-up' is `'horiz', and the first window matching
the predicate HORIZ-P is wider than the car of `bbdb-horiz-pop-up-window-size'
then the window will be split horizontally rather than vertically."
  (let ((buffer (get-buffer bbdb-buffer-name)))
    (unless buffer
      (error "No %s buffer to display" bbdb-buffer-name))
    (cond ((let ((window (get-buffer-window buffer t)))
             ;; We already have a BBDB window so that at most we select it
             (and window
                  (or (not select) (select-window window)))))

          ;; try horizontal split
          ((and (eq bbdb-mua-pop-up 'horiz)
                horiz-p
                (>= (frame-width) (car bbdb-horiz-pop-up-window-size))
                (let ((window-list (window-list))
                      (b-width (cdr bbdb-horiz-pop-up-window-size))
                      (search t) s-window)
                  (while (and (setq s-window (pop window-list))
                              (setq search (not (funcall horiz-p s-window)))))
                  (unless (or search (<= (window-width s-window)
                                         (car bbdb-horiz-pop-up-window-size)))
                    (condition-case nil ; `split-window' might fail
                        (let ((window (split-window
                                       s-window
                                       (if (integerp b-width)
                                           (- (window-width s-window) b-width)
                                         (round (* (- 1 b-width) (window-width s-window))))
                                       t))) ; horizontal split
                          (set-window-buffer window buffer)
                          (cond (bbdb-dedicated-window
                                 (set-window-dedicated-p window bbdb-dedicated-window))
                                ((fboundp 'display-buffer-record-window) ; GNU Emacs >= 24.1
                                 (set-window-prev-buffers window nil)
                                 (display-buffer-record-window 'window window buffer)))
                          (if select (select-window window))
                          t)
                      (error nil))))))

          ((eq t bbdb-pop-up-window-size)
           (bbdb-pop-up-window-simple buffer select))

          (t ;; vertical split
           (let* ((window (selected-window))
                  (window-height (window-height window)))
             ;; find the tallest window...
             (mapc (lambda (w)
                     (let ((w-height (window-height w)))
                       (if (> w-height window-height)
                           (setq window w window-height w-height))))
                   (window-list))
             (condition-case nil
		 (set-window-buffer window buffer) ; might fail
	       (cond (bbdb-dedicated-window
		      (set-window-dedicated-p window bbdb-dedicated-window))
		     ((and (fboundp 'display-buffer-record-window) ; GNU Emacs >= 24.1
                           (not (eql bbdb-pop-up-window-size 1.0)))
		      (set-window-prev-buffers window nil)
		      (display-buffer-record-window 'window window buffer)))
	       (if select (select-window window))))))))

(advice-add 'bbdb-pop-up-window :override #'bbdb-extras-bbdb-pop-up-window)

;; the two functions below replace the corresponding "non-quick"
;; native functions. they prompt for a name only.
(defun bbdb-extras-bbdb-create-quick (record)
  "Add a new RECORD to BBDB. Prompt for name only."
  (interactive (list (bbdb-extras-bbdb-read-record-quick current-prefix-arg)))
  (bbdb-change-record record)
  (bbdb-display-records (list record)))

(defun bbdb-extras-bbdb-read-record-quick (&optional first-and-last)
  "Read and return a new BBDB record.
To be used with `bbdb-extras-bbdb-create-quick'. FIRST-AND-LAST controls the
reading mode; see the `bbdb-read-name' docstring for details."
  (bbdb-editable)
  (let ((record (bbdb-empty-record)))
    (let (name)
      (bbdb-error-retry
       (setq name (bbdb-read-name first-and-last))
       (bbdb-check-name name))
      (setf (bbdb-record-firstname record) (car name))
      (setf (bbdb-record-lastname record) (cdr name)))
    record))

(defun bbdb-extras-bbdb-delete-field-or-record-no-confirm ()
  "Delete the current field or record without confirmation."
  (interactive)
  (bbdb-delete-field-or-record (bbdb-do-records) (bbdb-current-field) t))

(provide 'bbdb-extras)
;;; bbdb-extras.el ends here
