;;; simple-extras.el --- Extra functionality for the simple feature -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/lisp/simle-extras.el
;; Version: 0.1

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

;; Extra functionality for the `simple' feature.

;;; Code:

;;;; User options

;;;; Variables

;;;; Functions

(defmacro simple-extras-delete-instead-of-kill (&rest body)
  "Replace `kill-region' with `delete-region' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end)
                (delete-region beg end))))
     ,@body))

(defmacro simple-extras-copy-instead-of-kill (&rest body)
  "Replace `kill-region' with `kill-ring-save' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end)
                (kill-ring-save beg end)
                (setq this-command 'kill-region))))
     ,@body))

(defun simple-extras-kill-whole-thing (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun simple-extras-delete-word (&optional arg)
  "Like `kill-word', but deletes instead of killing.
With argument ARG, do this that many times."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (kill-word arg)))

(defun simple-extras-backward-delete-word (&optional arg)
  "Like `backward-kill-word', but deletes instead of killing.
With argument ARG, do this that many times."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (backward-kill-word arg)))

(defun simple-extras-copy-word (&optional arg)
  "Like `kill-word', but copies instead of killing.
With argument ARG, do this that many times."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (kill-word arg)))

;; The macro wasn't working for `backward-kill-word', so using a custom function.
(defun simple-extras-backward-copy-word ()
  "Like `backward-kill-word', but copies instead of killing."
  (interactive)
  (copy-region-as-kill (point) (progn (backward-word) (point))))

(defun simple-extras-kill-whole-word ()
  "Kill the word at point."
  (interactive)
  (simple-extras-kill-whole-thing 'word))

(defun simple-extras-delete-whole-word ()
  "Like `kill-whole-word', but deletes instead of killing."
  (interactive)
  (simple-extras-delete-instead-of-kill (simple-extras-kill-whole-word)))

(defun simple-extras-copy-whole-word ()
  "Like `kill-whole-word', but copies instead of killing.
With argument ARG, do this that many times."
  (interactive)
  (simple-extras-copy-instead-of-kill (simple-extras-kill-whole-word)))

(defun simple-extras-transpose-words-backward ()
  "Interchange words around point, leaving point at beginning."
  (interactive)
  (transpose-words -1))

(defun simple-extras-backward-zap-to-char (arg char &optional interactive)
  "Kill backward up to and including ARGth occurrence of CHAR.
When run interactively, the argument INTERACTIVE is non-nil.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'.
If called interactively, do a case sensitive search if CHAR
is an upper-case character."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char-from-minibuffer "Zap to char: "
						nil 'read-char-history)
		     t))
  (zap-to-char (* -1 arg) char interactive))

(defun simple-extras-zap-copy-to-char (arg char)
  "Copy up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char-from-minibuffer "Zap to char: "
                                                nil 'read-char-history)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (copy-region-as-kill (point) (progn
                                 (search-forward (char-to-string char) nil nil arg)
                                 (point))))

(defun simple-extras-backward-zap-copy-to-char ()
  "Copy backward up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'."
  (interactive)
  (simple-extras-zap-copy-to-char -1 (read-char-from-minibuffer "Zap to char: "
								nil 'read-char-history)))

(defun simple-extras-zap-delete-to-char (arg char)
  "Copy up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char-from-minibuffer "Zap to char: "
                                                nil 'read-char-history)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (delete-region (point) (progn
                           (search-forward (char-to-string char) nil nil arg)
                           (point))))

(defun simple-extras-backward-zap-delete-to-char ()
  "Copy backward up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'."
  (interactive)
  (simple-extras-zap-delete-to-char -1 (read-char-from-minibuffer "Zap to char: "
								  nil 'read-char-history)))

(defun simple-extras-transpose-chars-backward ()
  "Interchange characters around point, moving backward one character."
  (interactive)
  (transpose-chars -1))

(defun simple-extras-delete-line (&optional arg)
  "Like `kill-line', but deletes instead of killing.
With prefix argument ARG, kill that many lines from point."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (kill-line arg)))

(defun simple-extras-backward-delete-line ()
  "Like `backward-kill-line', but deletes instead of killing."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (kill-line 0)))

(defun simple-extras-copy-line (&optional arg)
  "Like `kill-line', but copies instead of killing.
With prefix argument ARG, copy that many lines from point."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (kill-line arg)))

(defun simple-extras-backward-copy-line ()
  "Like `backward-kill-line', but copies instead of killing."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (kill-line 0)))

(defun simple-extras-kill-whole-line ()
  "Kill the line at point."
  (interactive)
  (simple-extras-kill-whole-thing 'line))

(defun simple-extras-delete-whole-line ()
  "Like `kill-whole-line', but deletes instead of killing."
  (interactive)
  (simple-extras-delete-instead-of-kill (simple-extras-kill-whole-line)))

(defun simple-extras-copy-whole-line ()
  "Like `kill-whole-line', but copies instead of killing."
  (interactive)
  (simple-extras-copy-instead-of-kill (simple-extras-kill-whole-line)))

(defun simple-extras-extras-transpose-lines-backward ()
  "Exchange current line and previous line, leaving point between the two."
  (interactive)
  (transpose-lines -1))

(defun simple-extras-delete-sentence (&optional arg)
  "Like `kill-sentence', but deletes instead of killing.
With ARG, repeat; negative ARG -N means delete back to Nth start of
sentence."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (kill-sentence arg)))

(defun simple-extras-backward-delete-sentence (&optional arg)
  "Like `backward-kill-sentence', but deletes instead of killing.
With ARG, repeat, or delete forward to Nth end of sentence if
negative ARG -N."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (backward-kill-sentence arg)))

(defun simple-extras-copy-sentence (&optional arg)
  "Like `kill-sentence', but copies instead of killing.
With ARG, repeat; negative ARG -N means copy back to Nth start of
sentence."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (kill-sentence arg)))

(defun simple-extras-backward-copy-sentence (&optional arg)
  "Like `backward-kill-sentence', but copies instead of killing.
With ARG, repeat, or copy forward to Nth end of sentence if
negative ARG -N."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (backward-kill-sentence arg)))

(defun simple-extras-kill-whole-sentence ()
  "Kill the sentence at point."
  (interactive)
  (simple-extras-kill-whole-thing 'sentence))

(defun simple-extras-delete-whole-sentence ()
  "Like `kill-whole-sentence', but deletes instead of killing."
  (interactive)
  (simple-extras-delete-instead-of-kill (simple-extras-kill-whole-sentence)))

(defun simple-extras-copy-whole-sentence ()
  "Like `kill-whole-sentence', but copies instead of killing."
  (interactive)
  (simple-extras-copy-instead-of-kill (simple-extras-kill-whole-sentence)))

(defun simple-extras-transpose-sentences-backward ()
  "Interchange the current sentence with the previous one."
  (interactive)
  (transpose-sentences -1))

;; the functions below are derivatives of functions in `paragraphs.el' so maybe
;; they should be moved to another extra package there
(defun simple-extras-delete-paragraph (&optional arg)
  "Like `kill-paragraph', but deletes instead of killing.
With ARG N, delete forward to Nth end of paragraph;
negative ARG -N means delete backward to Nth start of paragraph."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (kill-paragraph arg)))

(defun simple-extras-backward-delete-paragraph (&optional arg)
  "Like `backward-kill-paragraph', but deletes instead of killing.
With ARG N, delete back to Nth start of paragraph;
negative ARG -N means delete forward to Nth end of paragraph."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (backward-kill-paragraph arg)))

(defun simple-extras-copy-paragraph (&optional arg)
  "Like `kill-paragraph', but copies instead of killing.
With ARG N, copy forward to Nth end of paragraph;
negative ARG -N means copy backward to Nth start of paragraph."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (kill-paragraph arg)))

(defun simple-extras-backward-copy-paragraph (&optional arg)
  "Like `backward-kill-paragraph', but copies instead of killing.
With ARG N, copy back to Nth start of paragraph;
negative ARG -N means copy forward to Nth end of paragraph."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (backward-kill-paragraph arg)))

(defun simple-extras-kill-whole-paragraph ()
  "Kill the paragraph at point."
  (interactive)
  (simple-extras-kill-whole-thing 'paragraph))

(defun simple-extras-delete-whole-paragraph ()
  "Like `kill-whole-paragraph', but deletes instead of killing."
  (interactive)
  (simple-extras-delete-instead-of-kill (simple-extras-kill-whole-paragraph)))

(defun simple-extras-copy-whole-paragraph ()
  "Like `kill-whole-paragraph', but copies instead of killing."
  (interactive)
  (simple-extras-copy-instead-of-kill (simple-extras-kill-whole-paragraph)))

(defun simple-extras-transpose-paragraphs-backward ()
  "Interchange the current paragraph with the previous one."
  (interactive)
  (transpose-paragraphs -1))

;; ;; the functions below are derivatives of functions in `lisp.el' so maybe
;; ;; they should be moved to another extra package there
(defun simple-extras-delete-sexp (&optional arg)
  "Like `kill-sexp', but deletes instead of killing.
With ARG, delete that many sexps after point.
Negative arg -N means delete N sexps before point."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (kill-sexp arg)))

(defun simple-extras-backward-delete-sexp (&optional arg)
  "Like `backward-kill-sexp', but deletes instead of killing.
With ARG, delete that many sexps before point.
Negative arg -N means delete N sexps after point."
  (interactive "p")
  (simple-extras-delete-instead-of-kill (backward-kill-sexp arg)))

(defun simple-extras-copy-sexp (&optional arg)
  "Like `kill-sexp', but copies instead of killing.
With ARG, copy that many sexps after point.
Negative arg -N means copy N sexps before point."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (kill-sexp arg)))

(defun simple-extras-backward-copy-sexp (&optional arg)
  "Like `backward-kill-sexp', but copies instead of killing.
With ARG, copy that many sexps before point.
Negative arg -N means copy N sexps after point."
  (interactive "P")
  (simple-extras-copy-instead-of-kill (backward-kill-sexp arg)))

(defun simple-extras-kill-whole-sexp ()
  "Kill the sexp at point."
  (interactive)
  (simple-extras-kill-whole-thing 'sexp))

(defun simple-extras-delete-whole-sexp ()
  "Like `kill-whole-sexp', but deletes instead of killing."
  (interactive)
  (simple-extras-delete-instead-of-kill (simple-extras-kill-whole-sexp)))

(defun simple-extras-copy-whole-sexp ()
  "Like `kill-whole-sexp', but copies instead of killing."
  (interactive)
  (simple-extras-copy-instead-of-kill (simple-extras-kill-whole-sexp)))

(defun simple-extras-transpose-sexps-backward ()
  "Like `transpose-sexps', but in reverse order."
  (interactive)
  (transpose-sexps -1))

(defun simple-extras-smart-kill-region ()
  "Kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun simple-extras-smart-delete-region ()
  "Kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-region)
    (call-interactively 'ps/delete-whole-line)))

(defun simple-extras-smart-copy-region ()
  "Kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'copy-region-as-kill)
    (call-interactively 'ps/copy-whole-line)))

(provide 'simple-extras)
;;; simple-extras.el ends here
