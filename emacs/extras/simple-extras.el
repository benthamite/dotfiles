;;; simple-extras.el --- Extra functionality for the simple feature -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/simle-extras.el
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

(require 'org)
(require 'url-parse)
(require 'url-vars)

;;;; Functions

;;;;; editing

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

;;;;;; words

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

;;;;;;
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

;;;;;; lines

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
  "Like the command `kill-whole-line', but deletes instead of killing."
  (interactive)
  (simple-extras-delete-instead-of-kill (simple-extras-kill-whole-line)))

(defun simple-extras-copy-whole-line ()
  "Like the command `kill-whole-line', but copies instead of killing."
  (interactive)
  (simple-extras-copy-instead-of-kill (simple-extras-kill-whole-line)))

(defun simple-extras-transpose-lines-backward ()
  "Exchange current line and previous line, leaving point between the two."
  (interactive)
  (transpose-lines -1))

;;;;;; sentences

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

;;;;;; paragraphs

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

;;;;;; sexps

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

;;;;;; region

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
    (call-interactively 'simple-extras-delete-whole-line)))

(defun simple-extras-smart-copy-region ()
  "Kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'copy-region-as-kill)
    (call-interactively 'simple-extras-copy-whole-line)))

;;;;;; yank

(defun simple-extras-yank-and-pop ()
  "Yank, then pop the last kill off the ring."
  (interactive)
  (yank)
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))
  (when kill-ring-yank-pointer
    (setq kill-ring-yank-pointer kill-ring))
  (message "Last kill popped off kill-ring."))


;;;;; Other

;; spwhitton.name/blog/entry/transient-mark-mode/
(defun simple-extras-exchange-point-and-mark (arg)
  "Exchange point and mark, but reactivate mark a bit less often.

Specifically, invert the meaning of ARG in the case where
Transient Mark mode is on but the region is inactive."
  (interactive "P")
  (exchange-point-and-mark
   (if (and transient-mark-mode (not mark-active))
       (not arg)
     arg)))

(visible-mode 'toggle)

(defun simple-extras-visible-mode-enhanced (&optional arg)
  "Set `visible-mode' and associated modes.
Toggle the mode if ARG is `toggle' or called interactively. Enable the mode if
ARG is nil, omitted, or a positive number. Disable the mode if ARG is a negative
number."
  (interactive "P")
  (let ((arg (if (or (eq arg 'toggle)
		     (and (null arg) (called-interactively-p 'any)))
		 (if visible-mode 1 -1)
	       (or arg 1))))
    (visible-mode (* arg -1))
    (simple-extras-visible-mode-enhanced-org arg)
    ;; we repeat this to handle weird interaction between `visible-mode' and
    ;; `org-modern-mode'
    (visible-mode (* arg -1))))

(declare-function org-tidy-mode "org-tidy")
(declare-function org-modern-mode "org-modern")
(defun simple-extras-visible-mode-enhanced-org (&optional arg)
  "Set associated `org' modes based on ARG."
  (interactive "P")
  (when (or (derived-mode-p 'org-mode)
	    (derived-mode-p 'org-agenda-mode)
	    (derived-mode-p 'org-msg-mode))
    (when (member 'org-tidy-mode org-mode-hook)
      (org-tidy-mode arg))
    (org-display-inline-images arg)
    (unless (eq major-mode 'org-agenda-mode)
      (org-modern-mode arg))))

(defun simple-extras-count-words-dwim ()
  "Count the number of words in region, if active, otherwise in clipboard.
Either way, save count to kill ring."
  (interactive)
  (if (region-active-p)
      (let ((count (how-many "\\w+" (region-beginning) (region-end))))
	(message "%s words in region" count)
	(kill-new (number-to-string count))
	(message "region has %s words" count))
    (let ((clipboard-text (current-kill 0)))
      (with-temp-buffer
	(insert clipboard-text)
	(let ((clipboard-count (kill-new (format "%d" (count-words-region (point-min) (point-max))))))
	  (message clipboard-count))))))

(defun simple-extras-visual-line-mode-enhanced ()
  "Toggle `visual-line-mode' handling `truncate-lines'."
  (interactive)
  (if visual-line-mode
      (progn
	(visual-line-mode -1)
	(setq truncate-lines t))
    (visual-line-mode)
    (setq truncate-lines nil)))

;;;;; macros

;; From Gonçalo Santos (github.com/weirdNox/dotfiles/blob/master/config/.config/emacs/config.org#helpers)
(defmacro lambda! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

;;;;; indent

;; Adapted from `spacemacs/indent-region-or-buffer'.
(defun simple-extras-indent-dwim ()
  "Indent in a smart way, depending on context.
If a region is selected, indent it. Otherwise, if point is on code block indent
block only, else indent whole buffer."
  (interactive)
  (require 'org)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (if (when (derived-mode-p 'org-mode)
	    (org-in-src-block-p))
	  (let ((org-src-tab-acts-natively t))
	    (org-narrow-to-block)
	    (indent-region (point-min) (point-max) nil)
	    (widen))
	(indent-region (point-min) (point-max) nil)
	(message "Indented buffer.")))
    (whitespace-cleanup)))

;;;;; strip

;; github.com/typester/emacs/blob/master/lisp/url/url-util.el
(defun simple-extras-get-url-at-point (&optional pt)
  "Get the URL closest to point, but don't change position.
Has a preference for looking backward when not directly on a symbol.

If PT is non-nil, start at that position instead of `point'."
  ;; Not at all perfect - point must be right in the name.
  (require 'url-vars)
  (save-excursion
    (if pt (goto-char pt))
    (let (start url)
      (save-excursion
	;; first see if you're just past a filename
	(if (not (eobp))
	    (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
		(progn
		  (skip-chars-backward " \n\t\r({[]})")
		  (if (not (bobp))
		      (backward-char 1)))))
	(if (and (char-after (point))
		 (string-match (eval-when-compile
				 (concat "[" "-%.?@a-zA-Z0-9()_/:~=&" "]"))
			       (char-to-string (char-after (point)))))
	    (progn
	      (skip-chars-backward "-%.?@a-zA-Z0-9()_/:~=&")
	      (setq start (point))
	      (skip-chars-forward "-%.?@a-zA-Z0-9()_/:~=&"))
	  (setq start (point)))
	(setq url (buffer-substring-no-properties start (point))))
      (if (and url (string-match "^(.*)\\.?$" url))
	  (setq url (match-string 1 url)))
      (if (and url (string-match "^URL:" url))
	  (setq url (substring url 4 nil)))
      (if (and url (string-match "\\.$" url))
	  (setq url (substring url 0 -1)))
      (if (and url (string-match "^www\\." url))
	  (setq url (concat "http://" url)))
      (if (and url (not (string-match url-nonrelative-link url)))
	  (setq url nil))
      url)))

(defun simple-extras-strip-url ()
  "Strip URL of unnecessary elements."
  (interactive)
  (unless (simple-extras-get-url-at-point)
    (error "No URL at point."))
  (let* ((url-original (simple-extras-get-url-at-point))
	 (url-stripped (replace-regexp-in-string "\\(?:https?://\\)?\\(?:www.\\)?" "" url-original)))
    (search-backward " ")
    (while (search-forward url-original nil t)
      (replace-match url-stripped nil t))
    (search-backward url-stripped)))

;; TODO: expand
(defun simple-extras-strip-thing-at-point ()
  "Strip thing at point."
  (interactive)
  (cond ((simple-extras-get-url-at-point)
	 (simple-extras-strip-url)))
  (just-one-space 0))

;; save-excursion wasn't restoring point, so using this custom
;; function, from stackoverflow.com/a/24283996/4479455
(defmacro simple-extras-save-excursion (&rest forms)
  (let ((old-point (gensym "old-point"))
	(old-buff (gensym "old-buff")))
    `(let ((,old-point (point))
	   (,old-buff (current-buffer)))
       (prog1
	   (progn ,@forms)
	 (unless (eq (current-buffer) ,old-buff)
	   (switch-to-buffer ,old-buff))
	 (goto-char ,old-point)))))

;; endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun simple-extras-fill-or-unfill-paragraph ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
	 (if (eq last-command 'simple-extras-fill-or-unfill-paragraph)
	     (progn (setq this-command nil)
		    (point-max))
	   fill-column)))
    (call-interactively #'fill-paragraph)))

(declare-function eww-current-url "eww")
(declare-function ebib-get-field-value "ebib")
(declare-function ebib--get-key-at-point "ebib")
(defvar ebib--cur-db)
(defun simple-extras-get-url (url)
  "Get URL from URL, current buffer, or prompt user for it."
  (or url
      (when (eq major-mode 'eww-mode)
	(eww-current-url))
      (when (eq major-mode 'ebib-entry-mode)
	(ebib-get-field-value "url" (ebib--get-key-at-point) ebib--cur-db 'noerror t))
      (read-string "URL: ")))

;;;;; url-parse

(defun simple-extras-string-is-url-p (str)
  "Check if STR is a valid URL."
  (let ((url (url-generic-parse-url str)))
    (and (url-type url) (url-host url))))

;;;;; misc

(defun simple-extras-init-disable-funs (seconds funs)
  "Disable functions in list FUNS after SECONDS."
  (dolist (fun funs)
    (run-with-timer seconds nil (lambda () (advice-add fun :override #'ignore)))))

;; emacs.stackexchange.com/a/24658/32089
(defun simple-extras-advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(declare-function org-extras-narrow-to-entry-and-children "org-extras")
(declare-function ledger-mode-extras-narrow-to-xact "ledger-mode-extras")

;; Modified from endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun simple-extras-narrow-or-widen-dwim ()
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, ledger
transaction, or defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (declare (interactive-only t))
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-narrow-to-block) t))
               (t (org-extras-narrow-to-entry-and-children))))
        ((derived-mode-p 'ledger-mode)
         (ledger-mode-extras-narrow-to-xact))
        (t (narrow-to-defun))))

(provide 'simple-extras)
;;; simple-extras.el ends here
