;;; org-extras.el --- Extensions for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Pablo Stafforini

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.0.1
;; Homepage: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-extras.el
;; Keywords: org-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for `org-mode'.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'el-patch)

;;;; User options

(defgroup org-extras ()
  "Extensions for `org'."
  :group 'org)

(defcustom org-extras-agenda-files-excluded
  (list
   paths-file-tlon-tareas-leo
   paths-file-tlon-tareas-fede)
  "Files to exclude from `org-agenda'.
I have to exclude these files because otherwise extraneous information shows up
in my agenda, such as TODOs and time logs. These files lack the `property' tag
but the may still otherwise be included if they have been modified recently (see
the function `vulpea-agenda-files-update')."
  :type 'list
  :group 'org-extras)

(defcustom org-extras-id-auto-add-excluded-directories
  (list paths-dir-dropbox-tlon-fede
	paths-dir-dropbox-tlon-leo)
  "Directories to exclude from `org-extras-id-auto-add-ids-to-headings-in-file'."
  :type 'list
  :group 'org-extras)

(defcustom org-extras-id-auto-add-excluded-files
  (list paths-file-orb-noter-template)
  "Files to exclude from `org-extras-id-auto-add-ids-to-headings-in-file'."
  :type 'list
  :group 'org-extras)

(defcustom org-extras-bbdb-anniversaries-heading
  "2A37A3CC-2A11-4933-861B-48B129B9EA2D"
  "Heading in `calendar.org' that contains the BBDB anniversaries.
Set to nil to disable display of BBDB anniversaries in agenda."
  :type 'string
  :group 'org-extras)

;;;; Functions

;;;;; org

(defun org-extras-set-todo-properties ()
  "Set priority and effort."
  (interactive)
  (org-priority)
  (org-set-effort))

(defun org-extras-url-dwim ()
  "docstring"
  (interactive)
  (require 'url-util)
  (cond
   ((url-get-url-at-point)
    (kill-new (url-get-url-at-point)))
   ((org-extras-link-get-url-at-point)
    (kill-new (org-extras-link-get-thing-at-point 1)))))

;; Adapted from lists.gnu.org/archive/html/emacs-orgmode/2011-06/msg00716.html
(defun org-extras-link-get-thing-at-point (arg)
  "When point is on org link, extract object, as defined by ARG."
  (when (org-in-regexp org-link-bracket-re 1)
    (kill-new (org-link-unescape (match-string-no-properties arg)))))

(defun org-extras-link-get-link-at-point ()
  "When point is on org link, extract link.
The extracted link includes both the URL and the description."
  (interactive)
  (org-extras-link-get-thing-at-point 0))

(defun org-extras-link-get-url-at-point ()
  "When point is on org link, extract URL."
  (interactive)
  (org-extras-link-get-thing-at-point 1))

(defun org-extras-link-get-description-at-point ()
  "When point is on org link, extract description."
  (interactive)
  (org-extras-link-get-thing-at-point 2))

;; emacs.stackexchange.com/a/10714/32089
(defun org-extras-remove-link ()
  "Replace an org link by its description or, if empty, its address."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
	(let ((remove (list (match-beginning 0) (match-end 0)))
	      (description
	       (if (match-end 2)
		   (match-string-no-properties 2)
		 (match-string-no-properties 1))))
	  (apply 'delete-region remove)
	  (insert description)))))

(defun org-extras-insert-todo-subheading-after-body ()
  "Insert an org subheading at the end of the current subtree."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (outline-hide-subtree)
    (outline-show-entry)
    (goto-char (point-max))
    (org-beginning-of-line)
    (org-insert-todo-heading nil t)
    (org-do-demote)))

(defun org-extras-insert-subheading ()
  "Insert an org subheading at the end of the current subtree.
If the subtree contains other subheadings, insert it above them."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (outline-next-visible-heading 1)
    (left-char)
    (newline)
    (org-insert-heading)
    (org-do-demote)))

;; Adapted from hungyi.net/posts/org-mode-subtree-contents
(defun org-extras-get-heading-contents ()
  "Get the content text of the heading at point and add it to the `kill-ring'.
Excludes the heading itself and any child subtrees."
  (if (org-before-first-heading-p)
      (message "Not in or on an org heading")
    (save-excursion
      ;; If inside heading contents, move the point back to the heading
      ;; otherwise `org-agenda-get-some-entry-text' won't work.
      (unless (org-at-heading-p) (org-previous-visible-heading 1))
      (let ((contents (substring-no-properties
		       (org-agenda-get-some-entry-text
			(point-marker)
			most-positive-fixnum))))
	contents))))

(defun org-extras-copy-heading-contents ()
  "Copy the content text of the heading at point to the `kill-ring'."
  (interactive)
  (let ((contents (org-extras-get-heading-contents)))
    (if (string= contents "")
	(message "Heading is empty.")
      (message "Copied heading")
      (kill-new contents))))

(defun org-extras-copy-heading-name ()
  "Copy the name of the heading at point to the `kill-ring'."
  (interactive)
  (kill-new (org-entry-get nil "ITEM")))

;; reddit.com/r/emacs/comments/e4jnlj/how_to_create_a_word_counter_that_counts_words_in/f9e3796
(defun org-extras-count-words ()
  "Count words in region if active; otherwise count words in current subtree."
  (interactive)
  (if (use-region-p)
      (funcall-interactively #'count-words-region (region-beginning) (region-end))
    (org-with-wide-buffer
     (cl-loop for (lines words characters)
	      in (org-map-entries
		  (lambda ()
		    (unpackaged/org-forward-to-entry-content 'unsafe)
		    (let ((end (org-entry-end-position)))
		      (list (count-lines (point) end)
			    (count-words (point) end)
			    (- end (point)))))
		  nil 'tree)
	      sum lines into total-lines
	      sum words into total-words
	      sum characters into total-characters
	      finally return (let ((message (format "Subtree \"%s\" has %s lines, %s words, and %s characters."
						    (org-get-heading t t) total-lines total-words total-characters)))
			       (kill-new (number-to-string total-words))
			       (message message)
			       message)))))

(defun org-extras-jump-to-first-heading ()
  "Move point to the beginning of the first org heading in the current buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (org-next-visible-heading 1))

(defun org-extras-super-return (&optional indent arg interactive)
  "When `org-return-follows-link' is non-nil and point is on a
link, call `org-open-at-point' and set
`browse-url-browser-function' to `eww-browse-url'"
  (interactive "P")
  (let ((browse-url-browser-function 'eww-browse-url)
	(browse-url-handlers nil))
    (org-open-at-point)))

(defun org-extras-clear-heading-contents (&optional include-children include-properties)
  "Remove contents in org heading at point."
  (interactive)
  (save-restriction
    (if include-children
	(org-extras-narrow-to-entry-and-children)
      (org-extras-narrow-to-entry-no-children))
    (org-back-to-heading)
    (if include-properties
	(forward-line)
      (org-end-of-meta-data t))
    (delete-region (point) (point-max))))

(defun org-extras-paste-html ()
  "Convert the contents of the system clipboard to `org-mode' using `pandoc'."
  (interactive)
  (let* ((clipboard (if (eq system-type 'darwin)
			"pbv public.html"
		      "xclip -out -selection 'clipboard' -t text/html"))
	 (pandoc (concat "pandoc --wrap=none -f html -t org"))
	 (cmd (concat clipboard " | " pandoc))
	 (output (shell-command-to-string cmd))
	 ;; Not sure why Pandoc adds these double slashes; we remove them
	 (output (replace-regexp-in-string "^\\\\\\\\$" "" output))
	 (text (replace-regexp-in-string "= " "= " output)))
    (kill-new text)
    (yank)))

(defun org-extras-paste-image ()
  "Take the contents of the system clipboard and paste it as an image."
  (interactive)
  (if (executable-find "pngpaste")
      (let* ((counter 1)
	     (image-file (concat
			  paths-dir-org-images
			  (org-id-get nil 'create)
			  (format "-%d.png" counter))))
	(while (file-exists-p image-file)
	  (setq counter (1+ counter))
	  (setq image-file (concat
			    paths-dir-org-images
			    (org-id-get nil 'create)
			    (format "-%d.png" counter))))
	(call-process-shell-command (format "pngpaste '%s'" image-file))
	(let ((caption (read-string "Caption: ")))
	  (unless (string= caption "")
	    (insert (format "#+CAPTION: %s \n" caption))))
	(insert (format "[[file:%s]]" image-file))
	(org-display-inline-images)
	(message "You can toggle inline images with C-c C-x C-v"))
    (user-error "Requires pngpaste in PATH")))

;; The following functions produce a count of the TODOs added
;; or removed from all agenda files in the last day:
;; https://200ok.ch/posts/2022-12-06_how_much_did_you_get_done_today.html

(defun org-extras-count-lines-with-expression (s exp)
  "Count the number of lines in the string S that contain the regular expression EXP."
  (let ((count 0))
    (mapc (lambda (line)
	    (when (string-match-p exp line)
	      (setq count (+ 1 count))))
	  (split-string s "\n"))
    count))

(defun org-extras-productivity-of-the-day ()
  (seq-reduce
   (lambda (acc it)
     (let* ((folder (file-name-directory it))
	    (file (file-name-nondirectory it))
	    (base-cmd (concat "cd "
			      folder
			      "; git log --since=midnight -p "
			      file
			      "| grep TODO"))
	    (changed (shell-command-to-string base-cmd))
	    (added (count-lines-with-expression changed "^\\+"))
	    (removed (count-lines-with-expression changed "^\\-")))
       (cons (+ (car acc) added)
	     (- (cdr acc) removed))))
   org-agenda-files
   '(0 . 0)))

(defun org-extras-copy-dwim ()
  "Copy the contents of the org element at point."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (pcase (org-element-type (org-element-context))
	('headline (org-extras-copy-heading-name))
	('paragraph (org-extras-copy-heading-contents))
	('block (org-extras-copy-block))
	('table-cell (org-extras-copy-table-cell))
	(_ (user-error "I don't know what to copy")))
    (user-error "Not in org-mode")))

;;;;; org-agenda

(defun org-extras-agenda-switch-to-agenda-current-day ()
  "Open agenda in left window, creating it if necessary."
  (interactive)
  (window-extras-split-if-unsplit)
  (winum-select-window-1)
  (let ((agenda "*Org Agenda(a)*"))
    (if (get-buffer agenda)
	(switch-to-buffer agenda)
      (org-extras-agenda-toggle-anniversaries t)
      (org-agenda nil "a"))))

(defun org-extras-agenda-goto-and-start-clock ()
  "In org-agenda, go to entry at point and clock in."
  (interactive)
  (org-agenda-goto)
  (org-clock-in))

(defun org-extras-agenda-done-and-next ()
  "Temporary command to address bug when setting status via `org-agenda-todo'."
  (interactive)
  (org-agenda-goto)
  (org-todo "DONE")
  (org-extras-agenda-switch-to-agenda-current-day)
  (org-agenda-next-line))

(defun org-extras-agenda-postpone-and-next ()
  "Postpone task at point by one day and move to next task."
  (interactive)
  (org-agenda-date-later 1)
  (org-agenda-next-line))

(defun org-extras-unhighlight ()
  "Interactive version of `org-unhighlight'."
  (interactive)
  (org-unhighlight))

(defun org-extras-agenda-switch-to-dwim ()
  "Open the file at point or go to timer, based on `org-agenda' log state.
When point is in an agenda log line, go to that line and position in the
corresponding file. Else, open the file."
  (interactive)
  (if org-agenda-show-log
      (ignore-errors (call-interactively 'org-clock-convenience-goto-ts))
    (org-agenda-switch-to)))

;; this comments out the line "%%(org-bbdb-anniversaries-future 1)"
;; in `calendar.org', which is the only way I found to hide
;; anniversaries temporarily from the agenda
;; for context, see https://orgmode.org/manual/Weekly_002fdaily-agenda.html
(defun org-extras-agenda-toggle-anniversaries (&optional just-enable)
  "Toggle display of BBDB birthdays in the agenda.
If JUST-ENABLE is non-nil, always enable the display of birthdays."
  (interactive)
  (when org-extras-bbdb-anniversaries-heading
    (save-window-excursion
      (org-roam-extras-id-goto org-extras-bbdb-anniversaries-heading)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (org-end-of-meta-data t)
      (if (looking-at "%%")
	  (unless just-enable
	    (delete-line))
	(insert "%%(org-bbdb-anniversaries-future 1)\n")))
    (unless just-enable
      (org-agenda-redo))))

;;;;; org-capture

(defun org-extras-capture-hydra-notes-hook ()
  (when (string= (org-capture-get :key t) "p")
    (hydra-org-notes-only-clock/body)))

(defun org-extras-capture-before-finalize-hook-function ()
  "Define behavior of `org-capture-before-finalize-hook'."
  (require 'org-capture)
  (pcase (plist-get org-capture-plist :key)
    ("gg"
     (org-ai-mode)
     (org-narrow-to-subtree)
     (forward-line)
     (forward-line)
     (insert "#+begin_ai\n[SYS]: You are a helpful assistant.\n\n[ME]:\n#+end_ai
")
     (message "finished"))
    ("gd"
     (org-ai-mode)
     (org-narrow-to-subtree)
     (forward-line)
     (forward-line)
     (insert "#+begin_ai\n[SYS]: You are a helpful assistant.}\n\n}[ME]:\n#+end_ai
"))
    ("l"
     (org-align-all-tags)
     (ispell-change-dictionary "english"))
    ("la"
     (save-window-excursion
       (files-extras-switch-to-alternate-buffer)
       (org-extras-jump-to-first-heading)
       (widen)
       (org-narrow-to-subtree)
       (let ((org-use-tag-inheritance))
	 (org-roam-tag-remove '("unprocessed" "empty" "leo" "unpublished"))
	 (org-roam-tag-add '("leo")))
       (files-extras-show-buffer-name))
     (goto-char 0)
     (search-forward "Procesar ")
     (insert (format "~%s~" (current-kill 0))))
    ("le"
     (save-window-excursion
       (files-extras-switch-to-alternate-buffer)
       (files-extras-show-buffer-name))
     (goto-char 0)
     (search-forward "Renombrar ")
     (insert (concat "~" (current-kill 0) "~ ")))
    ;; Add link to open Slack message externally.
    ("s"
     (org-narrow-to-subtree)
     (let ((url (s-replace-regexp
		 "emacs-slack:[_[:digit:][:alnum:]]\\{11\\}&\\([_[:digit:][:alnum:]]\\{11\\}\\)&ts:\\([[:digit:]]\\{10\\}\\)\\.\\([[:digit:]]\\{6\\}\\)"
		 "https://samotsvety.slack.com/archives/\\1/p\\2\\3"
		 (plist-get org-store-link-plist :link))))
       (goto-char (point-max))
       (insert (format "[[%s][external link]]" url))))
    ("tmt"
     (org-web-tools-insert-link-for-url (current-kill 0)))
    ("v"
     (org-do-demote))
    ("y"
     (require 'youtube-dl)
     (youtube-dl (current-kill 0)
		 :directory paths-dir-downloads-directory
		 :destination (prot-eww--sluggify
			       (org-extras-web-tools--org-title-for-url))))))

;;;;; org-clock

(defun org-extras-new-clock-entry-today (begin end)
  "Insert a new clock entry with today's date, prompting for BEGIN and END times."
  (interactive "sTime begins: \nsTime ends: ")
  (org-extras-jump-to-latest-clock-entry)
  (crux-smart-open-line-above)
  (let ((today (format-time-string "%Y-%m-%d %a" (current-time))))
    (insert "CLOCK: [%s %s]--[%s %s]" today begin today end))
  (org-evaluate-time-range))

(defun org-extras-time-stamp-active-current-time ()
  "Insert an active timestamp with the current date and time."
  (interactive)
  (org-time-stamp '(16)))

(defun org-extras-time-stamp-inactive-current-time ()
  "Insert an inactive timestamp with the current date and time."
  (interactive)
  (org-time-stamp '(16) t))

(defun org-extras-time-stamp-active-current-date ()
  "Insert an active timestamp with the current date."
  (interactive)
  (org-insert-time-stamp (current-time) nil))

(defun org-extras-time-stamp-inactive-current-date ()
  "Insert an inactive timestamp with the current date."
  (interactive)
  (org-insert-time-stamp (current-time) nil t))

(defun org-extras-jump-to-latest-clock-entry ()
  "Jump to most recent clock entry for org heading at point."
  (interactive)
  (visible-mode)
  (org-extras-show-logbook)
  (org-back-to-heading)
  (re-search-forward "CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([A-Za-z]\\{3\\}\\) \\([0-9]\\{2\\}:\\)" nil t))

(defun org-extras-clock-report (start-date end-date)
  "Generate an org clock report for the period between START-DATE and END-DATE."
  (interactive
   (list (org-read-date nil nil nil "Start date: ")
	 (org-read-date nil nil nil "End date: ")))
  (insert (format "#+BEGIN: clocktable :scope subtree :maxlevel 4 :narrow 50 :tstart \"%s\" :tend \"%s\"\n#+END:" start-date end-date))
  (org-clock-report))

(defun org-extras-delete-headings-without-logbook ()
  "Delete all headings in the current buffer that do not have a logbook."
  (interactive)
  (when (y-or-n-p "Delete all headings without logbook in the current buffer? ")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ .*$" nil t)
	(unless (save-excursion
		  (re-search-forward ":LOGBOOK:" (save-excursion (outline-next-heading) (point)) t))
	  (let ((end (save-excursion (or (outline-next-heading) (point-max)))))
	    (delete-region (point-at-bol) end)))))))

(defun org-extras-clocktable-sorter (ipos tables params)
  "Sort clocktable tables by time.
IPOS, TABLES and PARAMS are required by the formatter function."
  (setq tables (cl-sort tables (lambda (table1 table2) (> (nth 1 table1) (nth 1 table2)))))
  (funcall (or org-clock-clocktable-formatter 'org-clocktable-write-default) ipos tables params))

;;;;; org-cycle

(defun org-extras-cycle-global (&optional arg)
  "Cycle the global visibility, hiding archived subtrees.
With `C-u' prefix ARG, switch to startup visibility. With a numeric prefix, show
all headlines up to that level."
  (interactive)
  (org-cycle-global arg)
  (org-cycle-hide-archived-subtrees 'all))

;;;;; org-fold

(defun org-extras-fold-show-all-headings ()
  "Show contents of all headings in buffer, except archives."
  (org-fold-show-all '(headings))
  (org-cycle-hide-archived-subtrees 'all))

;; github.com/org-roam/org-roam/wiki/User-contributed-Tricks#hiding-the-properties-drawer
(defun org-extras-hide-properties ()
  "Hide all `org-mode' headline property drawers in buffer.
Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	    "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
	(overlay-put ov_this 'display "")
	(overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-extras-hide-logbook ()
  "Hide all `org-mode' headline logbook drawers in buffer.
Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	    "^ *:logbook:\n\\(^clock:.*?\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
	(overlay-put ov_this 'display "")
	(overlay-put ov_this 'hidden-logbook-drawer t))))
  (put 'org-toggle-logbook-hide-state 'state 'hidden))

(defun org-extras-show-properties ()
  "Show all `org-mode' property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-extras-show-logbook ()
  "Show all `org-mode' logbook drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-logbook-drawer t)
  (put 'org-toggle-logbook-hide-state 'state 'shown))

;;;;; org-id

(defun org-extras-id-auto-add-ids-to-headings-in-file ()
  "Add IDs to all headings in the current file missing them."
  (when-let ((file (buffer-file-name)))
    (when (and (eq major-mode 'org-mode)
	       (string-match paths-dir-org file)
	       (eq buffer-read-only nil))
      (unless
	  (or
	   ;; exclude directories
	   (member (file-name-directory (buffer-file-name))
		   org-extras-id-auto-add-excluded-directories)
	   ;; exclude files
	   (member (buffer-file-name)
		   org-extras-id-auto-add-excluded-files)
	   (member (org-get-heading)
		   '("Local variables"
		     "COMMENT Local variables"
		     "TODO Local variables")))
	(org-map-entries 'org-id-get-create)))))

(defun org-extras-id-update-id-locations ()
  "Scan relevant files for IDs.
Store the relation between files and corresponding IDs. This will
scan all agenda files, all associated archives, all open Org
files, recursively all files in `org-directory', and all files in
`org-id-extra-files'."
  (interactive)
  (org-id-update-id-locations
   (directory-files-recursively org-directory ".org$\\|.org.gpg$")))

(defun org-extras-id-notes-with-clock (key)
  "Clock in to the org note with ID KEY."
  (funcall (intern (concat "hydra-org-notes/lambda-" key "-and-exit")))
  (org-clock-in))

;;;;; org-list

(defun org-extras-mark-checkbox-complete-and-move-to-next-item ()
  "Mark checkbox as completed and move to the next item."
  (interactive)
  (org-ctrl-c-ctrl-c nil)
  (let ((debug-on-error nil))
    (org-next-item)))

;; I should instead advice the original org function
(defun org-extras-reset-checkbox-state-subtree ()
  "Reset all checkboxes in an entry subtree, without showing heading properties."
  (interactive)
  (org-reset-checkbox-state-subtree)
  (org-cycle)
  (org-cycle))

;;;;; org-refile

(defun org-extras-refile-at-position (position)
  "Refile current subtree at POSITION."
  (org-refile nil nil (list nil (buffer-file-name) nil position)))

(defun org-extras-refile-goto-latest ()
  "Jump to the most recently refiled item."
  (interactive)
  (widen)
  (org-refile '(16)))

(defun org-extras-refile-and-archive ()
  "Refile and archive the current subtree."
  (interactive)
  (org-refile)
  (save-window-excursion
    (save-excursion
      (org-refile '(16))
      (org-archive-to-archive-sibling))))

;; Inspired by emacs.stackexchange.com/q/8045/32089
(defun org-extras-refile-to (file heading)
  "Refile HEADING to FILE."
  (let ((pos (save-excursion
	       (find-file file)
	       (org-find-exact-headline-in-buffer heading))))
    (org-refile nil nil (list heading file nil pos))))

;;;;; ol

(defun org-extras-sort-links (separator)
  "Sort list of links in current subtree separated by SEPARATOR."
  (interactive "sEnter separator: ")
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'paragraph))
	   (raw-text (buffer-substring-no-properties (car bounds) (cdr bounds)))
	   (elements (split-string raw-text separator t))
	   sorted-elements)
      (with-temp-buffer
	(org-mode)
	(dolist (el (reverse elements))
	  (insert "- " el "\n"))
	;; Since `org-sort-list` sorts from point to the end of the list,
	;; go to the beginning of the temp buffer
	(goto-char (point-min))
	(org-sort-list nil ?a nil #'downcase)
	;; Then gather the sorted elements back
	(setq sorted-elements (s-replace "- " "" (buffer-string))))
      ;; Update the original paragraph
      (delete-region (car bounds) (cdr bounds))
      (insert (string-join (split-string sorted-elements "\n" t) separator)))))

(defun org-extras-sort-keywords ()
  "Sort bullet-separated keywords in current subtree."
  (interactive)
  (org-extras-sort-links " • "))

(defun org-extras-linkify-elements (strings &optional separator)
  "For all STRINGS, return its link if node is found, else the string itself.
The elements are returned as a string separated by SEPARATOR. If
SEPARATOR is nil, use ' • '."
  (require 'org-roam)
  (let ((nodes (org-roam-node-list)))
    (string-join
     (mapcar (lambda (x)
	       (let* ((node (seq-find (lambda (n)
					(string-equal (downcase (org-roam-node-title n)) (downcase x)))
				      nodes))
		      (nodetitle (if node (org-roam-node-title node))))
		 (if node
		     (format "[[id:%s][%s]]" (org-roam-node-id node) nodetitle)
		   x)))
	     strings)
     (or separator " • "))))

;;;;; ob

(defun org-extras-confirm-babel-evaluate (lang body)
  "`org-babel' confirm function that prompts for confirmation.
LANG and BODY are required by the `org-confirm-babel-evaluate' user option."
  (not (member lang '("python" "emacs-lisp"))))

(defun org-extras-babel-tangle ()
  "Widen buffer, save its contents, and tangle file."
  (interactive)
  (widen)
  (save-buffer)
  (org-babel-tangle))

;;;;; org-crypt

(defun org-extras-crypt-dwim ()
  "Decrypt entry unless in clock, then evaluate time range."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (pcase (org-element-type (org-element-context))
	('clock (org-evaluate-time-range))
	(_ (org-decrypt-entry)))
    (user-error "Not in org-mode")))

;;;;; org-pomodoro

(defun org-extras-pomodoro-format-timer ()
  "Format the `org-pomodoro' timer.
We set this value by advising `org-pomodoro' so that the pomodoro
count is updated."
  (require 'org-pomodoro)
  (setq org-pomodoro-format
	(concat "🍅 %s"
		(format "|%s" (number-to-string org-pomodoro-count)))))

;;;;; to sort

(defun org-extras-show-subtree-hide-drawers ()
  "Show org subtree and hide drawers."
  (outline-hide-subtree)
  (org-fold-show-entry)
  (org-fold-show-children))

(defun org-extras-narrow-to-entry-and-children ()
  "Narrow org buffer to entry and all its children."
  (interactive)
  (org-narrow-to-subtree)
  (org-extras-show-subtree-hide-drawers))

(defun org-extras-narrow-to-entry-no-children ()
  "Narrow org buffer to entry excluding all children."
  (interactive)
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

(defun org-extras-goto-beginning-of-heading-text ()
  "Jump to the beginning of the headline text.
That is, move point after the stars, and the TODO and priority if present."
  (when (org-at-heading-p)
    (let ((org-special-ctrl-a/e t))
      (org-end-of-line)
      (org-beginning-of-line))))

(defun org-extras-id-notes-only-clock (key)
  "Clock in to a heading with KEY."
  (require 'simple-extras)
  (simple-extras-save-excursion
   (funcall (intern (concat "hydra-org-notes/lambda-" key "-and-exit")))
   (org-clock-in)))

;; Moved here temporarily, but should probably be intergrated into a proper `gdrive' package.
(defun org-extras-import-from-google-drive ()
  "Import Google Doc file with DOC-ID and convert it to `org-mode'.
To see a list of Google Docs and their respective IDs, run
`gdrive list' in the terminal."
  (interactive)
  (let* ((default-directory paths-dir-downloads)
	 (doc-id (read-from-minibuffer "Doc ID: "))
	 (doc-info (shell-command-to-string
		    (format "gdrive info '%s'" doc-id)))
	 (doc-name (when (string-match "^Name: \\(.*\\)$" doc-info)
		     (match-string 1 doc-info)))
	 (input (concat doc-name ".docx"))
	 (output (concat doc-name ".org")))
    ;; download Google Doc as docx
    (shell-command
     (format "gdrive export --mime application/vnd.openxmlformats-officedocument.wordprocessingml.document %s" doc-id))
    ;; export docx to org-mode
    (shell-command
     (format "pandoc -s '%s' -o '%s'" input output))))

;;;;; Patched functions

;; replace `org-files-list' with `org-agenda-files' so that
;; extraneous files are excluded from the dangling clocks check.
(el-patch-defun  org-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open Org clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (el-patch-swap
		      (org-files-list)
		      (org-agenda-files)))
	(let ((clocks (org-find-open-clocks file)))
	  (dolist (clock clocks)
	    (let ((dangling (or (not (org-clock-is-active))
				(/= (car clock) org-clock-marker))))
	      (if (or (not only-dangling-p) dangling)
		  (org-clock-resolve
		   clock
		   (or prompt-fn
		       (lambda (clock)
			 (format
			  "Dangling clock started %d mins ago"
			  (floor (org-time-convert-to-integer
				  (time-since (cdr clock)))
				 60))))
		   (or last-valid
		       (cdr clock)))))))))))

;; the function has to be slightly modified to make it work
(el-patch-defun org-clock-split
  (from-end splitter-string)
  "Split CLOCK entry under cursor into two entries.
Total time of created entries will be the same as original entry.

   WARNING: Negative time entries can be created if splitting at an offset
longer then the CLOCK entry's total time.

   FROM-END: nil if the function should split with duration from
   the start of the clock segment (default for backwards
   compatibility), t if the function should split counting from
   the end of the clock segment.

   SPLITTER-STRING: Time offset to split record at.  Examples: '1h', '01m', '68m1h', '9:20'."

  (interactive "P\nsTime offset to split clock entry (ex 1h2m): ")

  (move-beginning-of-line nil)
  (let ((original-line (buffer-substring (line-beginning-position) (line-beginning-position 2))))

    ;; Error if CLOCK line does not contain check in and check out time
    (unless (string-match org-clock-split-clock-range-regexp original-line)
      (error "Cursor must be placed on line with valid CLOCK entry range"))

    (let* ((whitespace (match-string 1 original-line))
	   (timestamps (org-clock-split-split-line-into-timestamps original-line splitter-string from-end))
	   (t0 (pop timestamps))
	   (t1 (el-patch-swap
		 (pop timestamps)
		 (concat "[" (pop timestamps) "]")))
	   (t2
	    (pop timestamps)))
      (delete-region
       (line-beginning-position)
       (line-end-position))
      (insert
       (format org-clock-split-clock-range-format whitespace t0 t1))
      (org-ctrl-c-ctrl-c)
      (move-beginning-of-line nil)
      (newline)
      (previous-line)
      (insert
       (format org-clock-split-clock-range-format whitespace t1 t2))
      (org-ctrl-c-ctrl-c)
      (move-beginning-of-line nil))))

;; Replace native function with variant that doesn't ask the user
;; multiple times to remove non-existent agenda file
(el-patch-defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (el-patch-swap
    (unless (file-exists-p file)
      (message "Non-existent agenda file %s.  [R]emove from list or [A]bort?"
	       (abbreviate-file-name file))
      (let ((r (downcase (read-char-exclusive))))
	(cond
	 ((equal r ?r)
	  (org-remove-file file)
	  (throw 'nextfile t))
	 (t (user-error "Abort")))))
    (unless (file-exists-p file)
      (org-remove-file file)
      (throw 'nextfile t))))

;; Comment out `org-cite--allowed-p' condition to allow invocation
;; in any mode. Even if inserting a citation is not allowed, one may
;; want to invoke the command to trigger contextual actions via
;; `embark'.
(el-patch-defun org-cite-insert (arg)
  "Insert a citation at point.
Insertion is done according to the processor set in `org-cite-insert-processor'.
ARG is the prefix argument received when calling interactively the function."
  (interactive "P")
  (unless org-cite-insert-processor
    (user-error "No processor set to insert citations"))
  (org-cite-try-load-processor org-cite-insert-processor)
  (let ((name org-cite-insert-processor))
    (cond
     ((not (org-cite-get-processor name))
      (user-error "Unknown processor %S" name))
     ((not (org-cite-processor-has-capability-p name 'insert))
      (user-error "Processor %S cannot insert citations" name))
     (t
      (let ((context (org-element-context))
	    (insert (org-cite-processor-insert (org-cite-get-processor name))))
	(cond
	 ((org-element-type-p context '(citation citation-reference))
	  (funcall insert context arg))
	 (el-patch-remove
	   ((org-cite--allowed-p context)
	    (funcall insert nil arg)))
	 (t
	  (el-patch-swap (user-error "Cannot insert a citation here")
			 (funcall insert nil arg)))))))))

;; name buffers more cleanly
(el-patch-defun org-src--construct-edit-buffer-name (org-buffer-name lang)
  "Construct the buffer name for a source editing buffer.
Format is \"*Org Src ORG-BUFFER-NAME[ LANG ]*\"."
  (el-patch-swap
    (concat "*Org Src " org-buffer-name "[ " lang " ]*")
    (concat org-buffer-name " (org src)")))

;;;; Footer

(provide 'org-extras)

;;; org-extras.el ends here
