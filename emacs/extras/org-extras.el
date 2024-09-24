;;; org-extras.el --- Extensions for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Pablo Stafforini

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

(require 'el-patch)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'oc)
(require 'paths)
(require 'simple-extras)
(require 'transient)

;;;; User options

(defgroup org-extras ()
  "Extensions for `org'."
  :group 'org)

(defcustom org-extras-agenda-files-excluded
  (list paths-file-tlon-tareas-leo
	paths-file-tlon-tareas-fede)
  "Files to exclude from `org-agenda'.
I have to exclude these files because otherwise extraneous information shows up
in my agenda, such as TODOs and time logs. These files lack the `property' tag
but the may still otherwise be included if they have been modified recently (see
the function `vulpea-agenda-files-update')."
  :type '(repeat file)
  :group 'org-extras)

(defcustom org-extras-id-auto-add-excluded-directories nil
  "Directories to exclude from `org-extras-id-auto-add-ids-to-headings-in-file'."
  :type '(repeat file)
  :group 'org-extras)

(defcustom org-extras-id-auto-add-excluded-files
  (list paths-file-orb-noter-template)
  "Files to exclude from `org-extras-id-auto-add-ids-to-headings-in-file'."
  :type '(repeat file)
  :group 'org-extras)

(defcustom org-extras-bbdb-anniversaries-heading
  "2A37A3CC-2A11-4933-861B-48B129B9EA2D"
  "Heading in `calendar.org' that contains the BBDB anniversaries.
Set to nil to disable display of BBDB anniversaries in agenda."
  :type 'string
  :group 'org-extras)

(defcustom org-extras-clock-report-parameters
  "#+BEGIN: clocktable :scope %s :maxlevel 9 :narrow 500! :fileskip0 t %s \n#+END:"
  "Parameters for `org-extras-clock-report-insert'.
The first %s is the scope of the report, and the second %s is the range."
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
  "When point is on a URL, copy it to the kill ring."
  (interactive)
  (cond
   ((thing-at-point-url-at-point)
    (kill-new (thing-at-point-url-at-point)))
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
;; consider replacing with https://emacs.stackexchange.com/questions/69924/count-words-under-subtree-ignoring-the-properties-drawer-and-the-subheading
(defun org-extras-count-words ()
  "Count words in region if active; otherwise count words in current subtree."
  (interactive)
  (if (use-region-p)
      (funcall-interactively #'count-words-region (region-beginning) (region-end))
    (org-with-wide-buffer
     (cl-loop for (lines words characters)
	      in (org-map-entries
		  (lambda ()
		    (org-end-of-meta-data 'full)
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

;; TODO: revise this
(defun org-extras-super-return ()
  "Call a special form of RET.
When `org-return-follows-link' is non-nil and point is on a link, call
`org-open-at-point' and set `browse-url-browser-function' to `eww-browse-url'"
  (interactive)
  (let ((browse-url-browser-function 'eww-browse-url)
	(browse-url-handlers nil))
    (org-open-at-point)))

(declare-function simple-extras-pandoc-convert "simple-extras")
(defun org-extras-paste-with-conversion ()
  "Convert the contents of the system clipboard to Org Mode using Pandoc.
This command will convert from HTML if the clipboard contains HTML, and from
Markdown otherwise.

See also `markdown-mode-extras-paste-with-conversion'. For the reverse process,
use `ox-clip-formatted-copy'."
  (interactive)
  (let ((output (simple-extras-pandoc-convert "org" "markdown")))
    (insert
     (with-temp-buffer
       (insert output)
       (dolist (regexp '(("^\\\\\\\\" . "")
			 ("\\\\\\\\$" . "")
			 (" " . " ")))
	 (goto-char (point-min))
	 (while (re-search-forward (car regexp) nil t)
	   (replace-match (cdr regexp) nil nil)))
       (buffer-string)))))

(defun org-extras-paste-image ()
  "Take the contents of the system clipboard and paste it as an image."
  (interactive)
  (if (executable-find "pngpaste")
      (let* ((counter 1))
	(while (file-exists-p (org-extras-make-image-filename counter))
	  (setq counter (1+ counter)))
	(let ((filename (org-extras-make-image-filename counter)))
	  (call-process-shell-command (format "pngpaste '%s'" filename))
	  (let ((caption (read-string "Caption: ")))
	    (unless (string-empty-p caption)
	      (insert (format "#+CAPTION: %s \n" caption))))
	  (insert (format "[[file:%s]]" filename))
	  (org-display-inline-images)))
    (user-error "`pngpaste' not found; please install it (e.g. `brew install pngpaste')")))

(defun org-extras-make-image-filename (count)
  "Make a unique filename for an image based on COUNT."
  (concat paths-dir-org-images
	  (org-id-get nil 'create)
	  (format "-%03d.png" count)))

(defun org-extras-inline-images (&optional arg)
  "Enable or disable the display of inline images.
If called interactively, toggle the display of inline images. If the prefix
argument is negative, disable the display of inline images; otherwise, enable
it.

If called from Lisp, toggle the display of inline images if ARG is `toggle'.
Enable the display of inline images if ARG is nil, omitted, or is a positive
number. Disable the mode if ARG is a negative number."
  (interactive "P")
  (when (or (and (called-interactively-p 'interactive) (not arg))
	    (eq arg 'toggle))
    (setq arg
	  (if (org--inline-image-overlays) -1 1)))
  (if (and arg (< arg 0))
      (org-remove-inline-images)
    (org-display-inline-images)))

;; The following functions produce a count of the TODOs added
;; or removed from all agenda files in the last day:
;; https://200ok.ch/posts/2022-12-06_how_much_did_you_get_done_today.html
(defun org-extras-count-lines-with-expression (s exp)
  "Count the number of lines in the string S that contain the regexp EXP."
  (let ((count 0))
    (mapc (lambda (line)
	    (when (string-match-p exp line)
	      (setq count (+ 1 count))))
	  (split-string s "\n"))
    count))

(defun org-extras-productivity-of-the-day ()
  "Compute productivity of the day."
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
	    (added (org-extras-count-lines-with-expression changed "^\\+"))
	    (removed (org-extras-count-lines-with-expression changed "^\\-")))
       (cons (+ (car acc) added)
	     (- (cdr acc) removed))))
   org-agenda-files
   '(0 . 0)))

;;;;; org-agenda

(defvar org-extras-agenda-switch-to-agenda-current-day-timer nil
  "Timer to switch to agenda of current day.")

(declare-function window-extras-split-if-unsplit "window-extras")
(declare-function winum-select-window-1 "winum")
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
  "In `org-agenda', go to entry at point and clock in."
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
(declare-function org-roam-extras-id-goto "org-roam-extras")
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

(defun org-extras-agenda-toggle-log-mode ()
  "Toggle `org-agenda-log-mode' and `org-agenda-log-mode-items'."
  (interactive)
  (org-agenda-log-mode 'clockcheck))

;;;;; org-capture

(declare-function org-web-tools-insert-link-for-url "org-web-tools")
(declare-function org-extras-web-tools--org-title-for-url "org-web-tools")
(declare-function youtube-dl "youtube-dl")
(declare-function org-roam-tag-remove "org-roam")
(declare-function org-roam-tag-add "org-roam")
(declare-function files-extras-show-buffer-name "file-extras")
(declare-function files-extras-switch-to-alternate-buffer "files-extras")
(defun org-extras-capture-before-finalize-hook-function ()
  "Define behavior of `org-capture-before-finalize-hook'."
  (pcase (plist-get org-capture-plist :key)
    ("l"
     (org-align-tags)
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
     (let ((url (replace-regexp-in-string
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
     (youtube-dl (current-kill 0)
		 :directory paths-dir-downloads
		 :destination (simple-extras-slugify
			       (org-extras-web-tools--org-title-for-url))))))

;;;;; org-clock

(declare-function crux-smart-open-line-above "crux")
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

(defun org-extras-clock-report-insert (start-date end-date scope)
  "Insert an org clock report for the period between START-DATE and END-DATE.
SCOPE is the scope of the report, and can be `agenda', `file', or `subtree'."
  (interactive
   (list (org-read-date nil nil nil "Start date: ")
	 (org-read-date nil nil nil "End date: ")
	 (org-completing-read "Scope: " '("agenda" "file" "subtree"))))
  (let ((range (if (string= start-date end-date)
		   (format ":block \"%s\"" start-date)
		 (format ":tstart \"%s\" :tend \"%s\"" start-date end-date))))
    (insert (format org-extras-clock-report-parameters scope range))
    (org-clock-report)))

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
	    (delete-region (line-beginning-position) end)))))))

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
    (when (and (derived-mode-p 'org-mode)
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

(declare-function s-replace "s")
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

(declare-function org-roam-node-list "org-roam")
(declare-function org-roam-node-title "org-roam")
(declare-function org-roam-node-id "org-roam")
(defun org-extras-linkify-elements (strings &optional separator)
  "For all STRINGS, return its link if node is found, else the string itself.
The elements are returned as a string separated by SEPARATOR. If
SEPARATOR is nil, use ' • '."
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

;;;;; ol-eww

;; like `org-eww-copy-for-org-mode', but also handle italics, boldface and bullets
;; TODO: handle headings. The relevant faces are `shr-h1', `shr-h2', etc.
(defvar shr-bullet)
(defun org-extras-eww-copy-for-org-mode ()
  "Copy current buffer content or active region with `org-mode' style links.
This will encode `link-title' and `link-location' with
`org-link-make-string' and insert the transformed text into the
kill ring, so that it can be yanked into an Org mode buffer with
links working correctly.

Further lines starting with a star get quoted with a comma to
keep the structure of the Org file."
  (interactive)
  (let ((regionp (use-region-p))
	(transform-start (point-min))
	(transform-end (point-max))
	(return-content ""))
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end)))
    (deactivate-mark)  ; Deactivate region highlighting if it's active.
    (save-excursion
      (goto-char transform-start)
      (while (< (point) transform-end)
	(let* ((text-face (if (listp (get-text-property (point) 'face))
			      (get-text-property (point) 'face)
			    (list (get-text-property (point) 'face))))
	       (link (get-text-property (point) 'shr-url))
	       (text (buffer-substring-no-properties (point)
						     (or (next-single-property-change (point) 'face nil transform-end)
							 (next-single-property-change (point) 'shr-url nil transform-end)
							 transform-end)))
	       (formatted-text (replace-regexp-in-string shr-bullet "- " text)))
	  ;; Apply Org mode formatting for italics and bold where applicable.
	  (when (memq 'italic text-face)
	    (setq formatted-text (concat "/" formatted-text "/")))
	  (when (memq 'bold text-face)
	    (setq formatted-text (concat "*" formatted-text "*")))
	  ;; Format links according to Org mode syntax.
	  (when link
	    (setq formatted-text (concat "[[" link "][" text "]]")))
	  ;; Append the formatted text to the return content.
	  (setq return-content (concat return-content formatted-text))
	  ;; Advance the point.
	  (goto-char (or (next-single-property-change (point) 'face)
			 (next-single-property-change (point) 'shr-url)
			 (point-max))))))
    ;; Copy the whole formatted content to the kill ring.
    (kill-new return-content)))

;;;;; ob

(defun org-extras-confirm-babel-evaluate (lang _)
  "Confirm function before evaluating code block.
LANG is the language of the code block."
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
      (org-beginning-of-line))
    ;; handle special case when heading consists of a TODO status followed by a single space
    (when (looking-at "^\\*+ [A-Z]+ $")
      (goto-char (match-end 0)))))

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

(defun org-extras-remove-trailing-heading ()
  "Remove empty heading at the end of current buffer.
This function is called automatically via a directory-local variable in the
directory that stores the `gptel' notes. It is used to avoid the creation of
empty headings, which trigger an `org-roam' warning."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (org-previous-visible-heading 1)
    (when (string-empty-p (org-get-heading))
      (delete-region (point) (point-max)))))

;;;;; Menus

(transient-define-prefix org-extras-personal-menu ()
  "Menu for personal projects."
  [[("e" "email"             (lambda () (interactive) (org-roam-extras-id-goto "96BBA849-B4CF-41C0-ABA3-A5D901BCDB18")))
    ("f" "finance"           (lambda () (interactive) (org-roam-extras-id-goto "EB812B59-BBFB-4E06-865A-ACF5A4DE5A5C")))
    ("i" "Anki"              (lambda () (interactive) (org-roam-extras-id-goto "50BAC203-6A4D-459B-A6F6-461E6908EDB1")))
    ("y" "YouTube"           (lambda () (interactive) (org-roam-extras-id-goto "14915C82-8FF3-460D-83B3-148BB2CA7B7E")))
    ("k c" "cat sense"      (lambda () (interactive) (org-roam-extras-id-goto "ED18E2B5-BDEA-4FC0-9D1C-F31338471814")))
    ("k h" "Hitler bio"      (lambda () (interactive) (org-roam-extras-id-goto "7DA83AB7-BCF4-4218-ADCF-91C8C5B991F1")))
    ("k g" "Pimsleur German"      (lambda () (interactive) (org-roam-extras-id-goto "3B9725E0-79C2-44FA-B0FA-41D43EC4F4A8")))
    ("k f" "Pimsleur French"      (lambda () (interactive) (org-roam-extras-id-goto "60232ADC-60B8-4617-AEFB-F6F73A099709")))]])

(transient-define-prefix org-extras-tlon-menu ()
  "Menu for Tlön projects."
  [["Tlön"
    ("t t" "tlon"              (lambda () (interactive) (org-roam-extras-id-goto "843EE71C-4D50-4C2F-82E6-0C0AA928C72A")))
    ("t e" "tlon-emacs"        (lambda () (interactive) (org-roam-extras-id-goto "E38478D6-1540-4496-83F3-43C964567A15")))
    ("t i" "tlon inbox"        (lambda () (interactive) (org-roam-extras-id-goto "E9C77367-DED8-4D59-B08C-E6E1CCDDEC3A")))]
   ["Babel"
    ("b c" "babel"             (lambda () (interactive) (org-roam-extras-id-goto "DFE45995-7935-4F19-80DA-FB9C11FE9E24")))
    ("b s" "babel-es"          (lambda () (interactive) (org-roam-extras-id-goto "A2347582-CF81-497E-81C9-CF82E56D8312")))
    ("b r" "babel-refs"        (lambda () (interactive) (org-roam-extras-id-goto "06C5E072-99F2-4A1F-A87E-0E05E330E111")))]
   ["Uqbar"
    ("q q" "uqbar-common"      (lambda () (interactive) (org-roam-extras-id-goto "1844F672-62B5-49CF-8BD8-A55F8FCAAFE9")))
    ("q s" "uqbar-es"          (lambda () (interactive) (org-roam-extras-id-goto "EF190A03-0037-430A-B8A1-414738AEAEA4")))
    ("q t" "uqbar-it"          (lambda () (interactive) (org-roam-extras-id-goto "124B8358-88C8-49CC-9C4D-6B6919BF64BE")))
    ("q r" "uqbar-fr"          (lambda () (interactive) (org-roam-extras-id-goto "8FDA9155-C270-4FB0-AA2A-DA87BED19BE0")))]
   ["utilitarianism"
    ("u n" "utilitarianism-en" (lambda () (interactive) (org-roam-extras-id-goto "F80849CB-F04A-4EDF-B71B-F98277D3F462")))]
   ["Longtermism"
    ("l s" "longtermism-es"    (lambda () (interactive) (org-roam-extras-id-goto "2514AA39-CFBF-4E5A-B18E-147497E31C8F")))]
   ["Radio Altruismo Eficaz"
    ("r" "rae"                 (lambda () (interactive) (org-roam-extras-id-goto "BA0985E0-13A4-4C01-9924-03559E100CF0")))]
   ["Misc"
    "EA International"
    ("i" "ea.international"    (lambda () (interactive) (org-roam-extras-id-goto "AF3FEF60-7624-4C3C-9A48-1FB531D1D635")))
    "EA News"
    ("n" "ean-issues"          (lambda () (interactive) (org-roam-extras-id-goto "A2710AA8-BEEB-412D-9FE0-8AF856E4464C")))
    "La Bisagra"
    ("s" "bisagra"             (lambda () (interactive) (org-roam-extras-id-goto "CE8A5497-1BF9-4340-9853-5ADA4605ECB5")))
    "Boletín"
    ("a" "boletin"             (lambda () (interactive) (org-roam-extras-id-goto "989E6696-2672-47FE-855B-00DA806B7A56")))
    "GWWC"
    ("g" "gwwc"                (lambda () (interactive) (org-roam-extras-id-goto "BA0985E0-13A4-4C01-9924-03559E100CF0")))]
   [""
    "Meetings"
    ("m f" "fede"              (lambda () (interactive) (org-roam-extras-id-goto "56CBB3F8-8E75-4298-99B3-899365EB75E0")))
    ("m l" "leo"               (lambda () (interactive) (org-roam-extras-id-goto "51610BEB-7583-4C84-8FC2-A3B28CA79FAB")))
    ("m g" "group"             (lambda () (interactive) (org-roam-extras-id-goto "BE68100E-753D-408B-9B31-2D58A457A70B")))
    ""
    "Comms"
    ("c x" "Check inbox"       (lambda () (interactive) (org-roam-extras-id-goto "A2555D4E-AC86-4986-AF7E-778D6E78D47E")))
    ("c i" "Check issues"      (lambda () (interactive) (org-roam-extras-id-goto "03B164B5-89FC-4611-A2B9-6E5D3AE7C1B1")))
    ("c e" "Check email"       (lambda () (interactive) (org-roam-extras-id-goto "EA0B83B2-8A4A-417A-8318-56B4EDC75FF5")))
    ("c t" "Check telegram"    (lambda () (interactive) (org-roam-extras-id-goto "DF643B0F-1956-44AB-90DD-749D849C285D")))]])

;;;###autoload (autoload 'org-extras-config-dispatch "org-extras" nil t)
(transient-define-prefix org-extras-config-dispatch ()
  "Jump to a section in `config.org'."
  [["Org headings: config.org"
    ("c" "Calc" (lambda () (interactive) (org-roam-extras-id-goto "50FAD2F3-E501-408E-A9A2-8358FAA87C1C")))
    ("d" "Dired" (lambda () (interactive) (org-roam-extras-id-goto "617F5323-6518-4751-948B-3E8032D93130")))
    ("e" "Elfeed" (lambda () (interactive) (org-roam-extras-id-goto "FF5DDBC3-ABB6-48A9-9B47-BC9A18F532D5")))
    ("f" "Files & buffers" (lambda () (interactive) (org-roam-extras-id-goto "B29F4586-2B8D-41FE-82DE-FEDCD863C74B")))
    ("g" "Graveyard" (lambda () (interactive) (org-roam-extras-id-goto "AACAE0F4-0B25-475B-831B-3F1E91E6349D")))
    ("i" "Introduction" (lambda () (interactive) (org-roam-extras-id-goto "A7940400-DD17-4B0B-A9B2-565A207D680C")))
    ("k" "Wiki" (lambda () (interactive) (org-roam-extras-id-goto "4373E661-B19D-4E6C-B7DE-C2A26619A515")))
    ("l" "Display" (lambda () (interactive) (org-roam-extras-id-goto "DE6D2307-9EBD-4E0F-B873-003C9813CA27")))
    ("m" "Completion " (lambda () (interactive) (org-roam-extras-id-goto "E83EC00B-0C94-44CD-9EC0-355992C99234")))
    ("n" "Text manipulation" (lambda () (interactive) (org-roam-extras-id-goto "179BB021-8B2A-4BF0-B3AA-43AF5A212D4B")))
    ("p" "Help" (lambda () (interactive) (org-roam-extras-id-goto "7F0CBD06-FDB3-4889-91CE-D8A25D4F2613")))
    ("s" "Search" (lambda () (interactive) (org-roam-extras-id-goto "9FDBBF3E-724F-4402-9DDB-F9349F65AB0E")))
    ("t" "Text movement" (lambda () (interactive) (org-roam-extras-id-goto "1E8F4417-5D5F-4406-BB70-AA272F714EF2")))
    ("u" "user-init" (lambda () (interactive) (org-roam-extras-id-goto "AA460F4A-4035-4C96-A3A1-078A43F7892D")))
    ("v" "Variables" (lambda () (interactive) (org-roam-extras-id-goto "10E891D3-9DF5-472A-8E3C-1DE30EE8C81F")))
    ("y" "Yasnippets" (lambda () (interactive) (org-roam-extras-id-goto "6405B8E7-6612-4D71-8C2C-A51F8808F4C6")))
    ("w" "Windows & frames" (lambda () (interactive) (org-roam-extras-id-goto "7E9A81E0-CAEB-4029-AD2C-B2416439FCDA")))]
   ["Org headings: config.org > org"
    ("o a" "org-agenda" (lambda () (interactive) (org-roam-extras-id-goto "E03F4142-C90D-4550-8990-15391E27AD77")))
    ("o b" "org key bindings" (lambda () (interactive) (org-roam-extras-id-goto "52C959E4-54F4-4499-AE3A-5251F6337FA0")))
    ("o c" "org-capture" (lambda () (interactive) (org-roam-extras-id-goto "14F93A83-0BE7-42E3-891E-F6806192296B")))
    ("o m" "org-roam" (lambda () (interactive) (org-roam-extras-id-goto "2F2E4C1E-4D9B-4A28-B08F-B381E83CFE17")))
    ("o n" "org-noter" (lambda () (interactive) (org-roam-extras-id-goto "A1BA5ED1-BF56-4C33-81F8-19D2AFC7F6D7")))
    ("o o" "org-mode" (lambda () (interactive) (org-roam-extras-id-goto "268B60E4-708C-4372-A59D-5DD876E493CA")))
    ("o f" "org-ref" (lambda () (interactive) (org-roam-extras-id-goto "35FB5BB5-6552-48C6-983A-F90011CCA908")))
    ("o r" "org-refile" (lambda () (interactive) (org-roam-extras-id-goto "3FAE7C0D-FB22-4175-A0A4-FFA392539743")))
    ("o t" "org-cite" (lambda () (interactive) (org-roam-extras-id-goto "8AF25840-AC38-4FF7-A45F-F01B96C5DF5A")))
    ("o x" "org-roam-bibtex" (lambda () (interactive) (org-roam-extras-id-goto "EC73B84D-530E-4179-BB67-F19110A543DF")))]])

;; make it part of the above dispatcher, using prefix key
(transient-define-prefix org-extras-config-org-dispatch ()
  "Jump to an `org-mode' subsection in `config.org'."
  )

;;;;; Patched functions

;; replace `org-files-list' with `org-agenda-files' so that
;; extraneous files are excluded from the dangling clocks check.
(el-patch-defun org-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
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
(with-eval-after-load 'oc
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
			   (funcall insert nil arg))))))))))

;;;; Footer

(provide 'org-extras)

;;; org-extras.el ends here
