;;; org-roam-extras.el --- Extensions for org-roam -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-roam-extras.el
;; Version: 0.2
;; Package-Requires: ((org-roam "2.1") (el-patch "1.0.0") (org-extras "0.1"))

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

;; Extensions for `org-roam'.

;;; Code:

(require 'el-patch)
(require 'org-extras)
(require 'org-roam)
(require 'cl-lib)
(require 'seq)
(require 'paths)

;;;; User options

(defgroup org-roam-extras ()
  "Extensions for `org-roam'."
  :group 'org-roam)

(defcustom org-roam-extras-excluded-dirs
  (list
   paths-dir-inactive
   paths-dir-archive
   paths-dir-dropbox-tlon-leo
   paths-dir-dropbox-tlon-fede
   (file-name-concat paths-dir-dropbox-tlon-fede "archive/"))
  "List of directories to exclude from `org-roam'."
  :type '(repeat directory)
  :group 'org-roam-extras)

(defcustom org-roam-extras-excluded-files
  (list
   paths-file-orb-noter-template
   "quotes-old.org"
   ".org2blog.org"
   "wiki-notes.org" ; removing temporarily
   ".*conflicted copy.*"
   ".*conflicted-copy.*")
  "List of files to exclude from `org-roam'."
  :type '(repeat string)
  :group 'org-roam-extras)

(defcustom org-roam-extras-auto-show-backlink-buffer nil
  "Whether to show the org-roam buffer when opening a file with backlinks."
  :type 'boolean
  :group 'org-roam-extras)

;;;; Main variables

(defvar-local org-roam-extras-current-backlink-count nil)

;;;; Functions

;;;;; Include function

;;;###autoload
(defun org-roam-extras-node-include-function ()
  "Return t iff point is on a valid node."
  (if (or
       ;; exclude based on tags
       (member "noid" (org-get-tags))
       (member "ARCHIVE" (org-get-tags))
       ;; exclude based on heading names
       (member (org-get-heading) '("Local variables"
                                   "COMMENT Local variables"
                                   "TODO Local variables"
                                   "Evaluation"
                                   "History"
                                   "Further reading"
                                   "External links"
                                   "Related entries"
                                   "Archive :ARCHIVE:"))
       ;; exclude buffers when in list of special dirs and org
       ;; heading at point is of level higher than 1 (i.e.
       ;; don't create unnecessary IDs for article
       ;; subsections)
       (and
        ;; dir condition
        (member
         (file-name-directory (buffer-file-name))
         (mapcar #'file-name-as-directory
                 ;; List of special dirs
                 (list
                  paths-dir-journal
                  (file-name-concat paths-dir-dropbox ".Trash")
                  (file-name-concat paths-dir-notes "gptel"))))
        ;; heading condition
        (> (org-current-level) 1))
       )
      nil
    t))

;;;;; Note management

(defun org-roam-extras-new-note ()
  "Create a new `org-roam' note, prompting for its type."
  (interactive)
  (let* ((props (org-roam-extras-get-note-properties))
	 (dir (car props))
	 (tags (cadr props))
	 (name (read-from-minibuffer "Entry name: ")))
    (org-roam-extras-create-file-for-note name dir)
    (insert "#+title: " name "\n\n")
    (org-insert-heading)
    (insert name)
    (org-set-tags tags)
    (org-id-get-create)
    (org-extras-narrow-to-entry-and-children)
    (goto-char (point-max))))

(declare-function simple-extras-slugify "simple-extras")
(defun org-roam-extras-create-file-for-note (note-name &optional dir)
  "Create a file named after NOTE-NAME.
If DIR is nil, use `paths-dir-notes'."
  (let* ((dir (or dir paths-dir-notes))
	 (slug (simple-extras-slugify note-name))
	 (filename (file-name-with-extension slug "org")))
    (when (file-exists-p filename)
      (user-error (format "File `%s' already exists" filename)))
    (find-file (file-name-concat dir filename))))

(defun org-roam-extras-get-note-properties ()
  "Prompt the user to select a note type and return its directory and tags."
  (let ((note-type (completing-read
		    "Select note type: "
		    '("generic" "person" "Borges" "tango" "tlon-notes")))
	(tags)
	(dir))
    (pcase note-type
      ("generic"
       (setq tags "note")
       (setq dir paths-dir-notes))
      ("person"
       (setq tags "person")
       (setq dir paths-dir-people))
      ((or "Borges" "tango" "tlon-notes")
       (setq dir (file-name-concat paths-dir-dropbox note-type))))
    (list dir tags)))

(defun org-roam-extras-convert-heading-to-note ()
  "Convert the heading at point into a note in a separate file."
  (interactive)
  (let ((note-name (read-string "Note name: " (org-get-heading t t t t))))
    (save-restriction
      (org-narrow-to-subtree)
      (kill-region (point-min) (point-max)))
    (org-roam-extras-create-file-for-note note-name default-directory)
    (insert "#+title: " note-name "\n\n")
    (insert (current-kill 0))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (org-edit-headline note-name)))

;;;;; Node find

(defun org-roam-extras-node-find ()
  "Find and open an Org-roam node by its title or alias."
  (interactive)
  (widen)
  (org-extras-fold-show-all-headings)
  (org-roam-node-find)
  (recenter 1))

(autoload 'consult--read "consult")
;;;###autoload
(defun org-roam-extras-node-find-special (&optional filter-spec)
  "Return a list of selected headings sorted by priority.
The selection includes all headings with a priority and either no todo status or
the todo status TODO, and excludes all headings with a date (scheduled or
deadline).

Optional FILTER-SPEC can be:
- nil: no filter applied
- t or prefix arg: prompt user to filter by tag or directory
- (:tag TAG): filter by the specified TAG
- (:dir DIRECTORY): filter by the specified DIRECTORY"
  (interactive "P")
  (let* ((filter-spec (if (or (eq filter-spec t) current-prefix-arg)
                          (let ((filter-type (completing-read "Filter by: " '("tag" "directory"))))
                            (cond
                             ((equal filter-type "tag")
                              (cons :tag (org-roam-extras-node-select-tag)))
                             ((equal filter-type "directory")
                              ;; Use file-truename to resolve symlinks/aliases
                              (list :dir (file-truename (read-directory-name "Select directory: "))))
                             (t nil)))
                        filter-spec))
         ;; Extract tag or directory from the determined filter-spec
         (selection (when (eq (car-safe filter-spec) :tag)
                      (cdr filter-spec)))
         (directory (when (eq (car-safe filter-spec) :dir)
                      ;; Get the directory path from the spec (already canonicalized if interactive)
                      (cadr filter-spec)))
         ;; Build the directory clause for the SQL query if a directory was specified
         (dir-clause (when directory
                       ;; Ensure directory has trailing slash for LIKE pattern
                       `(like nodes:file ,(concat (file-name-as-directory directory) "%"))))
         (headings-with-priority
          (cond
           ;; Filter by tag
           (selection
            (org-roam-db-query `[:select [id file title priority]
                                         :from nodes
                                         :left-join tags
                                         :on (= nodes:id tags:node-id)
                                         :where (and
                                                 (= tags:tag ,selection)
                                                 (notnull nodes:priority)
                                                 (not (notnull nodes:scheduled))
                                                 (not (notnull nodes:deadline))
                                                 (or
                                                  (= nodes:todo "TODO")
                                                  (not (notnull nodes:todo))))
                                         :order-by (asc nodes:priority)]))
           ;; Filter by directory (using the original complex query conditions)
           (dir-clause
            (org-roam-db-query `[:select [id file title priority]
                                         :from nodes
                                         :where (and
                                                 (notnull nodes:priority)
                                                 (not (notnull nodes:scheduled))
                                                 (not (notnull nodes:deadline))
                                                 (or
                                                  (= nodes:todo "TODO")
                                                  (not (notnull nodes:todo)))
                                                 ,dir-clause) ; Add the directory clause
                                         :order-by (asc nodes:priority)]))
           ;; No filter (original complex query without tag/dir)
           (t
            (org-roam-db-query `[:select [id file title priority]
                                         :from nodes
                                         :where (and
                                                 (notnull nodes:priority)
                                                 (not (notnull nodes:scheduled))
                                                 (not (notnull nodes:deadline))
                                                 (or
                                                  (= nodes:todo "TODO")
                                                  (not (notnull nodes:todo))))
                                         :order-by (asc nodes:priority)]))))
         (result '()))
    ;; Process the results
    (dolist (record headings-with-priority)
      (let* ((id (nth 0 record))
             (title (nth 2 record))
             (priority (nth 3 record))
             (formatted-priority (if priority
                                     (format "[#%c] " priority)
                                   ""))
             (formatted-heading (concat formatted-priority title)))
        (push (cons formatted-heading `(lambda ()
					 (org-id-goto ,id)))
              result)))
    (if result
	(let* ((candidates (reverse result))
	       (selection (consult--read
                           candidates
                           :prompt "Jump to heading: "
                           :category 'jump
                           :history t
                           :require-match t
                           :sort nil))
	       (action (cdr (assoc selection candidates))))
          (funcall action))
      (message "No headings found."))))

(defun org-roam-extras-node-select-tag ()
  "Prompt for tag selection from list of org tags."
  (interactive)
  (let* ((query-result (org-roam-db-query [:select :distinct [tag] :from tags]))
	 (tag-candidates (mapcar #'car query-result))
	 (selected-tag (consult--read tag-candidates
				      :prompt "Select a tag: "
				      :history 'org-roam-tag-history
				      :sort t
				      :require-match t)))
    (message "Selected tag: %s" selected-tag)
    selected-tag))

;;;;; Backlinks

;;;;;; backlink count
;; whether the count is shown in the modeline is controlled by the user option
;; `doom-modeline-extras-org-roam', of which see

(defun org-roam-extras-backlink-count ()
  "Return the number of org-roam backlinks for the current buffer."
  (when (derived-mode-p 'org-mode)
    (when-let* ((node-id (org-roam-id-at-point))
		(backlinks (org-roam-db-query
			    [:select [source] :from links :where (= dest $s1)]
			    node-id)))
      (length backlinks))))

(defun org-roam-extras-update-backlink-count ()
  "Update the number of backlinks for the current buffer."
  (setq org-roam-extras-current-backlink-count
	(org-roam-extras-backlink-count)))

(autoload 'doom-modeline-update-buffer-file-name "doom-modeline-segments")
(defun org-roam-extras-update-modeline ()
  "Update the modeline with the number of backlinks for the current buffer."
  (when (derived-mode-p 'org-mode)
    (org-roam-extras-update-backlink-count)
    (doom-modeline-update-buffer-file-name)))

;; (add-hook 'buffer-list-update-hook #'org-roam-extras-update-modeline)

;;;;;; backlink buffer

(defun org-roam-extras-show-backlink-buffer ()
  "Show the org-roam buffer if the current buffer has backlinks."
  (when (and org-roam-extras-auto-show-backlink-buffer
	     (derived-mode-p 'org-mode)
	     (member (org-roam-buffer--visibility) '(exists none)))
    (display-buffer (get-buffer-create org-roam-buffer))
    (org-roam-buffer-persistent-redisplay)))

;; (add-hook 'find-file-hook #'org-roam-extras-show-backlink-buffer)

;;;;; Get people

;;;###autoload
(defun org-roam-extras-get-people ()
  "Query title and ID of level-1 headings in files within the people directory."
  (org-roam-db-query
   [:select [nodes:title nodes:id]
            :from nodes
            :where (and (like nodes:file $s1)
                        (= nodes:level 1))
            :order-by [nodes:title]]
   (file-name-concat paths-dir-people "%")))

;;;;; Misc

(defun org-roam-extras-recent (days &optional limit)
  "Return a list of all files modified in the last DAYS.
Optionally, return such list only if its length is less than LIMIT."
  (let* ((mins (* 60 24 days))
	 (file-list (split-string
		     (shell-command-to-string
		      (format
		       "find %s -name '*.org'  -mmin -%s"
		       (directory-file-name org-roam-directory) mins)))))
    ;; Remove excluded files
    (setq file-list (cl-delete-if (lambda (k)
				    (string-match-p org-roam-file-exclude-regexp k))
				  file-list))
    (if limit
	(when (< (length file-list) limit)
	  file-list)
      file-list)))

(defun org-roam-extras-remove-file-level-properties ()
  "Remove `ROAM_REFS' and `ID' properties from file-level drawer."
  (when (string= "r" (plist-get org-capture-plist :key))
    (goto-char (point-min))
    (unless (org-get-heading)
      ;; Take action with file-level properties only.
      (org-delete-property "ID")
      (org-delete-property "ROAM_REFS")
      (org-extras-jump-to-first-heading)
      (org-id-get-create))))


;;;###autoload
(defun org-roam-extras-id-goto (id)
  "Open ID even if narrowed."
  (widen)
  (org-roam-id-open id nil)
  (widen)
  (org-roam-id-open id nil))

;;;;; Link replacement

;;;###autoload
(defun org-roam-extras-replace-star-links-with-id (dir &optional nocase dirs)
  "Walk through every *.org file inside DIR and turn links of the form
 
     [[*Title][Description]]
 
into
 
     [[id:UUID][Description]]
 
where UUID is the Org-ID of the node whose title is exactly TITLE.
When NOCASE (\\[universal-argument]) is non-nil the title match is
case-insensitive.

The command is completely automatic – it visits the file, performs the
substitutions, and saves the buffer when something changed."
  (interactive "DDirectory with Org files: \nP")
  (dolist (file (directory-files dir t "\\.org\\'"))
    (message "Processing %s" file)
    (when (org-roam-extras--replace-star-links-with-id-in-file file nocase dirs)
      (message "Updated %s" file))))

;;;###autoload
(defun org-roam-extras--replace-star-links-with-id-in-file (file &optional nocase dirs)
  "Replace [[*Title][Desc]] links with [[id:UUID][Desc]] in FILE.
Return non-nil when FILE was modified.  When NOCASE is non-nil the title match
is case-insensitive."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      ;; Collect all matches first to avoid positional drift that can still
      ;; yield `args-out-of-range' when editing the buffer while iterating.
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nocase)
            (matches '())
            (changed nil))
        ;; Record every match with its bounds and replacement text.
        (while (re-search-forward
                "\\[\\[\\*\\([^][[:cntrl:]]+?\\)\\]\\[\\([^][]*?\\)\\]\\]" nil t)
          (let* ((title (match-string-no-properties 1))
                 (desc  (match-string-no-properties 2))
                 (id (ignore-errors
                       (org-roam-extras-get-id-of-title title nocase dirs))))
            (when id
              (push (list (match-beginning 0)
                          (match-end 0)
                          (format "[[id:%s][%s]]" id desc))
                    matches))))
        ;; Perform the recorded replacements using the stored markers.
        ;; Sort from the end of buffer to the beginning so that deletions made
        ;; later do not affect the marker positions of earlier ones.
        (dolist (m (sort matches
                         (lambda (a b)
                           (> (marker-position (car a))
                              (marker-position (car b))))))
          (let* ((beg (marker-position (nth 0 m)))
                 (end (marker-position (nth 1 m)))
                 (replacement (nth 2 m)))
            (when (and beg end (<= end (point-max)))
              (setq changed t)
              (goto-char beg)
              ;; Wrap in a safety check to avoid args-out-of-range.
              (condition-case nil
                  (delete-region beg end)
                (args-out-of-range (setq changed nil)))
              (insert replacement))))
        ;; Clean up markers to avoid memory leaks.
        (dolist (m matches)
          (set-marker (nth 0 m) nil)
          (set-marker (nth 1 m) nil))
        (when changed
          (save-buffer))
        changed))))

;;;###autoload
(defun org-roam-extras-get-id-of-title (title &optional nocase dirs)
  "Return the Org-ID of the node whose TITLE matches exactly.

When NOCASE is non-nil the title comparison is case-insensitive.
If DIRS is a list of directories, only nodes whose source file
resides inside one of those directories (or their subdirectories)
are considered.

An error is signalled when no node (or more than one node)
satisfies these criteria."
  (let* ((rows (org-roam-db-query
                (vconcat
                 [:select [id file] :from nodes :where (= title $s1)]
                 (when nocase [:collate NOCASE]))
                title)))
    ;; Restrict to rows whose file is under one of DIRS, if given
    (when dirs
      ;; Accept a single directory string as well as a list.
      (setq dirs (if (listp dirs) dirs (list dirs)))
      (setq rows
            (cl-remove-if-not
             (lambda (row)
               (let ((file (cadr row)))
                 (seq-some
                  (lambda (dir)
                    (string-prefix-p
                     (file-name-as-directory (file-truename dir))
                     (file-truename file)))
                  dirs)))
             rows)))
    (cond
     ((null rows)
      (user-error "No node with title “%s”" title))
     ((> (length rows) 1)
      (user-error "Multiple nodes share the title “%s”" title))
     (t (caar rows)))))

;;;;; Patched functions

(el-patch-defun org-roam-db-query (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (el-patch-add (sleep-for 0.01))
  (apply #'emacsql (org-roam-db) sql args))

(provide 'org-roam-extras)
;;; org-roam-extras.el ends here
