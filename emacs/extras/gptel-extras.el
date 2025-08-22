;;; gptel-extras.el --- Extensions for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/gptel-extras.el
;; Version: 0.2
;; Package-Requires: ((gptel "0.7.1") (paths "0.1"))

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

;; Extensions for `gptel'.

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'org)
(require 'seq)
(require 'paths)

;;;; User options

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel)

;;;;; Aider

(defcustom gptel-extras-add-repo-map-to-context nil
  "Whether to add the repository map to the context when sending a request."
  :type 'boolean
  :group 'gptel-extras)

(defcustom gptel-extras-add-conventions-to-context nil
  "Whether to add the conventions file to the context when sending a request."
  :type 'boolean
  :group 'gptel-extras)

(defcustom gptel-extras-repo-map-cache-ttl 3600
  "Time-to-live for repository map cache entries, in seconds.
After this time, the cache entry is considered stale and will be regenerated."
  :type 'integer
  :group 'gptel-extras)

(defcustom gptel-extras-repo-map-invalidate-on-git-changes t
  "Whether to invalidate the repository map cache when git changes are detected.
If non-nil, the cache entry is considered stale when the git HEAD reference
changes."
  :type 'boolean
  :group 'gptel-extras)

;;;;; mu4e

(defcustom gptel-extras-mu4e-draft-reply-prompt
  (format "Attached is an email thread. Please draft an appropriate message on my behalf (my name is %s) based on it. If you see a partial draft in the attached buffer, it means I already started it myself, so I want you to finish it. This draft may or may not contain additional instructions for you to follow. Include only the body of the draft reply, skipping the subject and headers. Do not include my name at the end, since a signature is always automatically appended." user-full-name)
  "Prompt to use for drafting a reply to the current message."
  :type 'string
  :group 'gptel-extras)

;;;;; Misc

(defcustom gptel-extras-gemini-mullvad-disconnect-after 1
  "The number of minutes to disconnect `mullvad' after starting the Gemini session."
  :type 'integer
  :group 'gptel-extras)

(defcustom gptel-extras-dir (file-name-concat paths-dir-notes "gptel/")
  "The directory where to save the `gptel' buffers.
In Dired, this directory is sorted chronologically rather than alphabetically,
since the typical use case is to look for a recently modified `gptel' file. This
directory-local sorting is set via a the `.dir-locals.el' file in the directory."
  :type 'directory
  :group 'gptel-extras)

(defcustom gptel-extras-chatgpt-import-dir
  (file-name-concat paths-dir-notes "chatgpt/")
  "The directory where to save the imported ChatGPT conversations."
  :type 'directory
  :group 'gptel-extras)

(defcustom gptel-extras-alert-when-finished '()
  "List of models for which an alert is displayed when the response is inserted.
If t, always display an alert. If nil, never display an alert."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (repeat :tag "Models" symbol))
  :group 'gptel-extras)

;;;;; Search

(defcustom gptel-extras-search-model '("Perplexity" . sonar)
  "The model to use for search queries."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'gptel-extras)

(defcustom gptel-extras-search-prefix "https://www.google.com/search?q="
  "The prefix to use for search queries."
  :type 'string
  :group 'gptel-extras)

;;;; Variables

(defconst gptel-extras-changelog-file
  (file-name-concat paths-dir-dotemacs "extras/gptel-extras-changelog-template.org")
  "The file with the changelog template for `gptel-extras-summarize-commit-diffs'.")

;;;; Functions

;;;;; Summarize commit diffs

(autoload 'magit-commit-at-point "magit-git")
(autoload 'magit-git-insert "magit-git")
(defun gptel-extras-summarize-commit-diffs (beg end &optional include-stats)
  "Summarize the diffs of commits in the selected region using an LLM.
BEG and END mark the region of commits to summarize in `magit-log-mode'. When
INCLUDE-STATS is non-nil (with prefix arg), include diffstats in the prompt."
  (interactive "r\nP")
  (unless (derived-mode-p 'magit-log-mode)
    (user-error "This function is meant to be called from the Magit log (`M-x magit RET ll')"))
  (save-excursion
    (let* ((commits (save-restriction
		      (narrow-to-region beg end)
		      (goto-char (point-min))
		      ;; Get list of selected commit hashes
		      (cl-loop while (not (eobp))
			       collect (magit-commit-at-point)
			       do (forward-line))))
	   ;; Get diffs of all commits
	   (commit-diffs
	    (with-temp-buffer
	      (magit-git-insert "show" "--patch"
				(and (not include-stats) "--unified=3")
				commits)
	      (buffer-string)))
	   ;; Create appropriate prompt
	   (prompt (format
		    "Here are several git commit diffs:\n\n%s\n\nPlease analyze these commits and
provide a concise summary of the main changes. Include any significant patterns \
you notice. Write the summary using org-mode syntax, NOT Markdown. When writing the summary, \
focus on making it useful for someone who is already familiar with the code and \
wants to learn about the changes made in these commits, so that they can quickly \
determine if they need to handle any breaking changes or if they want to start \
using any of the new functionality. Organize the summary into sections, one for \
each package or feature, following this model:\n\n%s"
		    commit-diffs
		    (let ((file gptel-extras-changelog-file))
		      (with-temp-buffer (insert-file-contents file) (buffer-string)))))
	   (gptel-stream t))
      ;; Display summary in new buffer
      (with-current-buffer (get-buffer-create "*Commit Summary*")
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (read-only-mode -1)
	  (insert "Commit Summary:\n"
		  "==============\n\n"
		  (format "Selected commits: %s\n\n"
			  (mapconcat #'identity commits ", ")))
	  (gptel-request
	      prompt
	    :buffer (current-buffer)
	    :position (point)
	    :in-place t
	    :system
	    "You are a software developer's assistant focused on git commit analysis. \
Be concise but thorough when analyzing changes. Group related changes together if \
you notice patterns. If commit messages are included, use them to inform your analysis.")
	  (pop-to-buffer (current-buffer)))))))

;;;;; Activate Mullvad

(declare-function mullvad-connect-to-website "mullvad")
(defun gptel-extras-set-mullvad (orig-fun &rest args)
  "Enable `mullvad' when connecting to Gemini, then call ORIG-FUN with ARGS.
Use to circumvent Gemini’s location restrictions."
  (when (eq gptel-model 'gemini-pro)
    (mullvad-connect-to-website "Gemini"
				gptel-extras-gemini-mullvad-disconnect-after
				'silently))
  (apply orig-fun args))

(advice-add 'gptel-curl-get-response :around #'gptel-extras-set-mullvad)

;;;;; Save buffer

(declare-function simple-extras-slugify "simple-extras")
(autoload 'org-insert-heading "org")
(autoload 'org-next-visible-heading "org")
;;;###autoload
(defun gptel-extras-save-buffer (name _ _ interactivep)
  "Save the `gptel' buffer with NAME right after it is created.
The buffer is saved to a file in `gptel-extras-dir'. INTERACTIVEP is t when
gptel is called interactively.

This function is meant to be an `:after' advice to `gptel'."
  (when interactivep
    ;; do not run if the buffer is visiting a file, because that means the user
    ;; selected an existing buffer
    (unless (buffer-file-name (get-buffer name))
      (switch-to-buffer name)
      (let* ((extension (pcase major-mode
			  ('org-mode "org")
			  ('markdown-mode "md")
			  (_ (user-error "Unsupported major mode"))))
	     (filename (file-name-concat gptel-extras-dir
					 (file-name-with-extension (simple-extras-slugify name) extension))))
	(when (derived-mode-p 'org-mode)
	  (goto-char (point-min))
	  (org-insert-heading nil nil 1)
	  (insert name)
	  (org-next-visible-heading 1)
	  (end-of-line))
	;; we temporarily remove the hook because `gptel--save-state' throws an
	;; error if called at this early stage
	(remove-hook 'before-save-hook #'gptel--save-state t)
	(write-file filename 'confirm)
	(add-hook 'before-save-hook #'gptel--save-state nil t)))))

;;;;; post-response

(autoload 'org-latex-preview "org")
(defun gptel-extras-generate-latex-previews (_ _)
  "Generate LaTeX previews in the current `gptel' buffer."
  (when (and (derived-mode-p 'org-mode)
	     (string= default-directory gptel-extras-dir))
    (org-latex-preview)))

(autoload 'org-fold-show-all "org-fold")
(declare-function files-extras-kill-this-buffer "files-extras")
;;;###autoload
(defun gptel-extras-kill-buffer-then-reopen-file (&optional _ _)
  "Kill the current buffer then reopen the file it was visiting.
This hack command is meant to be added to `gptel-post-response-functions', if
necessary, or called manually, whenever `gptel' starts refusing to process the
request (mysteriously, just killing the buffer and reopening the visited file is
often enough to fix this)."
  (interactive)
  (when-let* ((file (buffer-file-name)))
    (save-buffer)
    (save-buffer)
    (files-extras-kill-this-buffer)
    (find-file file)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-all))))

;;;;; Alert

(declare-function alert "alert")
(defun gptel-extras-alert-when-finished (_ _)
  "Alert the user when `gptel' finishes inserting its response.
START and END are the positions of the inserted response.
To enable this feature, customize `gptel-extras-alert-when-finished'."
  (when (or (eq gptel-extras-alert-when-finished t)
            (memq gptel-model gptel-extras-alert-when-finished))
    (alert "Response inserted" :title "gptel")))

(add-hook 'gptel-post-response-functions #'gptel-extras-alert-when-finished)

;;;;; Continue in new buffer

(defmacro gptel-extras--with-top-level-heading (&rest body)
  "Execute BODY after moving point to the first top-level heading."
  `(org-with-wide-buffer
    (goto-char (point-min))
    (while (not (and
                 (org-at-heading-p)
                 (= (org-current-level) 1)))
      (org-next-visible-heading 1))
    ,@body))

(defun gptel-extras-continue-in-new-buffer ()
  "Continue the conversation in a new buffer with a link from the original.
Create a new buffer with the same heading as the current buffer, but with a
number appended to it. Then insert a link to the new buffer at the end of the
current buffer."
  (interactive)
  (gptel-extras-ensure-gptel-mode)
  (let* ((original-buffer (current-buffer))
         (heading (gptel-extras--with-top-level-heading
		   (substring-no-properties (org-get-heading t t t t))))
         (new-buffer-name (gptel-extras--generate-next-heading heading)))
    (gptel new-buffer-name nil nil t)
    (let ((new-buffer-id (gptel-extras--with-top-level-heading
			  (org-id-get-create))))
      (with-current-buffer original-buffer
        (gptel-extras--insert-continuation-link new-buffer-id)))))

(defun gptel-extras--generate-next-heading (heading)
  "Generate the next heading based on HEADING, incrementing any number suffix."
  (if (string-match "\\(.*\\) \\([0-9]+\\)$" heading)
      (format "%s %d"
              (match-string 1 heading)
              (1+ (string-to-number (match-string 2 heading))))
    (concat heading " 2")))

(defun gptel-extras--insert-continuation-link (buffer-id)
  "Insert a link to continuation with BUFFER-ID at the end of the current buffer."
  (save-excursion
    (goto-char (point-max))
    (insert (format "Continued [[id:%s][here]]" buffer-id))))

;;;;; Aider

(defvar gptel-extras--repo-map-cache (make-hash-table :test 'equal)
  "Cache for repository maps.
Keys are repository paths, values are cons cells of the form (TIMESTAMP . MAP).")

(defconst gptel-extras-repo-map-buffer-name-formatter "*Repo Map: %s*"
  "The buffer where the repository map is shown.")

(autoload 'project-name "project")
(autoload 'project-root "project")
(defun gptel-extras-get-repo-map-buffer-name ()
  "Return the name of the repository map buffer."
  (let* ((repo (gptel-extras-get-repo))
	 (name (project-name (project-current nil repo))))
    (format gptel-extras-repo-map-buffer-name-formatter name)))

(defun gptel-extras-get-repo ()
  "Return the repository for the current request."
  (let ((prompt (lambda () (expand-file-name
		       (read-directory-name "Repository: " paths-dir-personal-repos)))))
    (pcase major-mode
      ('org-mode
       (or (org-entry-get (point-min) "GPTEL_REPO")
	   (let ((repo (funcall prompt)))
	     (org-entry-put (point-min) "GPTEL_REPO" repo)
	     repo)))
      ;; TODO: Add markdown-mode support
      ('markdown-mode)
      (_ (if-let* ((project (project-current)))
	     (project-root project)
	   (funcall prompt))))))

(defun gptel-extras-get-git-head-ref (repo)
  "Return the current git HEAD reference for REPO."
  (let ((default-directory repo))
    (when (file-exists-p ".git")
      (string-trim (shell-command-to-string "git rev-parse HEAD")))))

(defun gptel-extras-repo-map-cache-valid-p (repo)
  "Check if the cached repository map for REPO is still valid."
  (when-let* ((cache-entry (gethash repo gptel-extras--repo-map-cache)))
    (and
     ;; Check if the cache entry is still fresh
     (when-let ((timestamp (cdr (assoc 'timestamp cache-entry))))
       (< (- (float-time) timestamp) gptel-extras-repo-map-cache-ttl))
     ;; Check if git HEAD has changed (if enabled)
     (or (not gptel-extras-repo-map-invalidate-on-git-changes)
         (let ((cached-head (cdr (assoc 'git-head cache-entry)))
               (current-head (gptel-extras-get-git-head-ref repo)))
           (or (not current-head) ; Not a git repo
               (and cached-head (string= cached-head current-head))))))))

(defun gptel-extras-get-repo-map ()
  "Return a map of current repository, if available.
Uses cached version if available and not stale."
  (let* ((repo (gptel-extras-get-repo))
         (cache-entry (gethash repo gptel-extras--repo-map-cache)))
    (if (and cache-entry (gptel-extras-repo-map-cache-valid-p repo))
        ;; Return cached map
        (cdr (assoc 'map cache-entry))
      ;; Generate new map
      (when (and (project-current nil repo)
                 (executable-find "aider"))
        (let* ((default-directory repo)
               (map (shell-command-to-string "aider --show-repo-map"))
               (git-head (gptel-extras-get-git-head-ref repo))
               (new-cache-entry `((timestamp . ,(float-time))
                                  (git-head . ,git-head)
                                  (map . ,map))))
          ;; Update cache
          (puthash repo new-cache-entry gptel-extras--repo-map-cache)
          map)))))

(defun gptel-extras-create-repo-map ()
  "Create and return a repo map buffer for the current project."
  (when-let* ((repo-map (gptel-extras-get-repo-map)))
    (with-current-buffer (get-buffer-create (gptel-extras-get-repo-map-buffer-name))
      (erase-buffer)
      (insert repo-map)
      (goto-char (point-min))
      (current-buffer))))

(defun gptel-extras-show-repo-map ()
  "Show the repository map in a new buffer."
  (interactive)
  (when-let* ((buffer (gptel-extras-create-repo-map)))
    (pop-to-buffer buffer)))

(defun gptel-extras-add-repo-map-to-context ()
  "Add the repository map to the gptel context."
  (interactive)
  (when-let* ((buffer (gptel-extras-create-repo-map)))
    (gptel-extras-remove-repo-map-from-context)
    (with-current-buffer buffer
      (insert "This is repository map of the current project. Please use it to get a sense of the project structure and to request any files you think might help you answer the prompt.\n\n")
      (gptel-context-add))))

(defun gptel-extras-add-conventions-to-context ()
  "Add the conventions file to the gptel context."
  (interactive)
  (let* ((default-directory (gptel-extras-get-repo))
	 (file (file-name-concat default-directory "conventions.md")))
    (gptel-context-add-file file)))

(defun gptel-extras-remove-repo-map-from-context ()
  "Remove the repository map from the gptel context."
  (interactive)
  (when-let ((buffer (get-buffer (gptel-extras-get-repo-map-buffer-name))))
    (setq gptel-context--alist (assq-delete-all buffer gptel-context--alist))))

(defun gptel-extras-invalidate-repo-map-cache (&optional repo)
  "Invalidate the repository map cache for REPO.
If REPO is nil, use the current repository."
  (interactive)
  (let ((repo (or repo (gptel-extras-get-repo))))
    (remhash repo gptel-extras--repo-map-cache)
    (message "Repository map cache invalidated for %s" repo)))

(defun gptel-extras-add-context-files (&optional _)
  "Add relevant context files to the `gptel' context.
The files added is controlled by the user options
`gptel-extras-add-repo-map-to-context' and
`gptel-extras-add-conventions-to-context'."
  (when gptel-extras-add-repo-map-to-context
    (gptel-extras-add-repo-map-to-context))
  (when gptel-extras-add-conventions-to-context
    (gptel-extras-add-conventions-to-context)))

(advice-add 'gptel-send :before #'gptel-extras-add-context-files)

(defun gptel-extras-toggle-aider-files ()
  "Toggle inclusion or exclusion of Aider files."
  (interactive)
  (let ((state gptel-extras-add-conventions-to-context))
    (setq gptel-extras-add-conventions-to-context (not state)
	  gptel-extras-add-repo-map-to-context (not state))
    (message (concat (if state "Diabled" "Enabled") " Aider files."))))

;;;;; ChatGPT Import

;;;###autoload
(defun gptel-extras-import-chatgpt-conversations (json-file &optional include-archived)
  "Import ChatGPT conversations from JSON-FILE into Org files.
This command processes the \"conversations.json\" file included in the ChatGPT
export, importing the text part of ChatGPT conversations only and ignoring
media, metadata and other non-textual parts. The conversations are saved in
`gptel-extras-chatgpt-import-dir' as Org files, with the title of each
conversation as the file name. The title is slugified to ensure it is a valid
file name.

If INCLUDE-ARCHIVED is non-nil, or the command is called with a prefix argument,
include archived conversations in the import."
  (interactive "fImport ChatGPT conversations from file: \nP")
  (unless (file-exists-p gptel-extras-chatgpt-import-dir)
    (make-directory gptel-extras-chatgpt-import-dir t))
  (let ((json-object-type 'hash-table)
        (json-array-type  'list))
    (let* ((conversations (gptel-extras--filter-conversations json-file include-archived))
	   (total (length conversations)))
      (dotimes (i total)
        (let* ((conv      (elt conversations i))
               (title     (or (gethash "title" conv) "Untitled"))
               (mapping   (gethash "mapping" conv))
               (conv-id   (or (gethash "conversation_id" conv)
                              (gethash "id" conv)))
               (file-name (concat gptel-extras-chatgpt-import-dir
                                  (simple-extras-slugify title) ".org"))
               (messages  (gptel-extras--collect-message-nodes mapping)))
          (setq messages (gptel-extras--sort-messages-by-timestamp messages))
          (gptel-extras--write-conversation-to-file title conv-id messages file-name)
	  (message "Imported \"%s\" to %s (%d/%d)" title file-name (1+ i) total))))))

(defun gptel-extras--filter-conversations (json-file include-archived)
  "Filter conversations in JSON-FILE based on INCLUDE-ARCHIVED flag.
If INCLUDE-ARCHIVED is non-nil, include all conversations.
Otherwise, exclude archived conversations."
  (seq-filter
   (lambda (conv)
     (or include-archived
         ;; keep if flag is missing or explicitly json-false
         (not (gethash "is_archived" conv))
         (eq (gethash "is_archived" conv) json-false)))
   (json-read-file json-file)))

;;;###autoload
(defun gptel-extras--collect-message-nodes (mapping)
  "Collect message nodes from MAPPING hash table."
  (let ((messages '()))
    (maphash
     (lambda (_ node)
       (let ((msg (gethash "message" node)))
         (when (and msg
                    (gethash "content" msg)
                    (member (gethash "role" (gethash "author" msg))
                            '("user" "assistant")))
           (push msg messages))))
     mapping)
    messages))

(defun gptel-extras--sort-messages-by-timestamp (messages)
  "Sort MESSAGES safely by timestamp."
  (sort messages
        (lambda (a b)
          (< (or (gethash "create_time" a) most-positive-fixnum)
             (or (gethash "create_time" b) most-positive-fixnum)))))

(defun gptel-extras--write-conversation-to-file (title conv-id messages file-name)
  "Write conversation with TITLE, CONV-ID and MESSAGES to FILE-NAME."
  (with-temp-buffer
    (insert (format "#+title: %s\n\n" title))
    (insert (format "* %s\n" title))
    (when conv-id
      (insert (format "[[https://chatgpt.com/c/%s][Open in ChatGPT]]\n\n" conv-id)))
    (dolist (msg messages)
      (let* ((role (gethash "role" (gethash "author" msg)))
             (content (gethash "content" msg))
             (parts (and content (gethash "parts" content)))
             (texts (seq-filter #'identity
                                (gptel-extras--extract-parts-text parts))))
        (when (and role texts)
          (insert (format "** %s\n\n%s\n\n"
                          (capitalize role)
                          (org-extras-convert-markdown-to-org
                           (mapconcat #'identity texts "\n\n")))))))
    (write-region (point-min) (point-max) file-name)))

(declare-function simple-extras-pandoc-convert "simple-extras")
(defun org-extras-convert-markdown-to-org (markdown)
  "Convert MARKDOWN string to Org using Pandoc and postprocess cleanup."
  (let ((output (simple-extras-pandoc-convert "org" "markdown" markdown)))
    (with-temp-buffer
      (insert output)
      (dolist (regexp '(("^\\\\\\\\" . "")
                        ("\\\\\\\\$" . "")
                        (" " . " ")))
        (goto-char (point-min))
        (while (re-search-forward (car regexp) nil t)
          (replace-match (cdr regexp) nil nil)))
      (buffer-string))))

(defun gptel-extras--extract-parts-text (parts)
  "Extract text strings from a list of PARTS (which may be strings or hash-tables)."
  (mapcar (lambda (p)
            (cond
             ((stringp p) p)
             ((and (hash-table-p p)
                   (gethash "text" p))
              (gethash "text" p))
             (t nil)))
          parts))

;;;;; Tools

;;;;;; Presets

(defmacro gptel-extras-make-tool-presets (name description tools)
  "Create two gptel presets for TOOLS with NAME and DESCRIPTION.
Create a base preset named NAME that sets the tools to TOOLS, and an additive
preset named NAME+ that appends TOOLS to existing tools. TOOLS is a list of tool
name strings. NAME is a symbol for the preset name (e.g., `tools-web-search').
DESCRIPTION is a string describing the tools (e.g., \"web search tools\")."
  (let* ((name-str (symbol-name name))
         (base-symbol name)
         (additive-symbol (intern (concat name-str "+")))
         (base-desc (concat "Set tools to " description))
         (additive-desc (concat "Add " description " to existing tools"))
	 (gptel-use-tools t)
	 (actual-tools (if (and (consp tools)
				(eq (car tools) 'quote)
				(consp (cdr tools))
				(listp (cadr tools)))
			   (cadr tools)
			 tools)))
    `(progn
       (gptel-make-preset ',base-symbol
	 :description ,base-desc
	 :tools ',actual-tools)
       (gptel-make-preset ',additive-symbol
	 :description ,additive-desc
	 :tools '(:append ,@actual-tools)))))

;;;;;; Filesystem tools

;;;;;;; edit_file

;; https://github.com/munen/emacs.d?tab=readme-ov-file#edit_file
(defun gptel-extras-edit-file (file-path &optional file-edits)
  "Edit FILE-PATH by applying FILE-EDITS using fuzzy string matching.

This function directly modifies the file on disk without user confirmation. Each
edit in FILE-EDITS should specify:

- `:old_string': the string to find and replace (fuzzy matched)

- `:new_string': the replacement string

EDITING RULES:

- The `old_string' is matched using fuzzy search throughout the file.

- If multiple matches exist, only the first occurrence is replaced.

- Include enough context in `old_string' to uniquely identify the location.

- Whitespace differences are normalized during matching.

- Keep edits focused on the specific change requested.

Returns a success/failure message indicating whether edits were applied."
  (if (and file-path (not (string= file-path "")) file-edits)
      (with-current-buffer (get-buffer-create "*edit-file*")
        (erase-buffer)
        (insert-file-contents (expand-file-name file-path))
        (let ((inhibit-read-only t)
              (target-file-name (expand-file-name file-path))
              (edit-success nil)
              (applied-edits 0)
              (total-edits (length file-edits)))
          ;; Apply changes
          (dolist (file-edit (seq-into file-edits 'list))
            (when-let ((old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string))
                       (is-valid-old-string (not (string= old-string ""))))
              (goto-char (point-min))
              ;; Try exact match first
              (if (search-forward old-string nil t)
                  (progn
                    (replace-match new-string t t)
                    (setq edit-success t)
                    (setq applied-edits (1+ applied-edits)))
                ;; If exact match fails, try fuzzy match
                (goto-char (point-min))
                (when (gptel-extras-fuzzy-search old-string)
                  (replace-match new-string t t)
                  (setq edit-success t)
                  (setq applied-edits (1+ applied-edits))))))
          ;; Return result
          (if edit-success
              (progn
                (write-file target-file-name nil)
                (kill-buffer (current-buffer))
                (format "Successfully edited and saved %s (%d/%d edits applied)"
                        target-file-name applied-edits total-edits))
            (progn
              (kill-buffer (current-buffer))
              (format "Failed to apply edits to %s. No matching strings found." target-file-name)))))
    (format "Failed to edit %s (invalid path or no edits provided)." file-path)))

(defun gptel-extras-normalize-whitespace (string)
  "Normalize whitespace in STRING for fuzzy matching.
Converts multiple whitespace characters to single spaces and trims."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " string)))

(defun gptel-extras-fuzzy-search (target-string)
  "Search for TARGET-STRING using fuzzy matching.
Returns t if found and positions point after the match, nil otherwise.
Tries multiple matching strategies in order of preference."
  (let ((normalized-target (gptel-extras-normalize-whitespace target-string))
        (case-fold-search nil))
    (or
     ;; Strategy 1: Normalized whitespace matching
     (progn
       (goto-char (point-min))
       (let ((found nil))
         (while (and (not found) (not (eobp)))
           (let* ((line-start (line-beginning-position))
                  (line-end (line-end-position))
                  (line-text (buffer-substring-no-properties line-start line-end))
                  (normalized-line (gptel-extras-normalize-whitespace line-text)))
             (when (string-match-p (regexp-quote normalized-target) normalized-line)
               ;; Found a match, now find the actual position in the original text
               (goto-char line-start)
               (when (re-search-forward
                      (gptel-extras-create-fuzzy-regex target-string)
                      line-end t)
                 (setq found t)))
             (unless found (forward-line 1))))
         found))
     ;; Strategy 2: Case-insensitive search
     (progn
       (goto-char (point-min))
       (let ((case-fold-search t))
         (search-forward target-string nil t)))
     ;; Strategy 3: Regex-based flexible matching
     (progn
       (goto-char (point-min))
       (re-search-forward (gptel-extras-create-flexible-regex target-string) nil t)))))

(defun gptel-extras-create-fuzzy-regex (string)
  "Create a regex pattern for fuzzy matching STRING.
Allows flexible whitespace matching between words."
  (let ((escaped (regexp-quote string)))
    ;; Replace escaped whitespace with flexible whitespace pattern
    (replace-regexp-in-string
     "\\\\[ \t\n\r]+"
     "[ \t\n\r]+"
     escaped)))

(defun gptel-extras-create-flexible-regex (string)
  "Create a very flexible regex pattern for STRING.
Allows optional whitespace between characters and words."
  (let* ((chars (string-to-list string))
         (pattern-parts '()))
    (dolist (char chars)
      (cond
       ((memq char '(?\s ?\t ?\n ?\r))
        ;; For whitespace, allow flexible matching
        (push "[ \t\n\r]*" pattern-parts))
       (t
        ;; For regular characters, escape and add optional whitespace after
        (push (concat (regexp-quote (char-to-string char)) "[ \t\n\r]*")
              pattern-parts))))
    (concat "\\(" (string-join (reverse pattern-parts) "") "\\)")))

(gptel-make-tool
 :function #'gptel-extras-edit-file
 :name "edit_file"
 :description "Edits a file by applying fuzzy string matching changes. This is a primary method for file modification.

If this tool fails, try 'apply_diff'. As a last resort, use 'replace_file_contents'.
This tool modifies the file directly on disk without user confirmation.

Each edit requires an old string to find (fuzzy matched) and a new string to replace it with."
 :args (list '(:name "file-path"
                     :type string
                     :description "The full path of the file to edit.")
             '(:name "file-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:old_string
                                    (:type string :description "The exact string to be replaced (will be fuzzy matched if exact match fails).")
                                    :new_string
                                    (:type string :description "The new string to replace old_string.")))
                     :description "A list of edits to apply to the file. Each edit must contain old_string and new_string."))
 :category "filesystem"
 :include t)

;;;;;;; apply_diff

;; https://github.com/munen/emacs.d?tab=readme-ov-file#apply_diff
(gptel-make-tool
 :name "apply_diff"
 :description (concat
               "Applies a diff (patch) to a file. This is a primary method for file modification. "
               "If this tool fails, try 'edit_file'. As a last resort, use 'replace_file_contents'. "
               "The diff must be in the unified format ('diff -u'). "
               "Ensure file paths in the diff (e.g., '--- a/file', '+++ b/file') match the 'file_path' argument and 'patch_options'. "
               "Common 'patch_options' include: '' (for exact/relative paths), "
               "'-p0' (if diff paths are full), '-p1' (to strip one leading directory). "
               "Default options are '-N' (ignore already applied patches).")
 :args (list
        '(:name "file_path"
                :type string
                :description "The path to the file that needs to be patched.")
        '(:name "diff_content"
                :type string
                :description "The diff content in unified format (e.g., from 'diff -u').")
        '(:name "patch_options"
                :type string
                :optional t
                :description "Optional: Additional options for the 'patch' command (e.g., '-p1', '-p0', '-R'). Defaults to '-N'. Prepend other options if needed, e.g., '-p1 -N'.")
        '(:name "working_dir"
                :type string
                :optional t
                :description "Optional: The directory in which to interpret file_path and run patch. Defaults to the current buffer's directory if not specified."))
 :category "filesystem"
 :function
 (lambda (file_path diff_content &optional patch_options working_dir)
   (let ((original-default-directory default-directory)
         (user-patch-options (if (and patch_options (not (string-empty-p patch_options)))
                                 (split-string patch_options " " t)
                               nil))
         ;; Combine user options with -N, ensuring -N is there.
         ;; If user provides -N or --forward, use their version. Otherwise, add -N.
         (base-options '("-N"))
         (effective-patch-options '()))

     (if user-patch-options
         (if (or (member "-N" user-patch-options) (member "--forward" user-patch-options))
             (setq effective-patch-options user-patch-options)
           (setq effective-patch-options (append user-patch-options base-options)))
       (setq effective-patch-options base-options))

     (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
            (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
            (target-file nil)
            (exit-status -1) ; Initialize to a known non-zero value
            (result-output "")
            (result-error ""))
       (unwind-protect
           (progn
             (when (and working_dir (not (string-empty-p working_dir)))
               (setq default-directory (expand-file-name working_dir)))

             (setq target-file (expand-file-name file_path))

             (unless (file-exists-p target-file)
               ;; Use error to signal failure, which gptel should catch.
               (error "File to patch does not exist: %s" target-file))

             (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file effective-patch-options)
               (with-temp-buffer
                 (insert diff_content)
                 (unless (eq (char-before (point-max)) ?\n)
                   (goto-char (point-max))
                   (insert "\n"))

                 ;; Pass buffer *names* to call-process-region
                 (setq exit-status (apply #'call-process-region
                                          (point-min) (point-max)
                                          "patch"       ; Command
                                          nil           ; delete region (no)
                                          (list out-buf-name err-buf-name) ; stdout/stderr buffer names
                                          nil           ; display (no)
                                          (append effective-patch-options (list target-file))))))

             ;; Retrieve content from buffers using their names
             (let ((stdout-buf (get-buffer out-buf-name))
                   (stderr-buf (get-buffer err-buf-name)))
               (when stdout-buf
                 (with-current-buffer stdout-buf
                   (setq result-output (buffer-string))))
               (when stderr-buf
                 (with-current-buffer stderr-buf
                   (setq result-error (buffer-string)))))

             (if (= exit-status 0)
                 (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                         target-file effective-patch-options result-output result-error)
               ;; Signal an Elisp error, which gptel will catch and display.
               ;; The arguments to 'error' become the error message.
               (error "Failed to apply diff to %s (exit status %s).\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                      target-file exit-status effective-patch-options result-output result-error)))
         ;; Cleanup clause of unwind-protect
         (setq default-directory original-default-directory)
         (let ((stdout-buf-obj (get-buffer out-buf-name))
               (stderr-buf-obj (get-buffer err-buf-name)))
           (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
           (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))
 :include t)

;;;;;;; replace_file_contents

;; https://github.com/munen/emacs.d?tab=readme-ov-file#misc
(gptel-make-tool
 :function (lambda (file_path new_content)
             (with-temp-message (format "Replacing content in file: `%s`" file_path)
               (let ((full-path (expand-file-name file_path)))
                 (with-temp-file full-path
                   (insert new_content))
                 (format "Successfully replaced content in %s" full-path))))
 :name "replace_file_contents"
 :description "Replaces the entire content of a file. Use this tool ONLY as a last resort if both 'edit_file' and 'apply_diff' fail. It is highly token-inefficient as it requires sending the full file content. WARNING: This operation completely overwrites the target file."
 :args (list
        '(:name "file_path"
                :type string
                :description "The path to the file that needs to be replaced.")
        '(:name "new_content"
                :type string
                :description "The new content for the file."))
 :category "filesystem"
 :include t)

;;;;;; BibTeX tools

(declare-function citar--format-candidates "citar")
(defun gptel-extras-citar-search (search-string &optional limit offset)
  "Search bibliography for SEARCH-STRING and return matching results.

This is a non-interactive search function intended for programmatic use.

SEARCH-STRING is the string to search for.
LIMIT, if non-nil, is the maximum number of results to return. Defaults to 10.
Use -1 for no limit.
OFFSET, if non-nil, is the starting position in the list of matches.

Returns a list of pairs (FORMATTED-STRING . CITE-KEY). FORMATTED-STRING is
the human-readable representation of a search result. CITE-KEY is the
corresponding citation key.

If no matches are found, returns nil."
  (let* ((candidates (or (citar--format-candidates)
                         (user-error "No bibliography set")))
         (matches (let ((found '())
                        (case-fold-search t)
                        (terms (split-string search-string nil t)))
                    (maphash (lambda (k _v)
                               (when (cl-every (lambda (term)
                                                 (string-match-p (concat "\\b" (regexp-quote term) "\\b") k))
                                               terms)
                                 (push k found)))
                             candidates)
                    (nreverse found))))
    (when matches
      (let* ((limit (cond ((null limit) 10)
                          ((and (numberp limit) (= limit -1)) nil)
                          (t limit)))
             (start (or offset 0))
             (end (when limit (min (+ start limit) (length matches))))
             (results (seq-subseq matches start end)))
        (mapcar (lambda (res) (cons (substring-no-properties res) (gethash res candidates))) results)))))

(gptel-make-tool
 :function #'gptel-extras-citar-search
 :name "search_bibliography"
 :description "Search the user's bibliography. The search string is split into space-separated terms, and the search returns entries that match all terms as whole words. Returns a list of matching entries, with pagination support. Each entry is a pair of [formatted citation, citation key]. To narrow down results, consider using the title of the work, if you know it, or combining multiple fields (such as 'ord 2020' for all works by Ord published in 2020). If you want to search for an author, use the last name only, since the search tool does not support full names. The search is case-insensitive."
 :args (list '(:name "search-string"
		     :type string
		     :description "One or more space-separated terms to search for in the bibliography.")
             '(:name "limit"
		     :type integer
		     :optional t
		     :description "The maximum number of results to return. Defaults to 10. Use -1 for no limit.")
             '(:name "offset"
		     :type integer
		     :optional t
		     :description "The starting position in the list of matches for pagination."))
 :category "bibtex")

(defvar zotra-extras-most-recent-bibkey)
(declare-function zotra-extras-add-entry "zotra-extras")
(defun gptel-extras-add-bib-entry (identifier bibfile)
  "Add bibliographic entry for IDENTIFIER to BIBFILE and return the entry’s bibkey.
IDENTIFIER can be a URL, ISBN, or DOI. This function calls
`zotra-extras-add-entry' with nil as the second argument and t as the fourth
argument."
  (zotra-extras-add-entry identifier nil bibfile t)
  zotra-extras-most-recent-bibkey)

(gptel-make-tool
 :function #'gptel-extras-add-bib-entry
 :name "add_bib_entry"
 :description "Adds a bibliographic entry for a URL, ISBN, or DOI to a BibTeX file."
 :args (list '(:name "identifier"
		     :type string
		     :description "The URL, ISBN, or DOI of the item to add.")
             '(:name "bibfile"
		     :type string
		     :description "The path to the BibTeX file to add the entry to."))
 :category "bibtex")

;;;;; mu4e

(declare-function org-msg-extras-begin-compose "org-msg-extras")
;;;###autoload
(defun gptel-extras-mu4e-draft-reply (&optional edit-prompt)
  "Draft a reply to the current message and insert it at point.
Use `gptel-extras-mu4e-draft-reply-prompt' as the prompt, If EDIT-PROMPT is
non-nil, or the command is called with a prefix argument, allow the user to edit
this prompt."
  (interactive "P")
  (unless (bound-and-true-p mu4e-compose-parent-message)
    (user-error "You do not currently appear to be replying to a message"))
  (gptel-extras-warn-when-context)
  (gptel-extras-add-buffer)
  (let ((prompt (if edit-prompt
		    (read-string "Prompt: " gptel-extras-mu4e-draft-reply-prompt)
		  gptel-extras-mu4e-draft-reply-prompt)))
    (gptel-request prompt
      :transforms (cons 'gptel--transform-add-context gptel-prompt-transform-functions))
    (gptel-context-remove-all)))

;;;;; Misc

;;;###autoload
(defun gptel-extras-set-backend-and-model (&optional backend model)
  "Set the model and backend for the current `gptel' buffer.
If MODEL is nil, use `gptel-model'. If BACKEND is nil, use `gptel-backend'."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer"))
  (setq-local gptel-backend
	      (if backend
		  (alist-get backend gptel--known-backends nil nil #'string=)
		gptel-backend))
  (setq-local gptel-model
	      (or model gptel-model)))

(defun gptel-extras-run-query (query &optional backend model)
  "Prompt for QUERY with MODEL and BACKEND and run it in a gptel buffer.
If MODEL and BACKEND are nil, use the default model and backend."
  (let* ((buffer-name (file-name-with-extension (simple-extras-slugify query)
						(pcase gptel-default-mode
						  ('markdown-mode "md")
						  ('org-mode "org")))))
    (gptel query nil nil t)
    (with-current-buffer buffer-name
      (gptel-extras-set-backend-and-model backend model))
    (goto-char (point-max))
    (gptel-request query
      :buffer (current-buffer)
      :position (point)
      :in-place t)))

;;;###autoload
(defun gptel-extras-search-and-ask-model (query)
  "Prompt for QUERY, search it externally, and ask a configured AI model via gptel.
Opens the search results in a browser using `gptel-extras-search-prefix'.
Creates a new gptel buffer, sets the model and backend according to
`gptel-extras-search-model' buffer-locally, and sends the QUERY to that model."
  (interactive (list (progn
		       (gptel-extras-warn-when-context)
		       (read-string "Search query: "))))
  (let ((browse-url-browser-function 'browse-url-chrome))
    (browse-url (concat gptel-extras-search-prefix (url-hexify-string query))))
  (gptel-extras-run-query query
			  (car gptel-extras-search-model)
			  (cdr gptel-extras-search-model)))

;;;###autoload
(defun gptel-extras-toggle-major-mode ()
  "Toggle between `markdown-mode' and `org-mode'."
  (interactive)
  (setq gptel-default-mode
	(pcase gptel-default-mode
	  ('markdown-mode 'org-mode)
	  ('org-mode 'markdown-mode)))
  (message "Switched to %s." gptel-default-mode))

;;;###autoload
(defun gptel-extras-goto-end-and-send ()
  "Go to the end of the buffer and send the prompt."
  (interactive)
  (goto-char (point-max))
  (gptel-send))

(declare-function gptel-rewrite "gptel-rewrite")
;;;###autoload
(defun gptel-extras-rewrite-defun ()
  "Rewrite the current function definition, marking it first.
This command is meant to be invoked via `embark'; simply binding `gptel-rewrite'
won't work because it needs the function to be selected."
  (interactive)
  (if (thing-at-point 'defun t)
      (progn (mark-defun)
             (gptel-rewrite))
    (user-error "Not on a function definition")))

(defun gptel-extras-fix-garbled-chars ()
  "Replace `let/' and `=(' with proper `let*' and `\`(' from point to end of buffer."
  (interactive)
  (save-excursion
    (while (search-forward "let/" nil t)
      (replace-match "let*"))
    (goto-char (point))
    (while (search-forward "=(" nil t)
      (replace-match "`("))))

(defun gptel-extras-ensure-gptel-mode ()
  "Throw an error unless `gptel-mode' is non-nil in the current buffer."
  (unless gptel-mode
    (user-error "Not in a `gptel' buffer")))

;;;###autoload
(defun gptel-extras-add-buffer (&optional buffer)
  "Add entire BUFFER to gptel context.
When called interactively, add the current buffer.  If BUFFER is
non-nil, it should be a buffer object whose whole contents will be
added as a single context chunk."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (gptel-context--add-region buf
                               (with-current-buffer buf (point-min))
                               (with-current-buffer buf (point-max))
                               t)
    (message "Buffer '%s' added as context." (buffer-name buf))))

;;;###autoload
(defun gptel-extras-warn-when-context ()
  "Prompt for confirmation to proceed when `gptel' context is not empty."
  (unless (or (null gptel-context--alist)
	      (y-or-n-p "The `gptel' context is not empty. Proceed? "))
    (let ((message "Aborted"))
      (when (y-or-n-p "Clear the `gptel' context? ")
	(gptel-context-remove-all)
	(setq message (concat message " (context cleared)")))
      (user-error message))))

(provide 'gptel-extras)
;;; gptel-extras.el ends here
