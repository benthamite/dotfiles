;;; forge-extras.el --- Extensions for forge -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/forge-extras.el
;; Version: 0.2
;; Package-Requires: ((forge "0.3.1") (shut-up "0.3.1"))

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

;; Extensions for `forge'.

;;; Code:

(require 'forge)
(require 'shut-up)
(require 'json)
(require 'seq)
(require 'cl-lib)

;;;; User options

(defgroup forge-extras ()
  "Extensions for `forge'."
  :group 'forge-extras)

(defcustom forge-extras-project-owner nil
  "The GitHub owner (organization or user) for the target project."
  :type 'string
  :group 'forge-extras)

(defcustom forge-extras-project-number nil
  "The GitHub Project number to use for fetching/setting issue status."
  :type 'integer
  :group 'forge-extras)

(defcustom forge-extras-project-node-id nil
  "The global Relay Node ID of the target GitHub project."
  :type 'string
  :group 'forge-extras)

(defcustom forge-extras-status-field-node-id nil
  "The global Relay Node ID of the \"Status\" field in project."
  :type 'string
  :group 'forge-extras)

(defcustom forge-extras-estimate-field-node-id nil
  "The global Relay Node ID of the \"Estimate\" field in project."
  :type 'string
  :group 'forge-extras)

(defcustom forge-extras-status-option-ids-alist nil
  "Alist mapping status names to Option IDs for the \"Status\" field in project."
  :type '(alist :key-type string :value-type string)
  :group 'forge-extras)

(defcustom forge-extras-issue-link-state "open"
  "Which issues to retrieve with `gh issue list`.
Allowed values are \"open\", \"closed\" or \"all\"."
  :type '(choice (const "open") (const "closed") (const "all"))
  :group 'forge-extras)


;;;; Functions

(declare-function org-store-link "ol")
(defun forge-extras-orgit-store-link (_arg)
  "Like `org-store-link' but store links to all selected commits, if any."
  (interactive "P")
  (if-let* ((sections (magit-region-sections 'commit)))
      (save-excursion
        (dolist (section sections)
          (goto-char (oref section start))
          (set-mark (point))
          (activate-mark)
          (call-interactively #'org-store-link))
        (deactivate-mark))
    (save-window-excursion
      (let ((topic (forge-topic-at-point)))
        (cond ((forge-pullreq-p topic)
               (forge-visit-pullreq topic))
              ((forge-issue-p topic)
               (forge-visit-issue topic)))
        (call-interactively #'org-store-link)))))

(defun forge-extras-browse-github-inbox ()
  "Browse the GitHub notification inbox."
  (interactive)
  (browse-url "https://github.com/notifications"))

(defun forge-extras-state-set-dwim (&optional issue)
  "Close ISSUE at point if open, or reopen it if closed.
If ISSUE is nil, use the issue at point or in the current buffer."
  (interactive)
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue))
	 (state (oref issue state)))
    (pcase state
      ('open (forge--set-topic-state repo issue 'completed))
      ('completed (forge--set-topic-state repo issue 'open)))))

(defun forge-extras-pull-notifications ()
  "Fetch notifications for all repositories from the current forge.
Do not update if `elfeed' is in the process of being updated, since this causes
problems."
  (unless (bound-and-true-p elfeed-extras-auto-update-in-process)
    (shut-up
      (with-no-warnings
	(forge-pull-notifications)))))

;;;;; sync read status

(defconst forge-extras-safari-script-format-string
  "osascript -e 'tell application \"Safari\"
                 tell window 1
                   set beforeCount to count of tabs
                   make new tab with properties {URL:\"%s\"}
                   delay 5
                   try
                     set loadState to do JavaScript \"document.readyState\" in tab (beforeCount + 1)
                     if loadState is \"complete\" then
                       close tab (beforeCount + 1)
                       return \"Tab loaded and closed\"
                     end if
                   on error errMsg
                     return \"Error: \" & errMsg
                   end try
                 end tell
               end tell'"
  "The AppleScript to open a new tab in Safari and run JavaScript.")

(autoload 'doom-modeline--github-fetch-notifications "doom-modeline-segments")
(defun forge-extras-sync-read-status (&optional _)
  "Ensure that the read status of the issue at point in Forge matches GitHub's."
  (let* ((issue (forge-current-topic))
         (url (forge-get-url issue)))
    (when (eq (oref issue status) 'unread)
      (forge-extras-async-shell-command-to-string
       (format forge-extras-safari-script-format-string url)
       #'forge-extras-update-github-counter))))

(defun forge-extras-update-github-counter (output)
  "Update the GitHub notification counter after the Safari page is loaded.
OUTPUT is the output of the AppleScript script; it is used to check whether
JavaScript is enabled in Safari, which is needed for the command to run
successfully."
  (forge-extras-safari-ensure-javascript-enabled output)
  (when (bound-and-true-p doom-modeline-github)
    (doom-modeline--github-fetch-notifications)))

(defun forge-extras-async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.
Call CALLBACK with the resulting output when done."
  (let ((output-buffer (generate-new-buffer "*Async Shell Command*")))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (let ((output (with-current-buffer output-buffer
                         (buffer-string))))
           (funcall callback output)
           (kill-buffer output-buffer)))))))

(defun forge-extras-safari-github-logged-in-p ()
  "Check if user is logged in to GitHub in Safari."
  (let ((output (shell-command-to-string
		 "osascript -e 'tell application \"Safari\" to get name of document 1'")))
    (forge-extras-safari-ensure-javascript-enabled output)
    ;; we search for a word that only shows up if the user is logged in
    (numberp (string-match-p "issue" output))))

(defun forge-extras-safari-ensure-javascript-enabled (output)
  "Ensure that JavaScript is enabled in Safari.
OUTPUT is the output of the shell command that calls the AppleScript."
  (when (string-match-p "allow javascript from apple events" output)
    (error "For this function to work, JavaScript from Apple Events must be enabled in
Safari. This can be done by going to Safari > Preferences > Advanced, ticking
the box labelled \"Show features for web developers\", and then going to Safari
> Preferences > Developer and ticking the box labeled \"Allow JavaScript from
Apple Events\"")))

;;;;; Track repos

(declare-function magit-status "magit-status")
(autoload 'vc-extras-is-git-dir-p "vc-extras")
;;;###autoload
(defun forge-extras-track-repository (&optional dir)
  "Add DIR to the Forge database.
If DIR is nil, use the current directory."
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (if (vc-extras-is-git-dir-p default-directory)
	(let ((url (and-let*
		       ((repo (forge-get-repository :stub))
			(remote (oref repo remote)))
		     (magit-git-string "remote" "get-url" remote))))
	  (forge-extras-track-repo-all-topics url)
	  (magit-status-setup-buffer dir))
      (user-error "`%s' is not a Git repository" default-directory))))

(defun forge-extras-track-repo-all-topics (&optional url-or-path)
  "Add a repository to the forge database, pulling all topics.
If URL-OR-PATH is provided, add that repository. Otherwise, add the current
repo."
  (let* ((default-directory (if (and url-or-path (file-directory-p url-or-path))
				(expand-file-name url-or-path)
			      (or (locate-dominating-file default-directory ".git")
				  default-directory)))
	 (remote (forge--get-remote))
	 (repo-url (cond
		    ((and url-or-path (string-match-p "^\\(https?\\|git@\\)" url-or-path))
		     url-or-path)
		    (remote
		     (magit-git-string "remote" "get-url" remote))
		    (t
		     (user-error "No suitable repository found"))))
	 (repo (forge-get-repository repo-url nil :insert!)))
    (forge--pull repo nil nil)))

;;;;; Navigate messages

(defun forge--goto-message (direction)
  "Move to a post in the current topic in the specified DIRECTION.
DIRECTION should be either `next' or `prev'."
  (let ((section (magit-current-section)))
    (cond
     ;; If we're on a post, try to find the next/previous post
     ((magit-section-match 'post section)
      (if-let ((target (car (magit-section-siblings section direction))))
          (progn
            (goto-char (oref target start))
            (magit-section-update-highlight))
        (message "No %s post" direction)))
     ;; If we're not on a post, find the first/last post
     (t
      (let* ((posts (seq-filter
                     (lambda (s) (eq (oref s type) 'post))
                     (oref (or (magit-section-at (point))
                               (magit-current-section))
                           children)))
             ;; For 'next, take the first post; for 'prev, take the last
             (target (if (eq direction 'next)
                         (car posts)
                       (car (last posts)))))
        (if target
            (progn
              (goto-char (oref target start))
              (magit-section-update-highlight))
          (message "No posts found")))))))

(defun forge-next-message ()
  "Move to the next message in the current topic."
  (interactive)
  (forge--goto-message 'next))

(defun forge-previous-message ()
  "Move to the previous message in the current topic."
  (interactive)
  (forge--goto-message 'prev))

;;;;; Copy message at point

(defun forge-extras-copy-message-at-point-as-kill ()
  "Copy the body of the message at point to the kill ring.
The formatting of the message is preserved."
  (interactive)
  (when-let ((message (forge-post-at-point t)))
    (kill-new (oref message body))
    (message "Message copied to kill ring.")))

;;;; Insert Markdown link to GitHub Issue

;;;###autoload
(defun forge-extras-test-fetch-issues (&optional repo)
  "Fetch issues for REPO and echo the result for debugging."
  (interactive
   (list (read-string "Repository (owner/repo): ")))
  (let ((issues (forge-extras--fetch-issues-for-repo repo)))
    (if issues
        (message "Fetched %d issues for %s"
                 (length issues) repo)
      (message "No issues fetched for %s" repo))))

(defun forge-extras--parse-gh-issue-json (json-string repo-string)
  "Parse JSON-STRING from `gh issue list` for REPO-STRING.
Return a list of issue plists."
  (condition-case err
      ;; read the json with string keys and list arrays
      (let* ((json-object-type 'alist)
             (json-array-type  'list)
             (json-key-type    'string)
             (raw-issues       (json-read-from-string json-string))) ; ⇒ list of alists
	;; transform each gh issue alist into our internal plist,
	;; dropping any record whose number is missing
	(cl-remove-if
	 #'null
	 (mapcar
          (lambda (issue)
            (let ((num   (alist-get "number" issue))
                  (title (alist-get "title"  issue))
                  (url   (alist-get "url"    issue)))
              (when num
		`(:repo ,repo-string  :number ,num  :title ,title  :url ,url))))
          raw-issues)))
    (error
     (message "Error parsing JSON for %s: %s" repo-string err)
     nil)))

(defun forge-extras--parse-gh-issue-table (table-string repo-string)
  "Parse the default (tabular) output of `gh issue list`.
Return a list of plists with :repo :number :title :url."
  (let ((lines   (split-string table-string "\n" t))
        (issues  nil))
    (dolist (ln lines)
      (when (string-match "^#?\\([0-9]+\\)\\s-+\\(.+\\)$" ln)
        (let* ((num   (string-to-number (match-string 1 ln)))
               (rest  (string-trim (match-string 2 ln)))
               ;; strip a trailing state word (Open, Closed, etc.) if present
               (title (if (string-match "\\(.*?\\)\\s-+\\(Open\\|Closed\\|Draft\\|Done\\|Merged\\)$" rest)
                          (match-string 1 rest)
                        rest)))
          (push (list :repo   repo-string
                      :number num
                      :title  title
                      :url    (format "https://github.com/%s/issues/%s"
                                      repo-string num))
                issues))))
    (nreverse issues)))

(defun forge-extras--fetch-issues-for-repo (repo-string)
  "Fetch open issues for REPO-STRING using `gh` CLI.
Return a list of issue plists, or nil on error."
  (let ((gh-executable (executable-find "gh")))
    (unless gh-executable
      ;; This error is better handled by the calling function once.
      (cl-return-from forge-extras--fetch-issues-for-repo nil))

    (let ((output-buffer (generate-new-buffer (format "*gh-issues-%s*" (replace-regexp-in-string "/" "-" repo-string))))
          (error-file (make-temp-file "gh-issues-err-"))
          (process-args (list "issue" "list"
                              "--repo" repo-string
                              "--state" forge-extras-issue-link-state
                              "--limit" "300"))
          issues)
      (unwind-protect
          (let ((exit-status
                 (apply #'call-process gh-executable
                        nil (list output-buffer error-file) nil process-args)))
            (if (zerop exit-status)
                ;; plain-text table → plist list
                (setq issues (forge-extras--parse-gh-issue-table
                              (with-current-buffer output-buffer (buffer-string))
                              repo-string))
              ;; non-zero exit, capture stderr
              (let ((err (with-temp-buffer
                           (insert-file-contents error-file)
                           (buffer-string))))
                (message "Error fetching issues for %s (exit status %d): %s"
                         repo-string exit-status err))))
        (kill-buffer output-buffer)
        (when (file-exists-p error-file)
          (delete-file error-file)))
      issues)))

;;;###autoload
(defun forge-extras-insert-issue-markdown-link (&optional repositories)
  "Fetch issues from REPOSITORIES, prompt the user, then insert a markdown link.
REPOSITORIES should be a list of \"owner/repo\" strings."
  (interactive
   (list (split-string
          (read-string "Repositories (space or comma separated owner/repo): ")
          "[ ,]+" t)))
  (unless (executable-find "gh")
    (user-error "The 'gh' command-line tool is not installed or not in PATH.")
    (cl-return-from forge-extras-insert-issue-markdown-link))

  (unless repositories
    (user-error "No repositories supplied")
    (cl-return-from forge-extras-insert-issue-markdown-link))

  (let ((all-issues nil)
        (completion-alist nil))
    (dolist (repo repositories)
      (let ((repo-issues (forge-extras--fetch-issues-for-repo repo)))
        (when repo-issues
          (setq all-issues (append all-issues repo-issues)))))

    (unless all-issues
      (user-error "No issues found in the configured repositories, or an error occurred.")
      (cl-return-from forge-extras-insert-issue-markdown-link))

    ;; Sort issues by repository name (ascending), then by issue number (descending).
    (setq all-issues
          (sort all-issues
                (lambda (i1 i2)
                  (if (string= (plist-get i1 :repo) (plist-get i2 :repo))
                      (> (plist-get i1 :number) (plist-get i2 :number))
                    (string< (plist-get i1 :repo) (plist-get i2 :repo))))))

    (setq completion-alist
          (mapcar (lambda (issue)
                    (cons (format "%s #%s: %s"
                                  (plist-get issue :repo)
                                  (plist-get issue :number)
                                  (plist-get issue :title))
                          issue))
                  all-issues))

    (let* ((selected-key (completing-read
                          "Select issue: " completion-alist nil 'require-match))
           (selected-issue-plist (cdr (assoc selected-key completion-alist))))
      (when selected-issue-plist
        (let* ((repo   (plist-get selected-issue-plist :repo))
               (number (plist-get selected-issue-plist :number))
               (url    (plist-get selected-issue-plist :url))
               (markdown-link (format "[%s#%s](%s)" repo number url)))
          (insert markdown-link)
          (message "Inserted: %s" markdown-link))))))

;;;; GitHub Project Integration

(defconst forge-extras-gh-project-query
  "{
  repository(owner: \"%s\", name: \"%s\") {
    issue(number: %s) {
      id # Issue's global Node ID
      title
      assignees(first: 10) {
	nodes {
	  login
	}
      }
      labels(first: 10) {
	nodes {
	  name
	}
      }
      projectItems(first: 10) { # Assuming an issue is in few projects, or we filter later
	nodes {
          id # Project item's global Node ID
          project {
            number
            id # Project's global Node ID (useful for verification)
          }
	  fieldValues(first: 10) {
	    nodes {
              __typename # Helpful for debugging
	      ... on ProjectV2ItemFieldTextValue {
		textValue: text
		field {
		  ... on ProjectV2FieldCommon {
		    name
		  }
		}
	      }
	      ... on ProjectV2ItemFieldNumberValue {
		numberValue: number
		field {
		  ... on ProjectV2FieldCommon {
		    name
		  }
		}
	      }
	      ... on ProjectV2ItemFieldSingleSelectValue {
		singleSelectValue: name
                optionId # ID of the selected option
		field {
		  ... on ProjectV2FieldCommon {
		    name
                    id # Field's global Node ID
		  }
		}
	      }
	      ... on ProjectV2ItemFieldIterationValue {
		iterationValue: title
		field {
		  ... on ProjectV2FieldCommon {
		    name
		  }
		}
	      }
	      ... on ProjectV2ItemFieldDateValue {
		dateValue: date
		field {
		  ... on ProjectV2FieldCommon {
		    name
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
}'"
  "Raw GraphQL query string to get project fields for an issue.
This string is intended to be formatted with owner name, repository name,
and issue number.")

(defconst forge-extras-gh-add-item-to-project-mutation-query
  "mutation($projectNodeId:ID!, $issueNodeId:ID!) {
    addProjectV2ItemById(input:{projectId:$projectNodeId contentId:$issueNodeId}) {
      item {
        id
      }
    }
  }"
  "GraphQL mutation to add an issue (contentId) to a project (projectId).")

(defconst forge-extras-gh-update-project-item-field-mutation-query
  "mutation($projectNodeId:ID!, $itemNodeId:ID!, $fieldNodeId:ID!, $statusOptionId:String!) {
    updateProjectV2ItemFieldValue(input:{
      projectId:$projectNodeId,
      itemId:$itemNodeId,
      fieldId:$fieldNodeId,
      value:{ singleSelectOptionId:$statusOptionId }
    }) {
      projectV2Item {
        id
      }
    }
  }"
  "GraphQL mutation to update a single field (e.g., Status) of a project item.")

(defconst forge-extras-gh-update-project-item-estimate-field-mutation-query
  "mutation($projectNodeId:ID!, $itemNodeId:ID!, $fieldNodeId:ID!, $estimateValue:Float!) {
    updateProjectV2ItemFieldValue(input:{
      projectId:$projectNodeId,
      itemId:$itemNodeId,
      fieldId:$fieldNodeId,
      value:{ number: $estimateValue }
    }) {
      projectV2Item {
        id
      }
    }
  }"
  "GraphQL mutation to update a numeric field (e.g., Estimate) of a project item.")

(defun forge-extras--execute-gh-graphql-query (query-string variables)
  "Execute a GitHub GraphQL QUERY-STRING with VARIABLES.
VARIABLES is an alist of (var-name . value) suitable for a JSON 'variables' object.
Returns the parsed JSON response as an Elisp data structure, or nil on failure."
  (let* ((lines (split-string query-string "\n" t))
         (lines-without-comments (mapcar (lambda (line) (replace-regexp-in-string "#.*" "" line)) lines))
         (query-almost-single-line (string-join lines-without-comments " "))
         (single-line-query (string-trim (replace-regexp-in-string "\\s-+" " " query-almost-single-line)))
         ;; Construct JSON payload
         (payload-alist `(("query" . ,single-line-query)))
         (json-payload-string "")
         (output-buffer (generate-new-buffer "*gh-graphql-query-output*"))
         (json-string "")
         (parsed-json nil)
         (exit-status nil))

    ;; Add variables to payload if they exist
    (when variables
      (setq payload-alist (append payload-alist `(("variables" . ,variables)))))
    (setq json-payload-string (json-encode payload-alist))

    (let* ((process-args (list "api" "--method" "POST" "-H" "Content-Type: application/json" "/graphql" "--input" "-"))
           (gh-executable (executable-find "gh")))
      (unless gh-executable
        (error "The 'gh' command-line tool was not found. Please ensure it is installed and accessible"))
      (with-temp-buffer
        (insert json-payload-string)
        (setq exit-status
              (apply #'call-process-region
                     (point-min) (point-max)
                     gh-executable
                     nil ; no infile
                     output-buffer ; capture stdout
                     nil ; display stdout: no
                     process-args))))

    (with-current-buffer output-buffer
      (setq json-string (buffer-string)))
    (kill-buffer output-buffer)

    (if (not (zerop exit-status))
        (message "forge-extras--execute-gh-graphql-query: 'gh' process exited with status %s. Output:\n%s" exit-status json-string))

    (if (or (null json-string) (string-empty-p json-string) (not (zerop exit-status)))
        (progn
          (message "forge-extras--execute-gh-graphql-query: Received empty or error response from gh command.")
          nil)
      (with-temp-buffer
        (insert json-string)
        (goto-char (point-min))
        (setq parsed-json (condition-case err
                              (let ((json-array-type 'list)
                                    (json-object-type 'alist))
                                (json-read-from-string (buffer-string)))
                            (error
                             (message "forge-extras--execute-gh-graphql-query: Error parsing JSON: %s" err)
                             nil))))
      parsed-json)))

(defconst forge-extras-gh-project-fields-query
  "query($projectNodeId:ID!) {
    node(id: $projectNodeId) {
      ... on ProjectV2 {
        fields(first: 100) { # Assuming max 100 fields
          nodes {
            ... on ProjectV2FieldCommon {
              id
              name
            }
          }
        }
      }
    }
  }"
  "GraphQL query to fetch all fields (name and ID) for a given project Node ID.")

(defun forge-extras-gh--call-api-graphql-mutation (mutation-query-string variables)
  "Execute a GitHub GraphQL MUTATION-QUERY-STRING with VARIABLES.
VARIABLES is an alist of (var-name . value) for GraphQL variables.
Values should be Elisp strings for GraphQL Strings/IDs, and Elisp numbers
for GraphQL Ints/Floats. This function sends the request by piping a
JSON payload to `gh api ... --input -'."
  (let* ((lines (split-string mutation-query-string "\n" t))
         (lines-without-comments (mapcar (lambda (line) (replace-regexp-in-string "#.*" "" line)) lines))
         (query-almost-single-line (string-join lines-without-comments " "))
         (single-line-mutation (string-trim (replace-regexp-in-string "\\s-+" " " query-almost-single-line)))
         ;; Construct the JSON payload for gh api --input -
         (payload-alist `(("query" . ,single-line-mutation)))
         (json-payload-string "")
         (output-buffer (generate-new-buffer "*gh-graphql-mutation-output*"))
         (json-string "")
         (parsed-json nil)
         (exit-status nil))
    (when variables
      (setq payload-alist (append payload-alist `(("variables" . ,variables)))))
    ;; `json-encode' will correctly handle Elisp numbers (integers, floats)
    ;; as JSON numbers, and Elisp strings as JSON strings.
    (setq json-payload-string (json-encode payload-alist))
    (message "forge-extras-gh--call-api-graphql-mutation: JSON payload for gh api:\n%s" json-payload-string)
    ;; Arguments for gh api --method POST -H "Content-Type: application/json" /graphql --input -
    (let* ((process-args (list "api" "--method" "POST" "-H" "Content-Type: application/json" "/graphql" "--input" "-"))
           (gh-executable (executable-find "gh")))
      (unless gh-executable
        (error "The 'gh' command-line tool was not found. Please ensure it is installed and accessible"))
      (message "forge-extras-gh--call-api-graphql-mutation: Executing 'gh %s' with payload via stdin" (string-join process-args " "))
      ;; Pass json-payload-string via stdin using call-process-region
      (with-temp-buffer
        (insert json-payload-string)
        (setq exit-status
              (apply #'call-process-region
                     (point-min) (point-max)
                     gh-executable
                     nil
                     output-buffer
                     nil
                     process-args))))
    (with-current-buffer output-buffer
      (setq json-string (buffer-string)))
    (kill-buffer output-buffer)
    ;; No temporary query file was created with this method
    (if (not (zerop exit-status))
        (message "forge-extras-gh--call-api-graphql-mutation: 'gh' process exited with status %s. Output:\n%s"
		 exit-status json-string)
      (message "forge-extras-gh--call-api-graphql-mutation: 'gh' process exited successfully."))
    (message "forge-extras-gh--call-api-graphql-mutation: Raw JSON response:\n%s" json-string)
    (if (or (null json-string) (string-empty-p json-string) (not (zerop exit-status)))
        (progn
          (message "forge-extras-gh--call-api-graphql-mutation: Received empty or error response from gh command.")
          nil)
      (with-temp-buffer
        (insert json-string)
        (goto-char (point-min))
        (setq parsed-json (condition-case err
                              (json-read-from-string (buffer-string)) ; Using json-read-from-string
                            (error
                             (message "forge-extras-gh--call-api-graphql-mutation: Error parsing JSON: %s" err)
                             nil))))
      (if parsed-json
          (message "forge-extras-gh--call-api-graphql-mutation: Successfully parsed JSON.")
        (message "forge-extras-gh--call-api-graphql-mutation: Failed to parse JSON from response."))
      parsed-json)))

(defun forge-extras-gh-get-issue-fields (issue-number repo-name)
  "Return the relevant fields for ISSUE-NUMBER in REPO-NAME as a raw list."
  (let* ((formatted-query-string (format forge-extras-gh-project-query
                                         forge-extras-project-owner
                                         repo-name
                                         issue-number))
         (parsed-json (forge-extras--execute-gh-graphql-query formatted-query-string nil)))
    (message "forge-extras-gh-get-issue-fields: Executed query for issue %s in %s. Success: %s"
             issue-number repo-name (if parsed-json "yes" "no"))
    parsed-json))

(defun forge-extras-gh-parse-issue-fields (raw-list)
  "Parse RAW-LIST of issue fields into a property list.
This function specifically looks for data related to project number
`forge-extras-project-number'."
  (let* ((data (cdr (assoc 'data raw-list)))
         (repository (cdr (assoc 'repository data)))
         (issue (cdr (assoc 'issue repository)))
         (issue-node-id (cdr (assoc 'id issue)))
         (title (cdr (assoc 'title issue))))
    (let* ((assignees (mapcar (lambda (node) (cdr (assoc 'login node)))
                              (cdr (assoc 'nodes (cdr (assoc 'assignees issue))))))
           (labels (mapcar (lambda (node) (cdr (assoc 'name node)))
                           (cdr (assoc 'nodes (cdr (assoc 'labels issue))))))
           (project-items-nodes (cdr (assoc 'nodes (cdr (assoc 'projectItems issue)))))
           (project-item-id nil)
           (status-field-id nil)
           (selected-status-option-id nil)
           (status-name nil)
           (effort nil))
      ;; Locate the project item that belongs to `forge-extras-project-number'.
      (when-let* ((target-item
                   (seq-find
                    (lambda (it)
                      (let* ((proj      (cdr (assoc 'project it)))
                             (proj-num  (cdr (assoc 'number proj))))
                        (eql proj-num forge-extras-project-number)))
                    project-items-nodes)))
        (setq project-item-id (cdr (assoc 'id target-item)))
        (let ((field-values (cdr (assoc 'nodes (cdr (assoc 'fieldValues target-item))))))
          ;; Effort
          (when-let ((effort-node
                      (seq-find
                       (lambda (fv)
                         (string= "Estimate"
                                  (forge-extras--get-field-property-value
                                   (cdr (assoc 'field fv)) 'name)))
                       field-values)))
            (setq effort (cdr (assoc 'numberValue effort-node))))
          ;; Status
          (when-let ((status-node
                      (seq-find
                       (lambda (fv)
                         (string= "Status"
                                  (forge-extras--get-field-property-value
                                   (cdr (assoc 'field fv)) 'name)))
                       field-values)))
            (setq status-name               (cdr (assoc 'singleSelectValue status-node)))
            (setq selected-status-option-id (cdr (assoc 'optionId status-node)))
            (setq status-field-id
                  (forge-extras--get-field-property-value
                   (cdr (assoc 'field status-node)) 'id)))))
      (list :issue-node-id issue-node-id
            :title title
            :assignees assignees
            :labels labels
            :effort effort
            :status status-name
            :project-item-id project-item-id
            :status-field-id status-field-id
            :selected-status-option-id selected-status-option-id))))

(defun forge-extras-gh-get-project-fields (project-node-id)
  "Fetch all fields for PROJECT-NODE-ID using GitHub GraphQL API.
Returns the raw parsed JSON response, or nil on failure."
  (let* ((query-string forge-extras-gh-project-fields-query)
         (variables `(("projectNodeId" . ,project-node-id)))
         (parsed-json (forge-extras--execute-gh-graphql-query query-string variables)))
    (message "forge-extras-gh-get-project-fields: Executed query for project %s. Success: %s"
             project-node-id (if parsed-json "yes" "no"))
    parsed-json))

(defun forge-extras-gh-parse-project-fields (raw-json-response)
  "Parse RAW-JSON-RESPONSE from project fields query into a list of cons cells."
  (if-let* ((data (cdr (assoc 'data raw-json-response)))
            (node (cdr (assoc 'node data)))
            (fields (cdr (assoc 'fields node)))
            (field-nodes (cdr (assoc 'nodes fields))))
      (mapcar (lambda (field-node)
                (cons (cdr (assoc 'name field-node))
                      (cdr (assoc 'id field-node))))
              field-nodes)
    nil))

(defun forge-extras--get-field-property-value (field-obj property-key)
  "Return the value of PROPERTY-KEY within FIELD-OBJ.
FIELD-OBJ might be a single cons pair like (PROPERTY-KEY . value) or an
alist like ((PROPERTY-KEY . value) ...)."
  (cond ((null field-obj) nil)
        ((and (consp field-obj) (not (consp (car field-obj)))) ; Single pair (KEY . VAL)
         (if (eq (car field-obj) property-key)
             (cdr field-obj)
           nil))
        (t ; Alist ((KEY . VAL) ...)
         (cdr (assoc property-key field-obj)))))

(defun forge-extras--ensure-issue-in-project (issue-number issue-node-id current-project-item-id)
  "Add issue to project when needed and return its project item ID.
ISSUE-NUMBER is the human-readable issue number.  ISSUE-NODE-ID is the
GraphQL node ID for that issue.  CURRENT-PROJECT-ITEM-ID is an existing
project item ID or nil.  If the issue is not in the project, it is
automatically added and the new item ID is returned."
  (or current-project-item-id
      (let ((new-id (forge-extras-gh-add-issue-to-project
                     forge-extras-project-node-id
                     issue-node-id)))
        (if new-id
            (message "Added issue #%s to project." issue-number)
          (message "Failed to add issue #%s to project." issue-number))
        new-id)))

(defun forge-extras-gh-add-issue-to-project (project-node-id issue-node-id)
  "Add ISSUE-NODE-ID to PROJECT-NODE-ID.
Returns the new project item's Node ID, or nil on failure."
  (let* ((variables `(("projectNodeId" . ,project-node-id)
                      ("issueNodeId" . ,issue-node-id)))
         (response (forge-extras-gh--call-api-graphql-mutation forge-extras-gh-add-item-to-project-mutation-query variables)))
    (if-let* ((data (cdr (assoc 'data response)))
              (add-item (cdr (assoc 'addProjectV2ItemById data)))
              (item (cdr (assoc 'item add-item)))
              (item-id (cdr (assoc 'id item))))
        item-id
      (message "Failed to add issue to project. Response: %s" response)
      nil)))

(defun forge-extras-gh-update-project-item-status-field (project-node-id item-node-id field-node-id status-option-id)
  "Update the project item's status field.
PROJECT-NODE-ID is the project's Node ID. ITEM-NODE-ID is the project item's
Node ID. FIELD-NODE-ID is the \"Status\" field's Node ID. STATUS-OPTION-ID is
the Node ID of the desired status option (e.g., for \"Doing\")."
  (let* ((variables `(("projectNodeId" . ,project-node-id)
                      ("itemNodeId" . ,item-node-id)
                      ("fieldNodeId" . ,field-node-id)
                      ("statusOptionId" . ,status-option-id)))
         (response (forge-extras-gh--call-api-graphql-mutation forge-extras-gh-update-project-item-field-mutation-query variables)))
    (if-let* ((data (cdr (assoc 'data response)))
              (update-value (cdr (assoc 'updateProjectV2ItemFieldValue data)))
              (projectV2Item (cdr (assoc 'projectV2Item update-value)))
              (item-id (cdr (assoc 'id projectV2Item))))
        (progn
          (message "Successfully updated project item status.")
          t)
      (message "Failed to update project item status. Response: %s" response)
      nil)))

(defun forge-extras-gh-update-project-item-estimate-field (project-node-id item-node-id field-node-id estimate-value)
  "Update the project item's estimate field.
PROJECT-NODE-ID is the project's Node ID. ITEM-NODE-ID is the project item's
Node ID. FIELD-NODE-ID is the \"Estimate\" field's Node ID. ESTIMATE-VALUE is
the numerical estimate to set."
  (let* ((variables `(("projectNodeId" . ,project-node-id)
                      ("itemNodeId" . ,item-node-id)
                      ("fieldNodeId" . ,field-node-id)
                      ("estimateValue" . ,(float estimate-value))))
	 (response (forge-extras-gh--call-api-graphql-mutation forge-extras-gh-update-project-item-estimate-field-mutation-query variables)))
    (if-let* ((data (cdr (assoc 'data response)))
              (update-value (cdr (assoc 'updateProjectV2ItemFieldValue data)))
              (projectV2Item (cdr (assoc 'projectV2Item update-value)))
              (item-id (cdr (assoc 'id projectV2Item))))
	(progn
          (message "Successfully updated project item estimate.")
          t)
      (message "Failed to update project item estimate. Response: %s" response)
      nil)))

;;;###autoload
(defun forge-extras-set-project-status (&optional issue status)
  "Set the GitHub Project status for the current ISSUE to STATUS.
ISSUE defaults to `forge-current-topic'.
If STATUS is provided, use it directly. Otherwise, prompt for a status from
`forge-extras-status-option-ids-alist'.
If the issue is not yet in `forge-extras-project-number', automatically add it.
Updates are performed via GitHub API calls."
  (interactive (list (forge-current-topic) nil))
  (unless issue
    (user-error "No current issue/topic found"))
  (let* ((repo (forge-get-repository issue))
         (issue-number (oref issue number))
         (repo-name (oref repo name))
         ;; Fetch current project data once so we can offer its status as default
         (gh-fields (forge-extras-gh-get-issue-fields issue-number repo-name))
         (parsed-fields (when gh-fields
                          (forge-extras-gh-parse-issue-fields gh-fields)))
         (current-status-name (plist-get parsed-fields :status))
         (status-choices (mapcar #'car forge-extras-status-option-ids-alist))
         (default-status (if (member current-status-name status-choices)
                             current-status-name
                           (caar forge-extras-status-option-ids-alist)))
         (chosen-status-name (or status
                                 (completing-read "Set project status: "
                                                  status-choices nil 'require-match default-status)))
         (chosen-status-option-id (cdr (assoc chosen-status-name forge-extras-status-option-ids-alist))))
    (unless chosen-status-option-id
      (user-error "Invalid status selected or selection cancelled")
      (cl-return-from forge-extras-set-project-status))
    (message "Fetching current project fields for issue #%s in %s/%s..."
	     issue-number forge-extras-project-owner repo-name)
    (unless parsed-fields
      (user-error "Could not retrieve project data for issue. Aborting"))
    (let* ((issue-node-id (plist-get parsed-fields :issue-node-id))
           (current-project-item-id (plist-get parsed-fields :project-item-id))
           (target-project-item-id current-project-item-id))
      (unless issue-node-id
        (user-error "Could not retrieve GitHub Issue Node ID. Aborting")
        (cl-return-from forge-extras-set-project-status))
      (when (string= chosen-status-name current-status-name)
        (message "Issue #%s is already in status '%s'. No change needed." issue-number chosen-status-name)
        (cl-return-from forge-extras-set-project-status))
      (setq target-project-item-id
            (forge-extras--ensure-issue-in-project
             issue-number issue-node-id current-project-item-id))
      (unless target-project-item-id
        (cl-return-from forge-extras-set-project-status))
      (message "Updating project status for item %s to '%s' (Option ID: %s)..."
               target-project-item-id chosen-status-name chosen-status-option-id)
      (if (forge-extras-gh-update-project-item-status-field
           forge-extras-project-node-id
           target-project-item-id
           forge-extras-status-field-node-id
           chosen-status-option-id)
          (progn
            (message "Project status updated successfully for issue #%s." issue-number)
            ;; Refresh the topic to reflect potential changes
            (when (derived-mode-p 'forge-topic-mode)
              (forge-topic-refresh-buffer)))
        (user-error "Failed to update project status for issue #%s" issue-number)))))

;;;###autoload
(defun forge-extras-set-project-estimate (&optional issue estimate)
  "Set the GitHub Project estimate for the current ISSUE to ESTIMATE.
ISSUE defaults to `forge-current-topic'.
If ESTIMATE is provided, use it directly. Otherwise, prompt for a numerical
estimate. The current estimate (if any, from a field named \"Estimate\") is
offered as default when prompting.
If the issue is not yet in `forge-extras-project-number', automatically add it.
Updates are performed via GitHub API calls using the field ID from
`forge-extras-estimate-field-node-id'."
  (interactive (list (forge-current-topic) nil))
  (unless issue
    (user-error "No current issue/topic found"))
  (unless (and (boundp 'forge-extras-estimate-field-node-id)
               (stringp forge-extras-estimate-field-node-id)
               (not (string-empty-p forge-extras-estimate-field-node-id)))
    (user-error "`forge-extras-estimate-field-node-id' is not configured. Please set it"))
  (let* ((repo (forge-get-repository issue))
         (issue-number (oref issue number))
         (repo-name (oref repo name))
         (gh-fields (forge-extras-gh-get-issue-fields issue-number repo-name))
         (parsed-fields (when gh-fields
                          (forge-extras-gh-parse-issue-fields gh-fields)))
         (current-estimate (plist-get parsed-fields :effort)) ; :effort is from "Estimate" field
         (chosen-estimate (or estimate
                              (read-number "Set project estimate: " current-estimate))))
    (unless (numberp chosen-estimate)
      (user-error "Invalid estimate entered or selection cancelled")
      (cl-return-from forge-extras-set-project-estimate))
    (message "Fetching current project fields for issue #%s in %s/%s..."
	     issue-number forge-extras-project-owner repo-name)
    (unless parsed-fields
      (user-error "Could not retrieve project data for issue. Aborting"))
    (let* ((issue-node-id (plist-get parsed-fields :issue-node-id))
           (current-project-item-id (plist-get parsed-fields :project-item-id))
           (target-project-item-id current-project-item-id))
      (unless issue-node-id
        (user-error "Could not retrieve GitHub Issue Node ID. Aborting")
        (cl-return-from forge-extras-set-project-estimate))
      (if (and current-estimate (= chosen-estimate current-estimate))
          (message "Issue #%s already has estimate '%s'. No change needed." issue-number current-estimate)
        (progn ; Proceed with update
          (setq target-project-item-id
                (forge-extras--ensure-issue-in-project
                 issue-number issue-node-id current-project-item-id))
          (unless target-project-item-id
            (cl-return-from forge-extras-set-project-estimate))
          (message "Updating project estimate for item %s to '%s'..."
                   target-project-item-id chosen-estimate)
          (if (forge-extras-gh-update-project-item-estimate-field
               forge-extras-project-node-id
               target-project-item-id
               forge-extras-estimate-field-node-id ; Use the configured estimate field ID
               chosen-estimate)
              (progn
                (message "Project estimate updated successfully for issue #%s." issue-number)
                ;; Refresh the topic to reflect potential changes
                (when (derived-mode-p 'forge-topic-mode)
                  (forge-topic-refresh-buffer)))
            (user-error "Failed to update project estimate for issue #%s" issue-number)))))))

;;;###autoload
(defun forge-extras-get-project-field-ids ()
  "Fetch and display all field names and their Node IDs for the Project.
The project is determined by `forge-extras-project-node-id'. The results are
shown in a new buffer, \"*GitHub Project Fields*\". This command helps in
finding the Node ID required for variables like
`forge-extras-estimate-field-node-id' and `forge-extras-status-field-node-id'."
  (interactive)
  (unless (and (boundp 'forge-extras-project-node-id)
               (stringp forge-extras-project-node-id)
               (not (string-empty-p forge-extras-project-node-id)))
    (user-error "`forge-extras-project-node-id' is not configured. Please set it first"))
  (message "Fetching project fields for Project Node ID: %s..." forge-extras-project-node-id)
  (let* ((raw-response (forge-extras-gh-get-project-fields forge-extras-project-node-id))
         (fields (if raw-response
                     (forge-extras-gh-parse-project-fields raw-response)
                   nil)))
    (if fields
        (let ((buffer (get-buffer-create "*GitHub Project Fields*"))
              (max-name-len 0))
          ;; Determine max field name length for formatting
          (dolist (field fields)
            (setq max-name-len (max max-name-len (length (car field)))))
          (setq max-name-len (max max-name-len (length "Field Name"))) ; Ensure header fits

          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "Project Fields for Project Node ID: %s\n\n" forge-extras-project-node-id))
            (insert (format (format "%%-%ds | Field ID\n" max-name-len) "Field Name"))
            (insert (format "%s-|-%s\n"
			    (make-string max-name-len ?-)
			    (make-string 30 ?-))) ; Adjust 30 if ID length varies significantly
            (dolist (field fields)
              (insert (format (format "%%-%ds | %%s\n" max-name-len) (car field) (cdr field)))))
          (display-buffer buffer)
          (message "Project fields displayed in *GitHub Project Fields* buffer."))
      (user-error "Could not retrieve or parse project fields for Project Node ID: %s" forge-extras-project-node-id))))

(defconst forge-extras-gh-project-items-by-repo-query
  "query($projectNodeId:ID!, $cursor:String) {
    node(id: $projectNodeId) {
      ... on ProjectV2 {
        items(first: 100, orderBy: {field: POSITION, direction: ASC}, after: $cursor) {
          nodes {
            content {
              __typename
              ... on Issue {
                id
                number
                title
                url
                state # Issue state (e.g., OPEN, CLOSED)
                repository {
                  nameWithOwner
                }
              }
              ... on PullRequest {
                id
                number
                title
                url
                state # PR state (e.g., OPEN, CLOSED, MERGED)
                repository {
                  nameWithOwner
                }
              }
            }
          }
          pageInfo {
            endCursor
            hasNextPage
          }
        }
      }
    }
  }"
  "GraphQL query to fetch items (Issues/PRs) for a project, ordered by position, with pagination support.")

(defun forge-extras--parse-project-items (raw-json-response &optional target-repo-name-with-owner include-closed-p)
  "Parse RAW-JSON-RESPONSE from project items query.
If TARGET-REPO-NAME-WITH-OWNER is non-nil, filter items for that repository.
If INCLUDE-CLOSED-P is nil, filter out closed/merged items.
Filters for type (Issue or PullRequest).
Returns a cons cell: (LIST-OF-ITEMS . PAGE-INFO-ALIST).
Each item in LIST-OF-ITEMS is a plist with :type :number :title :url :repo :state."
  (let ((items-and-prs nil)
        (page-info nil))
    (when-let* ((data (cdr (assoc 'data raw-json-response)))
                (node (cdr (assoc 'node data)))
                (items-connection (cdr (assoc 'items node))))
      (setq page-info (cdr (assoc 'pageInfo items-connection)))
      (when-let* ((item-nodes (cdr (assoc 'nodes items-connection))))
        (dolist (item-node item-nodes)
          (when-let* ((content (cdr (assoc 'content item-node)))
                      (type-name (cdr (assoc '__typename content)))
                      (item-state (cdr (assoc 'state content)))
                      (repo-info (cdr (assoc 'repository content)))
                      (repo-name (cdr (assoc 'nameWithOwner repo-info))))
            (when (and (member type-name '("Issue" "PullRequest"))
                       (or (null target-repo-name-with-owner)
                           (and repo-name (string= repo-name target-repo-name-with-owner)))
                       (or include-closed-p
                           (not (cond ((string= type-name "Issue")
                                       (string= item-state "CLOSED"))
                                      ((string= type-name "PullRequest")
                                       (member item-state '("CLOSED" "MERGED")))))))
              (let ((number (cdr (assoc 'number content)))
                    (title (cdr (assoc 'title content)))
                    (url (cdr (assoc 'url content))))
                (when (and number title url)
                  (push `(:type ,(if (string= type-name "Issue") 'issue 'pullreq)
                          :repo ,repo-name
                          :number ,number
                          :title ,title
                          :url ,url
                          :state ,item-state)
                        items-and-prs))))))))
    (cons (nreverse items-and-prs) page-info)))

;;;###autoload
(defun forge-extras-list-project-issues-by-repo-ordered (repo-name-with-owner)
  "List issues and pull requests from REPO-NAME-WITH-OWNER in the configured GitHub project.
The items are listed in the order they appear on the project board.
REPO-NAME-WITH-OWNER should be in \"owner/repo\" format.
Results are displayed in a new buffer \"*Project Issues for REPO-NAME-WITH-OWNER*\".
Returns the list of issue/PR plists."
  (interactive
   (list (read-string "Repository (owner/repo): ")))
  (unless (and (boundp 'forge-extras-project-node-id)
               (stringp forge-extras-project-node-id)
               (not (string-empty-p forge-extras-project-node-id)))
    (user-error "`forge-extras-project-node-id' is not configured. Please set it first"))
  (unless (executable-find "gh")
    (user-error "The 'gh' command-line tool is not installed or not in PATH."))
  (unless (string-match-p ".+/.+" repo-name-with-owner)
    (user-error "Invalid repository format. Expected \"owner/repo\"."))

  (message "Fetching project items for %s from project %s..."
           repo-name-with-owner forge-extras-project-node-id)

  (let* ((variables `(("projectNodeId" . ,forge-extras-project-node-id)))
         (raw-response (forge-extras--execute-gh-graphql-query
                        forge-extras-gh-project-items-by-repo-query
                        variables)) ; variables will not include cursor for this func
         (parsed-result (if raw-response
                            ;; Pass t to include closed items, maintaining previous behavior for this command.
                            (forge-extras--parse-project-items raw-response repo-name-with-owner t)
                          nil))
         (items (car parsed-result))) ; Get items from (items . pageInfo)
    (if items
        (let* ((buffer-name (format "*Project Issues for %s*" repo-name-with-owner))
               (buffer (get-buffer-create buffer-name))
               (max-title-len 0)
               (max-repo-len (length repo-name-with-owner))
               (max-num-len 0))

          ;; Determine max lengths for formatting
          (dolist (item items)
            (setq max-title-len (max max-title-len (length (plist-get item :title))))
            (setq max-num-len (max max-num-len (length (number-to-string (plist-get item :number))))))
          (setq max-title-len (min max-title-len 80)) ; Cap title length for display
          (setq max-repo-len (max max-repo-len (length "Repo")))
          (setq max-num-len (max max-num-len (length "Number")))


          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "Project items for %s (Project Node ID: %s)\n"
                            repo-name-with-owner forge-extras-project-node-id))
            (insert (format "Order reflects the project board.\n\n"))
            (insert (format (format "%%-%ds | %%-%ds | %%s\n" max-repo-len max-num-len)
                            "Repo" "Number" "Title"))
            (insert (format "%s-|-%s-|-%s\n"
                            (make-string max-repo-len ?-)
                            (make-string max-num-len ?-)
                            (make-string max-title-len ?-)))
            (dolist (item items)
              (insert (format (format "%%-%ds | %%-%ds | %%s\n" max-repo-len max-num-len)
                              (plist-get item :repo)
                              (plist-get item :number)
                              (truncate-string-to-width (plist-get item :title) max-title-len nil nil "…")))))
          (display-buffer buffer)
          (message "Project items for %s displayed in %s buffer." repo-name-with-owner buffer-name))
      (message "No items found for %s in project %s, or an error occurred."
               repo-name-with-owner forge-extras-project-node-id))
    items))

(defvar forge-extras--cached-project-items nil
  "Cache for items returned by `forge-extras-list-project-items-ordered`.
This variable stores the list of project items (issues and pull requests)
the last time `forge-extras-list-project-items-ordered` was successfully executed.
It can be inspected or cleared manually if needed.")

;;;###autoload
(defun forge-extras-list-project-items-ordered (&optional include-closed-p display-buffer-p project-items-list)
  "List all issues and pull requests from the configured GitHub project.
Items are fetched page by page and listed in the order they appear on the project board.

If `project-items-list` is non-nil, it is used directly instead of fetching from GitHub.
Otherwise, items are fetched from the GitHub API.

If `include-closed-p' is non-nil (e.g., when called with any prefix argument),
closed and merged items are included. Otherwise, they are excluded.

If `display-buffer-p' is non-nil (e.g., when called without a prefix argument),
results are displayed in a new buffer \"*All Project Items (Ordered by Board)*\".
Otherwise, the buffer is not displayed.

Default behavior (no prefix argument): Exclude closed items, display buffer.
With a prefix argument (e.g., C-u): Include closed items, do NOT display buffer.

The returned list is always cached in `forge-extras--cached-project-items`.
Returns the list of issue/PR plists."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) ; include-closed-p: non-nil if any prefix
         (not current-prefix-arg)                ; display-buffer-p: t if no prefix, nil if any prefix
         nil))                                   ; project-items-list: nil for interactive calls

  (let ((items-to-process nil))
    (if project-items-list
        (progn
          (message "Using provided list of %d project items." (length project-items-list))
          (setq items-to-process project-items-list))
      (progn ; Fetch from GitHub
        (unless (and (boundp 'forge-extras-project-node-id)
                     (stringp forge-extras-project-node-id)
                     (not (string-empty-p forge-extras-project-node-id)))
          (user-error "`forge-extras-project-node-id' is not configured. Please set it first"))
        (unless (executable-find "gh")
          (user-error "The 'gh' command-line tool is not installed or not in PATH."))

        (message "Fetching all project items from project %s (Include closed: %s)..."
                 forge-extras-project-node-id (if include-closed-p "yes" "no"))

        (let ((fetched-items-accumulator nil)
              (current-cursor nil)
              (has-next-page t))
          (while has-next-page
            (message "Fetching project items page (cursor: %s)..." (or current-cursor "start"))
            (let* ((variables `(("projectNodeId" . ,forge-extras-project-node-id)
                                ,@(when current-cursor `(("cursor" . ,current-cursor)))))
                   (raw-response (forge-extras--execute-gh-graphql-query
                                  forge-extras-gh-project-items-by-repo-query
                                  variables))
                   (parsed-result (if raw-response
                                      (forge-extras--parse-project-items raw-response nil include-closed-p)
                                    nil))
                   (current-page-items (car parsed-result))
                   (page-info (cdr parsed-result)))

              (when (and raw-response parsed-result current-page-items)
                (setq fetched-items-accumulator (nconc fetched-items-accumulator current-page-items)))

              (let ((new-cursor (and page-info (cdr (assoc 'endCursor page-info)))))
                (if (and page-info (cdr (assoc 'hasNextPage page-info)) new-cursor)
                    (setq current-cursor new-cursor)
                  (setq has-next-page nil)
                  (if (not page-info)
                      (message "Warning: pageInfo not found in GraphQL response. Assuming no more pages.")
                    (when (and (cdr (assoc 'hasNextPage page-info)) (not new-cursor))
                      (message "Warning: hasNextPage was true but endCursor is missing/null. Stopping pagination.")))))))
          (setq items-to-process fetched-items-accumulator)))))

    ;; Cache the result
    (setq forge-extras--cached-project-items items-to-process)

    ;; Display and return
    (if items-to-process
        (progn
          (if display-buffer-p
              (let* ((buffer-name "*All Project Items (Ordered by Board)*")
                     (buffer (get-buffer-create buffer-name))
                     (max-repo-len 0)
                     (max-title-len 0)
                     (max-num-len 0))

                ;; Determine max lengths for formatting
                (dolist (item items-to-process)
                  (setq max-repo-len (max max-repo-len (length (plist-get item :repo))))
                  (setq max-title-len (max max-title-len (length (plist-get item :title))))
                  (setq max-num-len (max max-num-len (length (number-to-string (plist-get item :number))))))
                (setq max-repo-len (max max-repo-len (length "Repo"))) ; Ensure header fits
                (setq max-title-len (min max-title-len 80)) ; Cap title length for display
                (setq max-num-len (max max-num-len (length "Number")))

                (with-current-buffer buffer
                  (erase-buffer)
                  (insert (format "All project items (Project Node ID: %s)\n" forge-extras-project-node-id))
                  (insert (format "Order reflects the project board.\n\n"))
                  (insert (format (format "%%-%ds | %%-%ds | %%s\n" max-repo-len max-num-len)
                                  "Repo" "Number" "Title"))
                  (insert (format "%s-|-%s-|-%s\n"
                                  (make-string max-repo-len ?-)
                                  (make-string max-num-len ?-)
                                  (make-string max-title-len ?-)))
                  (dolist (item items-to-process)
                    (insert (format (format "%%-%ds | %%-%ds | %%s\n" max-repo-len max-num-len)
                                    (plist-get item :repo)
                                    (plist-get item :number)
                                    (truncate-string-to-width (plist-get item :title) max-title-len nil nil "…")))))
                (display-buffer buffer)
                (message "All project items displayed in %s buffer. Total: %d" buffer-name (length items-to-process)))
            (message "Processed %d project items. Buffer not displayed." (length items-to-process)))
          items-to-process) ; Return the items
      (progn
        (if project-items-list
            (message "No items provided or an empty list was given.")
          (message "No items found in project %s, or an error occurred." forge-extras-project-node-id))
        nil)))) ; Return nil if no items

(provide 'forge-extras)
;;; forge-extras.el ends here
