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

;;;; Variables

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
DIRECTION should be either `next or `prev."
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

;;;; GitHub Project Integration

(defgroup forge-extras-projects nil
  "Settings for Forge Extras GitHub Project integration."
  :group 'forge-extras
  :group 'external)

(defcustom forge-extras-project-owner "tlon-team"
  "The GitHub owner (organization or user) for the target project."
  :type 'string
  :group 'forge-extras-projects)

(defcustom forge-extras-project-number 9
  "The GitHub Project number to use for fetching/setting issue status."
  :type 'integer
  :group 'forge-extras-projects)

(defcustom forge-extras-project-node-id "PVT_kwDOBtGWf84A5jZf"
  "The global Relay Node ID of the target GitHub Project.
Default is for tlon-team/Project #9."
  :type 'string
  :group 'forge-extras-projects)

(defcustom forge-extras-status-field-node-id "PVTSSF_lADOBtGWf84A5jZfzguVNY8"
  "The global Relay Node ID of the \"Status\" field within the target GitHub Project.
Default is for tlon-team/Project #9's Status field."
  :type 'string
  :group 'forge-extras-projects)

(defcustom forge-extras-status-option-ids-alist
  '(("Doing" . "47fc9ee4")
    ("Next" . "8607328f")
    ("Later" . "13e22f63")
    ("Someday" . "4bf0f00e")
    ("Done" . "98236657"))
  "Alist mapping GitHub Project status names (car) to their global Option IDs (cdr)
for the \"Status\" field in the target project.
Default is for tlon-team/Project #9."
  :type '(alist :key-type string :value-type string)
  :group 'forge-extras-projects)

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

(defun forge-extras-gh--call-api-graphql-mutation (mutation-query-string variables)
  "Execute a GitHub GraphQL MUTATION-QUERY-STRING with VARIABLES.
VARIABLES is a list of cons cells like \\='((key . value) (key2 . value2)) for
-F options."
  (let* ((lines (split-string mutation-query-string "\n" t)) ; t = omit empty lines
         (lines-without-comments (mapcar (lambda (line) (replace-regexp-in-string "#.*" "" line)) lines))
         (query-almost-single-line (string-join lines-without-comments " "))
         (single-line-mutation (string-trim (replace-regexp-in-string "\\s-+" " " query-almost-single-line)))
         (temp-file (make-temp-file "forge-extras-gh-mutation-" nil ".graphql" single-line-mutation))
         (process-args (list "api" "graphql" "-F" (concat "query=@" temp-file)))
         (output-buffer (generate-new-buffer "*gh-graphql-mutation-output*"))
         (json-string "")
         (parsed-json nil)
         (exit-status nil))
    (when variables
      (setq process-args
            (append process-args
                    (mapcan (lambda (kv)
                              (list "-f" (format "%s=%s" (car kv) (cdr kv))))
                            variables))))
    (message "forge-extras-gh--call-api-graphql-mutation: Mutation query content (single line):\n%s" single-line-mutation)
    (when variables
      (let ((json-object-type 'object))
        (message "forge-extras-gh--call-api-graphql-mutation: Intended variables: %s" (json-encode variables))))
    (message "forge-extras-gh--call-api-graphql-mutation: Executing 'gh %s'" (string-join process-args " "))
    (let ((gh-executable (executable-find "gh")))
      (unless gh-executable
        (error "The 'gh' command-line tool was not found. Please ensure it is installed and accessible"))
      (setq exit-status (apply #'call-process gh-executable nil output-buffer nil process-args)))
    (with-current-buffer output-buffer
      (setq json-string (buffer-string)))
    (kill-buffer output-buffer)
    (when (file-exists-p temp-file)
      (delete-file temp-file))
    (if (not (zerop exit-status))
        (message "forge-extras-gh--call-api-graphql-mutation: 'gh' process exited with status %s. Output:\n%s" exit-status json-string)
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
  (let* ((raw-query (format forge-extras-gh-project-query
                            forge-extras-project-owner
                            repo-name
                            issue-number))
         (lines (split-string raw-query "\n" t))
         (lines-without-comments (mapcar (lambda (line) (replace-regexp-in-string "#.*" "" line)) lines))
         (query-almost-single-line (string-join lines-without-comments " "))
         (single-line-query (string-trim (replace-regexp-in-string "\\s-+" " " query-almost-single-line)))
         (temp-file (make-temp-file "forge-extras-gh-query-" nil ".graphql" single-line-query))
         (process-args (list "api" "graphql" "-F" (concat "query=@" temp-file)))
         (output-buffer (generate-new-buffer "*gh-graphql-output*"))
         (json-string "")
         (parsed-json nil)
         (exit-status nil))
    (message "forge-extras-gh-get-issue-fields: Query content (single line):\n%s" single-line-query)
    (message "forge-extras-gh-get-issue-fields: Executing 'gh %s'" (string-join process-args " "))
    (let ((gh-executable (executable-find "gh")))
      (unless gh-executable
        (error "The 'gh' command-line tool was not found. Please ensure it is installed and accessible"))
      (setq exit-status (apply #'call-process gh-executable nil output-buffer nil process-args)))
    (with-current-buffer output-buffer
      (setq json-string (buffer-string)))
    (kill-buffer output-buffer)
    (when (file-exists-p temp-file)
      (delete-file temp-file))
    (if (not (zerop exit-status))
        (message "forge-extras-gh-get-issue-fields: 'gh' process exited with status %s. Output:\n%s" exit-status json-string)
      (message "forge-extras-gh-get-issue-fields: 'gh' process exited successfully."))
    (message "forge-extras-gh-get-issue-fields: Raw JSON string response:\n%s" json-string)
    (if (or (null json-string) (string-empty-p json-string) (not (zerop exit-status)))
        (progn
          (message "forge-extras-gh-get-issue-fields: Received empty or error response from gh command.")
          nil)
      (with-temp-buffer
        (insert json-string)
        (goto-char (point-min))
        (setq parsed-json (condition-case err
                              (json-read-from-string (buffer-string)) ; Using json-read-from-string
                            (error
                             (message "forge-extras-gh-get-issue-fields: Error parsing JSON: %s" err)
                             nil))))
      (if parsed-json
          (message "forge-extras-gh-get-issue-fields: Successfully parsed JSON.")
        (message "forge-extras-gh-get-issue-fields: Failed to parse JSON from response."))
      parsed-json)))

(defun forge-extras-gh-parse-issue-fields (raw-list)
  "Parse RAW-LIST of issue fields into a property list.
This function specifically looks for data related to project number
`forge-extras-project-number'."
  (message "Debug: forge-extras-gh-parse-issue-fields received raw-list: %S" raw-list)
  (let* ((data (cdr (assoc "data" raw-list)))
         (repository (cdr (assoc "repository" data)))
         (issue (cdr (assoc "issue" repository)))
         (issue-node-id (cdr (assoc "id" issue)))
         (title (cdr (assoc "title" issue))))
    (message "Debug: forge-extras-gh-parse-issue-fields extracted issue-node-id: %S" issue-node-id)
    (message "Debug: forge-extras-gh-parse-issue-fields extracted title: %S" title)
    (let* ((assignees (mapcar (lambda (node) (cdr (assoc "login" node)))
                              (cdr (assoc "nodes" (cdr (assoc "assignees" issue))))))
           (labels (mapcar (lambda (node) (cdr (assoc "name" node)))
                           (cdr (assoc "nodes" (cdr (assoc "labels" issue))))))
           (project-items-nodes (cdr (assoc "nodes" (cdr (assoc "projectItems" issue)))))
           (project-item-id nil)
           (status-field-id nil)
           (selected-status-option-id nil)
           (status-name nil)
           (effort nil))
      (cl-block project-item-loop
        (dolist (item project-items-nodes)
          (let* ((project-node (cdr (assoc "project" item)))
                 (project-number (cdr (assoc "number" project-node))))
            (when (eql project-number forge-extras-project-number)
              (setq project-item-id (cdr (assoc "id" item)))
              (let ((field-values (cdr (assoc "nodes" (cdr (assoc "fieldValues" item))))))
                (when-let ((effort-node (seq-find (lambda (fv-item)
                                                    (string= "Estimate" (cdr (assoc "name" (cdr (assoc "field" fv-item))))))
                                                  field-values)))
                  (setq effort (cdr (assoc "numberValue" effort-node))))
                (when-let ((status-node (seq-find (lambda (fv-item)
                                                    (string= "Status" (cdr (assoc "name" (cdr (assoc "field" fv-item))))))
                                                  field-values)))
                  (setq status-name (cdr (assoc "singleSelectValue" status-node)))
                  (setq selected-status-option-id (cdr (assoc "optionId" status-node)))
                  (setq status-field-id (cdr (assoc "id" (cdr (assoc "field" status-node))))))))
              (cl-return-from project-item-loop)))))
      (list :issue-node-id issue-node-id
            :title title
            :assignees assignees
            :labels labels
            :effort effort
            :status status-name
            :project-item-id project-item-id
            :status-field-id status-field-id
            :selected-status-option-id selected-status-option-id))))

(defun forge-extras-gh-add-issue-to-project (project-node-id issue-node-id)
  "Add ISSUE-NODE-ID to PROJECT-NODE-ID.
Returns the new project item's Node ID, or nil on failure."
  (let* ((variables `(("projectNodeId" . ,project-node-id)
                      ("issueNodeId" . ,issue-node-id)))
         (response (forge-extras-gh--call-api-graphql-mutation forge-extras-gh-add-item-to-project-mutation-query variables)))
    (if-let* ((data (cdr (assoc "data" response)))
              (add-item (cdr (assoc "addProjectV2ItemById" data)))
              (item (cdr (assoc "item" add-item)))
              (item-id (cdr (assoc "id" item))))
        item-id
      (progn
        (message "Failed to add issue to project. Response: %s" response)
        nil))))

(defun forge-extras-gh-update-project-item-status-field (project-node-id item-node-id field-node-id new-status-option-id)
  "Update the project item's status field.
PROJECT-NODE-ID is the project's Node ID. ITEM-NODE-ID is the project item's
Node ID. FIELD-NODE-ID is the \"Status\" field's Node ID. NEW-STATUS-OPTION-ID
is the Node ID of the desired status option (e.g., for \"Doing\")."
  (let* ((variables `(("projectNodeId" . ,project-node-id)
                      ("itemNodeId" . ,item-node-id)
                      ("fieldNodeId" . ,field-node-id)
                      ("statusOptionId" . ,new-status-option-id)))
         (response (forge-extras-gh--call-api-graphql-mutation forge-extras-gh-update-project-item-field-mutation-query variables)))
    (if-let* ((data (cdr (assoc "data" response)))
              (update-value (cdr (assoc "updateProjectV2ItemFieldValue" data)))
              (projectV2Item (cdr (assoc "projectV2Item" update-value)))
              (item-id (cdr (assoc "id" projectV2Item))))
        (progn
          (message "Successfully updated project item status.")
          t) ; Return t on success
      (progn
        (message "Failed to update project item status. Response: %s" response)
        nil))))

;;;###autoload
(defun forge-extras-set-project-status (&optional issue)
  "Set the GitHub Project status for the current ISSUE.
ISSUE defaults to `forge-current-topic'.
Prompts for a status from `forge-extras-status-option-ids-alist'.
If the issue is not yet in `forge-extras-project-number', prompts to add it.
Updates are performed via GitHub API calls."
  (interactive (list (forge-current-topic)))
  (unless issue
    (user-error "No current issue/topic found"))
  (let* ((repo (forge-get-repository issue))
         (issue-number (oref issue number))
         (repo-name (oref repo name))
         (status-choices (mapcar #'car forge-extras-status-option-ids-alist))
         (chosen-status-name (completing-read "Set project status: " status-choices nil t nil nil (caar forge-extras-status-option-ids-alist)))
         (chosen-status-option-id (cdr (assoc chosen-status-name forge-extras-status-option-ids-alist))))

    (unless chosen-status-option-id
      (user-error "Invalid status selected or selection cancelled")
      (cl-return-from forge-extras-set-project-status))

    (message "Fetching current project fields for issue #%s in %s/%s..." issue-number forge-extras-project-owner repo-name)
    (let* ((gh-fields (forge-extras-gh-get-issue-fields issue-number repo-name))
           (parsed-fields (if gh-fields (forge-extras-gh-parse-issue-fields gh-fields) nil))
           (issue-node-id (plist-get parsed-fields :issue-node-id))
           (current-project-item-id (plist-get parsed-fields :project-item-id))
           (current-status-name (plist-get parsed-fields :status))
           (target-project-item-id current-project-item-id))

      (unless issue-node-id
        (user-error "Could not retrieve GitHub Issue Node ID. Aborting.")
        (cl-return-from forge-extras-set-project-status))

      (when (string= chosen-status-name current-status-name)
        (message "Issue #%s is already in status '%s'. No change needed." issue-number chosen-status-name)
        (cl-return-from forge-extras-set-project-status))

      (unless target-project-item-id
        (if (y-or-n-p (format "Issue #%s is not in Project %s (%s). Add it and set status to '%s'?"
                              issue-number forge-extras-project-number forge-extras-project-owner chosen-status-name))
            (progn
              (message "Adding issue #%s to project %s..." issue-number forge-extras-project-node-id)
              (setq target-project-item-id (forge-extras-gh-add-issue-to-project forge-extras-project-node-id issue-node-id))
              (unless target-project-item-id
                (user-error "Failed to add issue to project. Aborting status update.")
                (cl-return-from forge-extras-set-project-status))
              (message "Issue added to project (New Item ID: %s)." target-project-item-id))
          (progn
            (message "User cancelled adding issue to project. Aborting status update.")
            (cl-return-from forge-extras-set-project-status))))

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
              (forge-topic-refresh)))
        (user-error "Failed to update project status for issue #%s." issue-number)))))

(provide 'forge-extras)
;;; forge-extras.el ends here
