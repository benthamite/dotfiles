;;; vulpea-extras.el --- Extensions for vulpea -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/vulpea-extras.el
;; Version: 0.2
;; Package-Requires: ((paths "0.1") (vulpea "0.2.0"))

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

;; Extensions for `vulpea'.

;;; Code:

(require 'paths)
(require 'vulpea)

;;;; User options

(defgroup vulpea-extras ()
  "Extensions for `vulpea'."
  :group 'vulpea)

(defcustom vulpea-extras-excluded-directories
  (list
   paths-dir-dropbox-tlon-fede
   paths-dir-dropbox-tlon-leo)
  "Directories to exclude from list of projects, recursively."
  :type '(repeat directory)
  :group 'vulpea-extras)

(defcustom vulpea-extras-excluded-files
  '()
  "Files to exclude from list of projects."
  :type '(repeat file)
  :group 'vulpea-extras)

;;;; Functions

(defun vulpea-extras-project-p ()
  "Return non-nil if current buffer has a TODO, a schedule or a deadline.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (when (and (derived-mode-p 'org-mode)
             ;; exclude dirs recursively
             (not (vulpea-extras-file-in-excluded-directory-p (buffer-file-name)))
             ;; exclude files
             (not (member (buffer-file-name) vulpea-extras-excluded-files)))
    (org-element-map
	(org-element-parse-buffer 'headline)
	'headline
      (lambda (headline)
	(let* ((org-use-tag-inheritance t)
	       (tags (org-get-tags (org-element-property :begin headline))))
	  (and
	   ;; exclude archived TODOs
	   (not (member "ARCHIVE" tags))
	   (or
	    ;; either it is a todo...
	    (eq (org-element-property :todo-type headline) 'todo)
	    ;; ...or it has a schedule or a deadline and is not done
	    (and
	     (not (eq (org-element-property :todo-type headline) 'done))
	     (or (org-element-property :scheduled headline)
		 (org-element-property :deadline headline)))))))
      nil
      'first-match)))

(defun vulpea-extras-file-in-excluded-directory-p (file)
  "Return non-nil if FILE is in one of `vulpea-extras-excluded-directories`."
  (seq-some (lambda (dir) (file-in-directory-p file dir))
            vulpea-extras-excluded-directories))

(defun vulpea-extras-anniversary-p ()
  "Return non-nil if current buffer has an anniversary."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (search-forward "%%(org-anniversary" nil t))))

(defun vulpea-extras-project-files ()
  "Return a list of note files containing `project' tag."
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-extras-project-update-tag ()
  "Update \"PROJECT\" tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (vulpea-extras-buffer-p)
             (not (and (fboundp 'track-changes-inconsistent-state-p)
		       (track-changes-inconsistent-state-p))))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (or (vulpea-extras-project-p) (vulpea-extras-anniversary-p))
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))
        (setq tags (seq-uniq tags))
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(defvar org-extras-agenda-files-excluded)
(autoload 'org-roam-extras-recent "org-roam-extras")
(defun vulpea-extras-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files
        (seq-difference
         (delete-dups (append
		       (org-agenda-files)
		       (vulpea-extras-project-files)
		       (org-roam-extras-recent 2 500)))
         org-extras-agenda-files-excluded)))

(advice-add 'org-agenda :before #'vulpea-extras-agenda-files-update)

(defun vulpea-extras-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(provide 'vulpea-extras)
;;; vulpea-extras.el ends here

