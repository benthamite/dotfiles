;;; gh-notify-extras.el --- Extensions for gh-notify -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/gh-notify-extras.el
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

;; Extensions for `gh-notify'.

;;; Code:

(require 'doom-modeline-segments)
(require 'el-patch)
(require 'gh-notify)
(require 'paths)
(require 'w3m)

;;;; User options

(defgroup gh-notify-extras ()
  "Extensions for `gh-notify'."
  :group 'gh-notify)

(defcustom gh-notify-extras-repos paths-dir-tlon-repos
  "Directory where the repositories are stored."
  :type 'directory
  :group 'gh-notify-extras)

;;;; Functions

;; TODO: check that this is programmed correctly
(defun gh-notify-extras-visit-next-notification ()
  "Visit the next notification and mark it as read."
  (interactive)
  (let ((old-buffer (current-buffer))
        (old-window (selected-window)))
    (forward-line)
    (call-interactively 'gh-notify-visit-notification)
    (select-window old-window)
    (switch-to-buffer old-buffer)))

;; the code below is a workaround to make `gh-notify' mark issues as read when
;; they are visited. It assumes that you have authenticated with GitHub using
;; the `w3m' browser. If not, please run `gh-notify-extras-w3m-login' and enter
;; your credentials.

(defun gh-notify-extras-w3m-login ()
  "Log in to GitHub using `w3m'."
  (interactive)
  (w3m "https://github.com/login"))

(defun gh-notify-extras-get-issue-url ()
  "Get the URL of the issue at point."
  (unless (or (derived-mode-p 'forge-issue-mode)
	      (derived-mode-p 'forge-pullreq-mode))
    (user-error "Not in `forge-issue-mode' or `forge-pullreq-mode'"))
  (forge-get-url forge-buffer-topic))

(defun gh-notify-extras-mark-issue-as-read ()
  "Mark issue at point as read on GitHub."
  (interactive)
  (require 'w3m)
  (if (or (derived-mode-p 'forge-issue-mode)
	  (derived-mode-p 'forge-pullreq-mode))
      (save-window-excursion
	(let ((url (gh-notify-extras-get-issue-url))
	      (w3m-new-session-in-background t))
	  (w3m-goto-url-new-session url nil nil nil nil t)))
    (run-with-timer 1 nil 'gh-notify-extras-mark-issue-as-read)))

(defun gh-notify-extras-delete-residual-w3m-buffers (&rest _)
  "Delete all `w3m' buffers minus one."
  (require 'w3m)
  (let ((w3m-buffers (w3m-list-buffers)))
    (while (> (length w3m-buffers) 1)
      (setq w3m-buffers (cdr w3m-buffers))
      (when (buffer-live-p (car w3m-buffers))
        (with-current-buffer (car w3m-buffers)
          (w3m-delete-buffer))))))

(defun gh-notify-extras-w3m-after-load-funs (original-func format-string &rest args)
  "Functions to trigger when `w3m' is done loading.
ORIGINAL-FUNC, FORMAT-STRING and ARGS are passed to the advised function."
  (require 'w3m)
  (let ((message-text (apply 'format format-string args))
	(inhibit-message t))
    (when (or (string-match "The content (\\(.*\\)) has been retrieved in \\(.*\\)" message-text)
	      (string-match "fontifying...done" message-text))
      (forge-pull-notifications)
      (when (featurep 'doom-modeline)
	(doom-modeline--github-fetch-notifications)))
    (apply original-func format-string args)))

;; TODO: restrict the scope of this so that it doesn’t conflict with normal uses
;; of `w3m'
(defun gh-notify-extras-rename-w3m-buffers ()
  "Rename w3m buffers to be hidden."
  (when (string-match "^\\*w3m*" (buffer-name))
    (rename-buffer (concat " " (buffer-name)) t)))

(add-hook 'buffer-list-update-hook 'gh-notify-extras-rename-w3m-buffers)
(advice-add 'w3m-message :around #'gh-notify-extras-w3m-after-load-funs)
(advice-add 'w3m--goto-url--handler-function :after #'gh-notify-extras-delete-residual-w3m-buffers)

(defun gh-notify-extras-visit-notification (P)
  "Visit the notification at point and mark it as read if unread.
Browse issue or PR on prefix P."
  (interactive "P")
  (let ((default-directory gh-notify-extras-repos)
	(unread (eq (get-text-property (point) 'face)
		    'gh-notify-notification-unread-face)))
    (gh-notify-visit-notification P)
    (when unread
      (gh-notify-extras-mark-issue-as-read))))

(defun gh-notify-extras-full-refresh ()
  "Pull Forge notifications and then refresh the `gh-notify' buffer."
  (interactive)
  (forge-pull-notifications)
  (gh-notify-forge-refresh))

(defun gh-notify-extras-refresh-in-background ()
  "Refresh `gh-notify' without switching to its buffer."
  (interactive)
  (let ((buf (get-buffer-create "*github-notifications*")))
    (set-buffer buf)
    (unless (derived-mode-p 'gh-notify-mode)
      (gh-notify-mode)))
  (gh-notify-forge-refresh))

;;;;; patched functions

;; do not run `forge-pull-topic' when visiting an issue or PR
(el-patch-defun gh-notify-visit-notification (P)
  "Attempt to visit notification at point in some sane way.
Browse issue or PR on prefix P."
  (interactive "P")
  (cl-assert (eq major-mode 'gh-notify-mode) t)
  (when-let ((current-notification (gh-notify-current-notification)))
    (let* ((repo-id (gh-notify-notification-repo-id current-notification))
	   (repo (gh-notify-notification-repo current-notification))
	   (topic (gh-notify-notification-topic current-notification))
	   (type (gh-notify-notification-type current-notification))
	   (title (gh-notify-notification-title current-notification)))
      (if P
	  ;; browse url for issue or pull request on prefix
	  (gh-notify-browse-notification repo-id type topic)
	;; handle through magit forge otherwise

	;; important: we want to re-render on read/unread state before switching
	;; buffers, that's because we do an auto-magic point reposition based on
	;; the last notification state, but on a buffer switch, the active point
	;; is lost in the middle of this logic, this doesn't "break" anything, but
	;; it can result in a lagging point, so take care of all the state rendering
	;; first, and THEN trigger the buffer switch

	(let ((default-directory gh-notify-smokescreen-path))

	  ;; XXX: this is a really ugly hack until I figure out how to cleanly make
	  ;; XXX: magit ignore errors when we don't have a local copy of the repo
	  ;; XXX: checked out in our magit paths ... we really don't need a local copy
	  ;; XXX: for interacting with issues and even performing reviews ... e.g.
	  ;; XXX: github-review will work fine with a template magit buffer from a PR
	  ;; XXX: for a non-local repo ...

	  ;; XXX: so we throw up a smokescreen with an empty tmp git repo, magit will
	  ;; XXX: fall back to default-directory if it can't find the actual repo ;)
	  ;; XXX: surely there's some non-ganky way to achieve this, but will have to
	  ;; XXX: dig into magit/forge guts a bit more ...

	  (unless (file-exists-p default-directory)
	    (make-directory default-directory)
	    (set-file-modes default-directory #o700)
	    (magit-init default-directory))

	  (pcase type
	    ('issue
	     ;;(message "handling an issue ...")
	     (gh-notify-mark-notification-read current-notification)
	     (with-demoted-errors "Warning: %S"
	       (with-temp-buffer
		 (forge-visit-issue (forge-get-issue repo topic))
		 (el-patch-remove (forge-pull-topic topic)))))
	    ('pullreq
	     ;;(message "handling a pull request ...")
	     (gh-notify-mark-notification-read current-notification)
	     (with-demoted-errors "Warning: %S"
	       (with-temp-buffer
		 (forge-visit-pullreq (forge-get-pullreq repo topic))
		 (forge-pull-topic topic))))
	    ('commit
	     (message "Commit not handled yet!"))
	    (_
	     (message "Handling something else (%s) %s\n" type title))))))))

(provide 'gh-notify-extras)
;;; gh-notify-extras.el ends here
