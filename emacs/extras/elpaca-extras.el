;;; elpaca-extras.el --- Extensions for elpaca -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/elpaca-extras.el
;; Version: 0.2
;; Package-Requires: ((elpaca "0.0.1"))

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

;; Extensions for `elpaca'.

;;; Code:

(require 'elpaca)

;;;; Variables

(defgroup elpaca-extras ()
  "Extensions for `elpaca'."
  :group 'elpaca)

(defcustom elpaca-extras-write-lock-file-excluded nil
  "List of package identifiers that must never be written to a lock file."
  :type '(repeat symbol))

(defvar elpaca-extras--build-reload-statuses (make-hash-table :test #'equal)
  "Status table for asynchronous build-and-reload requests.")

;;;; Functions

;; github.com/progfolio/elpaca/issues/250
(defun elpaca-extras-reload (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

New definitions overwrite old ones; existing variable values are
preserved unless the new code changes their defaults."
  (interactive
   (list (let ((elpaca-overriding-prompt "Reload package: "))
           (elpaca--read-queued))
         current-prefix-arg))
  (message "Reloading: %s" package)
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (let* ((package-name (symbol-name package))
         (located (locate-file package-name load-path (get-load-suffixes)))
         (package-dir (and located (file-name-directory located)))
         (package-files (and package-dir
                             (directory-files package-dir 'full (rx ".el" eos))))
         (package-features
          (cl-loop for file in package-files
                   when (with-temp-buffer
                          (insert-file-contents file)
                          (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                            (goto-char (match-beginning 0))
                            (cadadr (read (current-buffer)))))
                   collect it)))
    (unless allp
      (setf package-features (seq-intersection package-features features))
      ;; Always include the main feature: when the user explicitly
      ;; rebuilds a package, the main module must be loaded even if
      ;; it was only set up via autoloads and never fully loaded.
      (cl-pushnew package package-features))
    ;; Load the main feature first so sub-modules find its variables.
    (when (memq package package-features)
      (setf package-features
            (cons package (delq package package-features))))
    ;; Force-load each file via `load' rather than `require'.
    ;; `require' is a no-op when the feature is in `features', and
    ;; elpaca's rebuild can re-add features (via autoloads) before
    ;; we get here.  `load' always evaluates the file.
    (dolist (feature package-features)
      (load (locate-file (symbol-name feature) load-path
                         (get-load-suffixes))
            nil 'nomessage))
    (when package-features
      (message "Reloaded: %s" (mapconcat #'symbol-name package-features " ")))))

;;;###autoload
(defun elpaca-extras-update-and-reload (&optional pkg)
  "Update PKG and reload its features.
If PKG is nil, prompt for it."
  (interactive (list (elpaca--read-queued "Update and reload package: ")))
  (elpaca-extras--build-and-reload pkg #'elpaca-update "Updated"))

;;;###autoload
(defun elpaca-extras-rebuild-and-reload (&optional pkg)
  "Rebuild PKG and reload its features.
If PKG is nil, prompt for it."
  (interactive (list (elpaca--read-queued "Rebuild and reload package: ")))
  (elpaca-extras--build-and-reload pkg #'elpaca-rebuild "Rebuilt"))

;;;###autoload
(defun elpaca-extras-build-reload-status (token)
  "Return the build-and-reload status plist for TOKEN."
  (when-let* ((status (gethash token elpaca-extras--build-reload-statuses)))
    (copy-sequence status)))

;;;###autoload
(defun elpaca-extras-format-build-reload-status (token)
  "Return a compact status string for build-and-reload TOKEN.
The result is formatted as STATE:MESSAGE so shell hooks can poll
completion with short `emacsclient' calls."
  (let* ((status (elpaca-extras-build-reload-status token))
         (state (or (plist-get status :state) 'missing))
         (message (or (plist-get status :message) "")))
    (format "%s:%s" state message)))

(defun elpaca-extras--build-and-reload (pkg build-fn verb)
  "Build PKG asynchronously using BUILD-FN, then reload it.
VERB is a past-tense verb for the success message (e.g., \"Updated\").

Completion is driven entirely by `elpaca-post-queue-hook', which elpaca
runs from its build-process sentinels.  Nothing blocks the command loop:
the build is enqueued and this function returns immediately, and the
reload happens once the build process actually finishes.  This avoids
`elpaca-wait', whose `sit-for' loop pumps the event loop and can wedge a
daemon that is concurrently serving `emacsclient' requests."
  (letrec ((token (elpaca-extras--build-reload-token pkg))
           (callback
            (lambda ()
              (elpaca-extras--handle-build-complete pkg callback verb token))))
    (elpaca-extras--record-build-reload-status
     token :package pkg :state 'queued :message "Build queued")
    (add-hook 'elpaca-post-queue-hook callback)
    (funcall build-fn pkg t)
    token))

(defun elpaca-extras--build-reload-token (pkg)
  "Return a unique build-and-reload token for PKG."
  (format "%s-%s-%s" pkg (float-time) (random most-positive-fixnum)))

(defun elpaca-extras--record-build-reload-status (token &rest status)
  "Record STATUS under build-and-reload TOKEN."
  (when token
    (puthash token status elpaca-extras--build-reload-statuses)))

(defun elpaca-extras--handle-build-complete (pkg callback verb &optional token)
  "Handle build completion for PKG, removing CALLBACK from hook.
VERB is a past-tense verb for the success message.

TOKEN, when non-nil, identifies the status entry to update."
  (let* ((e (elpaca-get pkg))
         (status (and e (elpaca--status e))))
    (when (memq status '(finished failed))
      (remove-hook 'elpaca-post-queue-hook callback)
      (pcase status
        ('finished
         (elpaca-extras-reload pkg)
         (elpaca-extras--record-build-reload-status
          token :package pkg :state 'finished
          :message (format "%s and reloaded: %s" verb pkg))
         (message "%s and reloaded: %s" verb pkg)
         'finished)
        ('failed
         (let ((message (format "Build failed for %s: %s" pkg
                                (or (and e (nth 2 (car (elpaca<-log e))))
                                    "unknown error"))))
           (elpaca-extras--record-build-reload-status
            token :package pkg :state 'failed :message message)
           (message "%s" message)
           'failed))))))

;;;;; Lock file

;;;###autoload
(defun elpaca-extras-write-lock-file-excluding (path &optional elpacas)
  "Write a lock file to PATH, excluding selected packages.
The list of exclusions is defined in `elpaca-extras-write-lock-file-excluded'.
PATH is the destination file.

ELPACAS, when non-nil, should be a queue-like list as accepted by
`elpaca-write-lock-file'.  When it is nil the current queue is used."
  (interactive "FWrite lock-file to: ")
  (let* ((elpacas (or elpacas (elpaca--queued)))
         (filtered (cl-remove-if
                    (lambda (cell)
                      (memq (car cell) elpaca-extras-write-lock-file-excluded))
                    elpacas)))
    (elpaca-write-lock-file path filtered)))

(provide 'elpaca-extras)
;;; elpaca-extras.el ends here
