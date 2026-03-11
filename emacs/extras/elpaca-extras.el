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
         (package-dir (file-name-directory
                       (locate-file package-name load-path (get-load-suffixes))))
         (package-files (directory-files package-dir 'full (rx ".el" eos)))
         (package-features
          (cl-loop for file in package-files
                   when (with-temp-buffer
                          (insert-file-contents file)
                          (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                            (goto-char (match-beginning 0))
                            (cadadr (read (current-buffer)))))
                   collect it)))
    (unless allp
      (setf package-features (seq-intersection package-features features)))
    ;; Load the main feature first so sub-modules find its variables.
    (when (memq package package-features)
      (setf package-features
            (cons package (delq package package-features))))
    ;; Remove from `features' so `require' reloads each file.
    ;; We avoid `unload-feature' because it unbinds variables from
    ;; all files that declared them (via `defvar'), not just the file
    ;; that defined them (via `defcustom').  In multi-file packages
    ;; this makes shared variables void.
    (dolist (feature package-features)
      (setq features (delq feature features)))
    (dolist (feature package-features)
      (require feature))
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

(defun elpaca-extras--build-and-reload (pkg build-fn verb)
  "Build PKG using BUILD-FN, then reload.
VERB is a past-tense verb for the success message (e.g., \"Updated\")."
  (letrec ((callback
            (lambda ()
              (elpaca-extras--handle-build-complete pkg callback verb))))
    (add-hook 'elpaca-post-queue-hook callback)
    (funcall build-fn pkg t)))

(defun elpaca-extras--handle-build-complete (pkg callback verb)
  "Handle build completion for PKG, removing CALLBACK from hook.
VERB is a past-tense verb for the success message."
  (let* ((e (elpaca-get pkg))
         (status (and e (elpaca--status e))))
    (when (memq status '(finished failed))
      (remove-hook 'elpaca-post-queue-hook callback)
      (pcase status
        ('finished
         (elpaca-extras-reload pkg)
         (message "%s and reloaded: %s" verb pkg))
        ('failed
         (message "Build failed for %s: %s" pkg
                  (or (and e (nth 2 (car (elpaca<-log e)))) "unknown error")))))))

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

