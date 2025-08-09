;;; elpaca-extras.el --- Extensions for elpaca -*- lexical-binding: t -*-

;; Copyright (C) 2025

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

Note that this unloads all of the package's symbols before
reloading.  Any data stored in those symbols will be lost, so if
the package would normally save that data, e.g. when a mode is
deactivated or when Emacs exits, the user should do so before
using this command."
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
    (dolist (feature package-features)
      (ignore-errors
        ;; Ignore error in case it's not loaded.
        (unload-feature feature 'force)))
    (dolist (feature package-features)
      (require feature))
    (when package-features
      (message "Reloaded: %s" (mapconcat #'symbol-name package-features " ")))))

;;;###autoload
(defun elpaca-extras-update-and-reload (&optional pkg)
  "Update PKG and reload its features.
If PKG is nil, prompt for it."
  (interactive (list (elpaca--read-queued "Update and reload package: ")))
  (let (on-update-finish)
    (setq on-update-finish
          (lambda ()
            (elpaca-extras--update-finish-callback pkg on-update-finish)))
    ;; Run the callback after Elpaca finalises its queues.
    (add-hook 'elpaca--post-queues-hook on-update-finish)
    ;; Trigger the update immediately.
    (elpaca-update pkg t)))

(defun elpaca-extras--update-finish-callback (pkg callback)
  "Helper function to handle package update completion.
PKG is the package that was updated, CALLBACK is the function to remove from
hooks."
  (let ((pkg-data (elpaca-get pkg)))
    (when (and pkg-data (eq (elpaca--status pkg-data) 'finished))
      ;; Remove the callback before reloading.
      (remove-hook 'elpaca--post-queues-hook callback)
      (elpaca-extras-reload pkg))))

(provide 'elpaca-extras)
;;; elpaca-extras.el ends here

