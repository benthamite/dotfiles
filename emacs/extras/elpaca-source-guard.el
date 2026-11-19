;;; elpaca-source-guard.el --- Keep Elpaca from deleting checkouts it does not own -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/elpaca-source-guard.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

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

;; `elpaca-delete' removes a package's source directory recursively.  For
;; a recipe whose `:repo' is a local path, Elpaca builds the package in
;; place and that source directory is the checkout itself, such as
;; ~/repos/protean, from which Protean's engine also runs.  The global
;; minor mode `elpaca-source-guard-mode' makes Elpaca refuse to delete
;; any package whose source directory lies outside
;; `elpaca-sources-directory'.

;;; Code:

(defvar elpaca-sources-directory)
(declare-function elpaca<-id "elpaca")
(declare-function elpaca<-source-dir "elpaca")

;;;; User options

(defgroup elpaca-source-guard ()
  "Keep Elpaca from deleting checkouts it does not own."
  :group 'convenience)

;;;; Functions

;;;###autoload
(define-minor-mode elpaca-source-guard-mode
  "Refuse to delete a package whose source is outside Elpaca's sources.
When on, `elpaca-delete' signals an error, and deletes nothing, for a
package whose source directory lies outside `elpaca-sources-directory'."
  :global t
  :group 'elpaca-source-guard
  (if elpaca-source-guard-mode
      (advice-add 'elpaca--delete :before #'elpaca-source-guard-refuse)
    (advice-remove 'elpaca--delete #'elpaca-source-guard-refuse)))

(defun elpaca-source-guard-refuse (e)
  "Signal an error when deleting E would delete a checkout Elpaca does not own.
E is the Elpaca order about to be deleted.  Its source directory is
Elpaca's own only when it lies inside `elpaca-sources-directory'."
  (when-let* ((source (elpaca<-source-dir e)))
    (unless (elpaca-source-guard-owned-p source)
      (user-error "Refusing to delete %s: its source %s is outside `elpaca-sources-directory'"
                  (elpaca<-id e) source))))

(defun elpaca-source-guard-owned-p (source)
  "Return non-nil when SOURCE lies inside `elpaca-sources-directory'.
Both directories are compared by their true names, so a link cannot
make an outside checkout look like Elpaca's."
  (file-in-directory-p (file-truename source)
                       (file-truename elpaca-sources-directory)))

(provide 'elpaca-source-guard)
;;; elpaca-source-guard.el ends here
