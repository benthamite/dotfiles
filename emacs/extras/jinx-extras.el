;;; jinx-extras.el --- Extensions for jinx -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/jinx-extras.el
;; Version: 0.2
;; Package-Requires: ((jinx "0.1"))

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

;; Extensions for `jinx'.

;;; Code:

(require 'jinx)
(require 'paths)

;;;; User options

(defgroup jinx-extras ()
  "Extensions for `jinx'."
  :group 'jinx)

(defcustom jinx-extras-relink-program
  (file-name-concat paths-dir-dotfiles "bin/jinx-enchant-relink")
  "Program that makes the jinx module use the GLib bundled in Emacs.app.
Homebrew's enchant links Homebrew's GLib, so a module linked to it loads a
second GLib into Emacs.app, which bundles its own; the Objective-C runtime
then warns that `GNotificationCenterDelegate' is implemented twice.  The
program relinks the module to a copy of enchant that uses Emacs.app's GLib.
Set to nil to disable."
  :type '(choice file (const :tag "Disabled" nil))
  :group 'jinx-extras)

;;;; Functions

(defun jinx-extras--use-emacs-glib (orig)
  "Relink the jinx module to Emacs.app's GLib, then call ORIG.
ORIG is `jinx--load-module'.  When the module was just compiled by ORIG,
relink it afterwards, so the next Emacs session loads the relinked module."
  (let ((module (jinx-extras--module-file)))
    (when (and module (jinx-extras--links-homebrew-enchant-p module))
      (jinx-extras--relink module))
    (funcall orig)
    (unless module
      (when-let* ((built (jinx-extras--module-file)))
        (jinx-extras--relink built)))))

(advice-add 'jinx--load-module :around #'jinx-extras--use-emacs-glib)

(defun jinx-extras--module-file ()
  "Return the jinx module file, or nil if it is not built yet."
  (when module-file-suffix
    (locate-library (file-name-with-extension "jinx-mod" module-file-suffix) t)))

(defun jinx-extras--links-homebrew-enchant-p (module)
  "Return non-nil if MODULE links enchant outside `jinx-extras-relink-program'.
Only meaningful on macOS, where `otool' lists a module's libraries."
  (and jinx-extras-relink-program
       (eq system-type 'darwin)
       (executable-find "otool")
       (string-match-p "/opt/homebrew/.*/libenchant"
                       (shell-command-to-string
                        (format "otool -L %s" (shell-quote-argument module))))))

(defun jinx-extras--relink (module)
  "Run `jinx-extras-relink-program' on MODULE and report a failure."
  (with-temp-buffer
    (let ((status (call-process jinx-extras-relink-program nil t nil module)))
      (if (eq status 0)
          (let ((inhibit-message t))
            (message "jinx-extras: relinked %s to Emacs.app's GLib" module))
        (message "jinx-extras: relinking %s failed: %s" module
                 (string-trim (buffer-string)))))))

;; github.com/minad/jinx/wiki#save-misspelling-and-correction-as-abbreviation
(defun jinx-extras--add-to-abbrev (overlay word)
  "Add abbreviation to `text-mode-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
  (let ((abbrev (buffer-substring-no-properties
                 (overlay-start overlay)
                 (overlay-end overlay))))
    (message "Abbrev: %s -> %s" abbrev word)
    (define-abbrev text-mode-abbrev-table abbrev word)))

(advice-add 'jinx--correct-replace :before #'jinx-extras--add-to-abbrev)

(defun jinx-extras-toggle-languages ()
  "Toggle between English and Spanish."
  (interactive)
  (let ((lang (if (string= jinx-languages "en") "es" "en")))
    (jinx-languages lang)))

(provide 'jinx-extras)
;;; jinx-extras.el ends here
