;;; use-package-extras.el --- Extensions for use-package -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/use-package-extras.el
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

;; Extensions for `use-package'.

;;; Code:

(require 'use-package)

;;;; Functions

;; github.com/raxod502/radian/blob/develop/emacs/radian.el
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

(defmacro use-personal-package (name &rest args)
  "Like `use-package' but accounting for personal packages.
NAME and ARGS as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca (,name
              :host github
              :repo "benthamite/dotfiles"
              :files ,(list (file-name-concat
                             "emacs/extras"
                             (file-name-with-extension (symbol-name (eval `(quote ,name))) "el"))))
     ,@args))

;; From Gonçalo Santos (github.com/weirdNox/dotfiles/blob/master/config/.config/emacs/config.org#helpers)
(defmacro lambda! (&rest body)
  "Return a lambda function with BODY."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(provide 'use-package-extras)
;;; use-package-extras.el ends here

