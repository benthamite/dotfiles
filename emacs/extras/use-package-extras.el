;;; use-package-extras.el --- Extensions for use-package -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/use-package-extras.el
;; Version: 0.2
;; Package-Requires: ((use-package))

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
     :ensure nil
     ,@args))

;; From Gonçalo Santos (github.com/weirdNox/dotfiles/blob/master/config/.config/emacs/config.org#helpers)
(defmacro lambda! (&rest body)
  "Return a lambda function with BODY."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

;; systemcrafters.net/emacs-from-scratch/cut-start-up-time-in-half/#letrsquos-find-out-how-long-itrsquos-taking
(defun use-package-extras-display-startup-time ()
  "Display the time it took to load Emacs and the number of garbage collections."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(provide 'use-package-extras)
;;; use-package-extras.el ends here

