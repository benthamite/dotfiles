;;; copilot-extras.el --- Extensions for copilot -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/copilot-extras.el
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

;; Extensions for `copilot'.

;;; Code:

(require 'copilot)

;;;; User options

(defgroup copilot-extras ()
  "Extensions for `copilot'."
  :group 'prog-mode)

(defcustom copilot-extras-excluded-modes '()
  "List of modes in which `copilot' should not be enabled."
  :type '(repeat symbol)
  :group 'copilot-extras)

;;;; Functions

(defun copilot-extras-enable-conditionally ()
  "Enable `copilot' except in read-only modes or excluded modes.
The list of excluded modes is defined in `copilot-extras-excluded-modes'."
  (unless (or buffer-read-only
	      (memq major-mode copilot-extras-excluded-modes))
    (copilot-mode)))

(provide 'copilot-extras)
;;; copilot-extras.el ends here
