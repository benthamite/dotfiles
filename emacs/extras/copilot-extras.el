;;; copilot-extras.el --- Extensions for copilot -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/copilot-extras.el
;; Version: 0.2
;; Package-Requires: ((copilot "0.0.1"))

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
The list of excluded modes is defined in `copilot-extras-excluded-modes'.
Also skip enabling when called from a non-interactive context (e.g. a timer),
since starting the copilot server in such contexts can trigger errors that
freeze Emacs."
  (unless (or buffer-read-only
	      (memq major-mode copilot-extras-excluded-modes)
	      (null this-command)
	      (bound-and-true-p org-capture-mode))
    (copilot-mode)))

(defun copilot-extras-restart-copilot ()
  "Hack: `restart' copilot periodically."
  (when copilot-mode
    (copilot-mode -1)
    (sleep-for 0.001)
    (copilot-mode +1)))

(defcustom copilot-extras-suppressed-error-codes '(-32800 -32602)
  "JSON-RPC error codes to suppress in `copilot--log'.
-32800 means the server canceled a superseded request (normal during typing).
-32602 means the server does not recognize the document URI
\(transient mismatch)."
  :type '(repeat integer)
  :group 'copilot-extras)

(defun copilot-extras-suppress-canceled-log (orig-fn level format &rest args)
  "Suppress noisy errors in `copilot--log'.
ORIG-FN is the original `copilot--log' function, called with LEVEL,
FORMAT, and ARGS unless the message carries a JSON-RPC code listed
in `copilot-extras-suppressed-error-codes'."
  (unless (and (eq level 'error)
               (cl-some (lambda (arg)
                          (and (listp arg)
                               (plist-member arg :code)
                               (memq (plist-get arg :code)
                                     copilot-extras-suppressed-error-codes)))
                        args))
    (apply orig-fn level format args)))

(advice-add 'copilot--log :around #'copilot-extras-suppress-canceled-log)

(provide 'copilot-extras)
;;; copilot-extras.el ends here
