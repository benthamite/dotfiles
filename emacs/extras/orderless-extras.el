;;; orderless-extras.el --- Extensions for orderless -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/orderless-extras.el
;; Version: 0.2
;; Package-Requires: ((orderless "1.1"))

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

;; Extensions for `orderless'.

;;; Code:

(require 'orderless)

;;;; Functions

(defun orderless-extras-flex-dispatcher (pattern _index _total)
  "Flex dispatcher using `~' as suffix.
PATTERN is the string to match."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun orderless-extras-initialism-dispatcher (pattern _index _total)
  "Initialism dispatcher using `\,' as suffix.
PATTERN is the string to match."
  (when (string-suffix-p "," pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))

(defun orderless-extras-prefixes-dispatcher (pattern _index _total)
  "Prefix dispatcher using `\;' as suffix.
PATTERN is the string to match."
  (when (string-suffix-p ";" pattern)
    `(orderless-prefixes . ,(substring pattern 0 -1))))

(defun orderless-extras-exclusion-dispatcher (pattern _index _total)
  "Exclusion dispatcher using `!' as suffix.
PATTERN is the string to match."
  (when (string-suffix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

(provide 'orderless-extras)
;;; orderless-extras.el ends here

