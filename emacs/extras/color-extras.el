;;; color-extras.el --- Extensions for color -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/color-extras.el
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

;; Extensions for `color'.

;;; Code:

(require 'color)
(require 'hexrgb)

;;;; Functions

(defun color-extras-hex-to-hsl (hex)
  "Convert a HEX color string to its HSL representation."
  (cl-destructuring-bind (red green blue)
      (hexrgb-hex-to-rgb hex)
    (color-rgb-to-hsl red green blue)))

(defun color-extras-hsl-to-hex (hue saturation luminance)
  "Convert HUE, SATURATION and LUMINANCE to a hex color string."
  (cl-destructuring-bind (red green blue)
      (color-hsl-to-rgb hue saturation luminance)
    (hexrgb-rgb-to-hex red green blue 2)))

(defun color-extras-hsl-to-string (hsl &optional format)
  "Convert HSL to a string.
Optionally, specify a FORMAT string to control the output. If FORMAT is nil,
default to \"(%0.6f %0.6f %0.6f)\"."
  (cl-destructuring-bind (hue saturation luminance) hsl
    (let ((format (or format "(%0.6f %0.6f %0.6f)")))
      (format format hue saturation luminance))))

(defun color-extras-hsl-string-to-hex (string)
  "Convert a STRING representation of HSL to HEX."
  (cl-destructuring-bind (hue saturation luminance)
      (read string)
    (color-extras-hsl-to-hex hue saturation luminance)))

(defun color-extras-rgb-to-hex (red green blue)
  "Convert RGB values to a hexadecimal color string.
RED, GREEN and BLUE should each be integers between 0 and 255, inclusive."
  (format "#%02x%02x%02x" red green blue))

(provide 'color-extras)
;;; color-extras.el ends here

