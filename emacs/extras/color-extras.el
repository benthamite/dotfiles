;;; color-extras.el --- Extensions for color -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/color-extras.el
;; Version: 0.2

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

(require 'cl-lib)
(require 'color)

;;;; Variables

(defconst color-extras-hex-pattern
  "\\(?1:#?\\(?:[[:xdigit:]]\\{6\\}\\)\\)\\b"
  "Regular expression matching a hexadecimal RGB color string.
Do not include the \"#\" in the match data.")

(defconst color-extras-hsl-scaled-pattern
  "\\(?1:[[:digit:]]\\{1,3\\}\\), ?\\(?2:[[:digit:]]\\{1,3\\}\\)%?, ?\\(?3:[[:digit:]]\\{1,3\\}\\)%?"
  "Regular expression matching a scaled HSL color string.
The expression matches three numbers separated by commas, corresponding to
hue, saturation and luminance, respectively. Hue is a number between 0 and 360,
and saturation and luminance are each numbers between 0 and 100.")

;;;; Functions

(defun color-extras-parse-color (color &optional string)
  "Parse COLOR.
If color is HSL, return it as a list of three numbers, unless STRING is non-nil."
  (cond
   ((string-match color-extras-hex-pattern color)
    (match-string-no-properties 1 color))
   ((string-match color-extras-hsl-scaled-pattern color)
    (let ((list (color-extras-hsl-split)))
      (color-extras-format-hsl list string)))
   (t (error "Invalid color: %s" color))))

;;;;; Looking at

(autoload 'thing-at-point-looking-at "thingatpt")
(defun color-extras-looking-at-rgb ()
  "Return the RGB color at point, if any."
  (when (thing-at-point-looking-at color-extras-hex-pattern)
    (match-string-no-properties 1)))

(defun color-extras-looking-at-hsl-scaled (&optional string)
  "Return the HSL color at point, if any.
If STRING is non-nil, return the HSL color as a string. Otherwise, return the
HSL color as a list of three numbers."
  (when (thing-at-point-looking-at color-extras-hsl-scaled-pattern)
    (let ((hsl-list (color-extras-hsl-split)))
      (color-extras-format-hsl hsl-list string))))

(defun color-extras-looking-at-color ()
  "Return the color at point, if any."
  (or (color-extras-looking-at-rgb) (color-extras-looking-at-hsl-scaled)))

;;;;; Conversion

(defun color-extras-convert (&optional color)
  "Convert the HSL or HEX COLOR to the other format.
If COLOR is nil, use the color at point."
  (interactive)
  (let* ((color (or color (color-extras-looking-at-color)))
         converted-color)
    (cond
     ((stringp color)
      (progn
	(string-match color-extras-hex-pattern color)
	(setq converted-color (color-extras-hsl-to-string
			       (color-extras-hex-to-hsl (match-string-no-properties 1 color) t)))))
     ((listp color)
      (setq converted-color (apply #'color-extras-hsl-to-hex (append color '(t)))))
     (t (message "No recognizable color format at point.")))
    (kill-new converted-color)
    (message "Saved color `%s' to the kill ring." converted-color)))

(defun color-extras-convert-all (format &optional file)
  "Convert all colors in FILE in HEX or HSL FORMAT to the other format.
FORMAT is either \"hex\" or \"hsl\". If FILE is nil, use the buffer visited by
the current file."
  (interactive (list (completing-read
		      (format "Convert all color values in `%s' to this format: "
			      (file-name-nondirectory (buffer-file-name)))
		      '("hex" "hsl"))))
  (let ((file (or file (buffer-file-name)))
	(pattern (pcase format
		   ("hex" color-extras-hsl-scaled-pattern)
		   ("hsl" color-extras-hex-pattern))))
    (save-excursion
      (with-current-buffer (find-file-noselect file))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(replace-match
	 (pcase format
	   ("hex" (apply #'color-extras-hsl-to-hex (append (color-extras-hsl-split) '(t))))
	   ("hsl" (color-extras-hsl-to-string
		   (color-extras-hex-to-hsl (match-string-no-properties 1) t)))))))))

;;;;;; HSL internal conversion

(defun color-extras-hsl-split ()
  "Split HSL values from the current match into a list of numbers."
  (let ((hue (string-to-number (match-string-no-properties 1)))
        (saturation (string-to-number (match-string-no-properties 2)))
        (luminance (string-to-number (match-string-no-properties 3))))
    (list hue saturation luminance)))

(defun color-extras-hsl-to-string (hsl)
  "Convert HSL values to a string.
Specifically, convert from list of numbers to a string rounded to the nearest
integer."
  (format "%d, %d%%, %d%%"
	  (round (nth 0 hsl)) (round (nth 1 hsl)) (round (nth 2 hsl))))

(defun color-extras-format-hsl (hsl &optional string)
  "Format HSL values as a string if STRING is non-nil, else as a list."
  (if string (color-extras-hsl-to-string hsl) hsl))

;;;;;; HSL <> HEX

;;;;;;; Rescale

(defun color-extras-hsl-rescale (hue saturation luminance operation)
  "Rescale HUE, SATURATION and LUMINANCE.
If OPERATION is `*', the values are rescaled to be each between 0 and 1. If
OPERATION is `/', hue is rescaled to be between 0 and 360, and saturation and
luminance are each rescaled to be between 0 and 100."
  (let ((hue (funcall operation hue 360.0))
	(saturation (funcall operation saturation 100.0))
	(luminance (funcall operation luminance 100.0)))
    (list hue saturation luminance)))

(defun color-extras-hsl-maybe-rescale (hue saturation luminance rescale operation)
  "Rescale HUE, SATURATION and LUMINANCE if RESCALE is non-nil.
If OPERATION is `*', hue is rescaled to be between 0 and 360, and saturation and
luminance are each rescaled to be between 0 and 100. If OPERATION is `/', all
three values are rescaled to be each between 0 and 1."
  (if rescale
      (color-extras-hsl-rescale hue saturation luminance operation)
    (list hue saturation luminance)))

;;;;;;; Convert

(defun color-extras-hex-to-hsl (hex &optional rescale)
  "Convert a HEX color string to its HSL representation.
The HSL representation is a list of three numbers each between 0.0 and 1.0,
inclusive. If RESCALE is non-nil, hue is a number between 0 and 360, and
saturation and luminance are each numbers between 0 and 100."
  (cl-destructuring-bind (red green blue)
      (color-extras-hex-to-rgb hex)
    (cl-destructuring-bind (hue saturation luminance)
	(color-rgb-to-hsl red green blue)
      (color-extras-hsl-maybe-rescale hue saturation luminance rescale #'*))))

(defun color-extras-hsl-to-hex (hue saturation luminance &optional rescale)
  "Convert HUE, SATURATION and LUMINANCE to a HEX color string.
By default, HUE, SATURATION and LUMINANCE are each numbers between 0.0 and 1.0,
inclusive. If RESCALE is non-nil, HUE is a number between 0 and 360, and
SATURATION and LUMINANCE are each numbers between 0 and 100."
  (cl-destructuring-bind (red green blue)
      (apply #'color-hsl-to-rgb
	     (color-extras-hsl-maybe-rescale hue saturation luminance rescale #'/))
    (color-rgb-to-hex red green blue 2)))

;;;;;; RGB <> HEX

(defun color-extras-rgb-to-hex (red green blue)
  "Convert RGB values to a hexadecimal color string.
RED, GREEN and BLUE should each be integers between 0 and 255, inclusive."
  (format "#%02x%02x%02x" red green blue))

(defun color-extras-hex-to-rgb (hex)
  "Convert a hexadecimal color notation to RGB values.
HEX should be a string starting with \"#\" followed by either 3, 4, 6, or 8
hexadecimal digits.

Return a list of three floating-point values between 0.0 and 1.0, inclusive, for
the red, green, and blue components."
  (let ((len (- (length hex) 1)))
    (cond
     ((not (and (stringp hex) (= (aref hex 0) ?#)))
      (error "Invalid hexadecimal color notation: %S" hex))
     ((= len 3)
      (color-extras-hex-to-rgb (format "#%c%c%c%c%c%c"
                                       (aref hex 1) (aref hex 1)
                                       (aref hex 2) (aref hex 2)
                                       (aref hex 3) (aref hex 3))))
     ((= len 4)
      (mapcar (lambda (c) (/ c 65535.0))
              (color-extras-hex-to-rgb (format "#%c%c%c%c%c%c%c%c"
                                               (aref hex 1) (aref hex 1)
                                               (aref hex 2) (aref hex 2)
                                               (aref hex 3) (aref hex 3)
                                               (aref hex 4) (aref hex 4)))))
     ((= len 6)
      (mapcar (lambda (c) (/ c 255.0))
              (list (string-to-number (substring hex 1 3) 16)
                    (string-to-number (substring hex 3 5) 16)
                    (string-to-number (substring hex 5 7) 16))))
     ((= len 8)
      (mapcar (lambda (c) (/ c 65535.0))
              (list (string-to-number (substring hex 1 5) 16)
                    (string-to-number (substring hex 5 9) 16)
                    (string-to-number (substring hex 9 13) 16))))
     (t (error "Invalid hexadecimal color notation: %S" hex)))))

;;;;; Contrast

(autoload 'ct-contrast-ratio "ct")
(defun color-extras-contrast (&optional color1 color2)
  "Measure WCAG contrast ratio between COLOR1 and COLOR2."
  (interactive)
  (let* ((color1 (or color1 (color-extras-looking-at-color)
		     (read-string  "sColor (hex or comma-separated hsl): ")))
	 (color2 (or color2 (read-string "[optional] Other color (hex or comma-separated hsl): ")))
	 (parsed (color-extras-parse-color color1))
	 (color1-valid (cond
			((listp parsed)
			 (apply #'color-extras-hsl-to-hex (append (mapcar #'string-to-number parsed) '(percent))))
			((stringp parsed)
			 (if (string-match  "#" color1) color1 (concat "#" color1)))
			(t (user-error "Invalid color"))))
	 (white-contrast (ct-contrast-ratio color1-valid "#ffffff"))
	 (black-contrast (ct-contrast-ratio color1-valid "#000000"))
	 (color2-contrast (unless (string-empty-p color2)
			    (ct-contrast-ratio color1 color2)))
	 (message (format "White: %2f. Black: %2f." white-contrast black-contrast)))
    (message (if (string-empty-p color2)
		 message
	       (concat message (format " Chosen color: %2f" color2-contrast))))))

;;;;; Embark integration

(autoload 'simple-extras-string-at-point "simple-extras")
(defun color-extras-embark-color-finder ()
  "Return the HEX or HSL color value at point."
  (when-let* ((string (simple-extras-string-at-point))
   	      (patterns (list color-extras-hex-pattern
   			      color-extras-hsl-scaled-pattern)))
    (when (string-match (mapconcat #'identity patterns "\\|") string)
      (cons 'color (match-string-no-properties 0 string)))))

(defvar embark-general-map)
(declare-function embark-copy-as-kill "embark")
(with-eval-after-load 'embark
  (defvar-keymap color-extras-embark-map
    :doc "Embark key map for `color-extras'."
    :parent embark-general-map
    "c" #'color-extras-convert
    "w" #'embark-copy-as-kill)
  
  (defvar embark-keymap-alist)
  (add-to-list 'embark-keymap-alist '(color . color-extras-embark-map)))

(provide 'color-extras)
;;; color-extras.el ends here

