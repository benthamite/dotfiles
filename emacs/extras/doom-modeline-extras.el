;;; doom-modeline-extras.el --- Extensions for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/doom-modeline-extras.el
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

;; Extensions for `doom-modeline'.

;;; Code:

(require 'doom-modeline)
(require 'el-patch)

;;;; User options

(defgroup doom-modeline-extras ()
  "Extensions for `doom-modeline'."
  :group 'doom-modeline)

(defcustom doom-modeline-extras-gptel t
  "Whether to display the `gptel' model in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-gptel-cost t
  "Whether to display the `gptel' model cost in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-org-roam t
  "Whether to display the `org-roam' backlink count in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-tlon-split t
  "Whether to display if `tlon-split-mode' is active in the modeline."
  :type 'boolean
  :group 'doom-modeline)

;;;; Functions

;;;;; Modeline segments

;;;;;; gptel

(defvar gptel-mode)
(defvar gptel-model)
(doom-modeline-def-segment gptel ()
  "Display the `gptel' model active in the current buffer."
  (when (doom-modeline-extras-display-gptel-segment-t)
    (concat (symbol-name gptel-model) (doom-modeline-spc))))

(defvar gptel-context--alist)
(declare-function gptel-extras-get-cost "gptel-extras")
(doom-modeline-def-segment gptel-cost ()
  "Display the cost of prompting the current model."
  (when (doom-modeline-extras-display-gptel-segment-t)
    (concat (format "$%.2f" (gptel-extras-get-cost))
	    (let ((filenum (length gptel-context--alist)))
	      (unless (zerop filenum)
		(format " (%d)" filenum)))
	    (doom-modeline-spc))))

(defun doom-modeline-extras-display-gptel-segment-t ()
  "Whether to display the `gptel' segment in the modeline."
  (and doom-modeline-extras-gptel
       (bound-and-true-p gptel-model)
       gptel-mode))

;;;;;; tlon

(defvar tlon-split-mode)
(doom-modeline-def-segment tlon-split ()
  "Display \"split\" in the modeline when `tlon-split-mode' is enabled."
  (when (and doom-modeline-extras-tlon-split
	     (bound-and-true-p tlon-split-mode))
    (concat "split" (doom-modeline-spc))))

;;;;;; org-roam

(defvar org-roam-extras-current-backlink-count)
(doom-modeline-def-segment org-roam-backlinks
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p org-roam-extras-current-backlink-count))
    (concat (doom-modeline-spc) (format "%dB" org-roam-extras-current-backlink-count))))

(provide 'doom-modeline-extras)
;;; doom-modeline-extras.el ends here
