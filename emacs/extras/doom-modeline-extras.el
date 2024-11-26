;;; doom-modeline-extras.el --- Extensions for doom-modeline -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/doom-modeline-extras.el
;; Version: 0.2
;; Package-Requires: ((doom-modeline "2.0"))

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

;;;; Variables

(defvar doom-modeline-extras-github-last-count 0
  "Last count of GitHub notifications.")

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

;;;;; GitHub notifications

(declare-function forge-pull-notifications "forge-commands")
(defun doom-modeline-extras-handle-github-notifications (&rest _)
  "Handle GitHub notifications after they are fetched."
  (unless (= doom-modeline--github-notification-number doom-modeline-extras-github-last-count)
    (when (> doom-modeline--github-notification-number 0)
      (forge-pull-notifications)
      (message "Pulled forge notifications.")))
  (setq doom-modeline-extras-github-last-count doom-modeline--github-notification-number))

(add-hook 'doom-modeline-after-github-fetch-notification-hook #'doom-modeline-extras-handle-github-notifications)

(provide 'doom-modeline-extras)
;;; doom-modeline-extras.el ends here
