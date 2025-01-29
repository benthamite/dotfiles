;;; profiler-extras.el --- Assorted functions for Pablo's config -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/profiler-extras.el
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

;; This file contains assorted functions that are used in my config.

;;; Code:

(require 'profiler)

;;;; Variables

(defvar profiler-extras-profiler-toggle nil)

;;;; Functions

;;;###autoload
(defun profiler-extras-profiler-toggle ()
  "Start or stop Emacs profiler and generate profiling report."
  (interactive)
  (if (not profiler-extras-profiler-toggle)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq profiler-extras-profiler-toggle (not profiler-extras-profiler-toggle)))

;;;###autoload
(defun profiler-extras-profiler-report-toggle-entry-global ()
  "Expand all subentries below entry at point."
  (interactive)
  (profiler-report-toggle-entry '(4)))

(provide 'profiler-extras)
;;; profiler-extras.el ends here
