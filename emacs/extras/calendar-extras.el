;;; calendar-extras.el --- Extensions for calendar -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/calendar-extras.el
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

;; Extensions for `calendar'.

;;; Code:

(require 'calendar)

(defvar paths-dir-archive)
(defvar paths-file-calendar)
(declare-function org-get-deadline-time "org" (timestamp))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))

;;;; User options

(defgroup calendar-extras ()
  "Extensions for `calendar'."
  :group 'calendar)

(defcustom calendar-extras-location-name ""
  "Name of the current location.
The value can be set manually.  It can also be set via
`calendar-extras-set-geolocation'."
  :type 'string
  :group 'calendar-extras )

(defcustom calendar-extras-personal-geolocation '()
  "Personal geolocation."
  :type '(alist :key-type (string :tag "City")
		:value-type (plist :tag "Geolocation"
				   (float :tag "Latitude")
				   (float :tag "Longitude")
				   (string :tag "Timezone")))
  :group 'calendar-extras)

(defcustom calendar-extras-use-geolocation nil
  "Whether to use geolocation to set calendar variables."
  :type 'boolean
  :group 'calendar-extras)

(defcustom calendar-extras-archive-age 30
  "Number of days after which calendar entries are archived.
Entries whose deadline is more than this many days in the past
will be moved to the archive file by
`calendar-extras-archive-old-entries'."
  :type 'natnum
  :group 'calendar-extras)

;;;; Functions
;;;;; Geolocation

(defvar url-request-method)
(defvar url-request-extra-headers)
(autoload 'json-read "json")
(declare-function auth-source-pass-get "auth-source-pass")
(defun calendar-extras-get-geolocation ()
  "Get geolocation from IP address.
If IP is non-nil, use the local IP address."
  (let* ((key (auth-source-pass-get 'secret "tlon/core/api.geoapify.com"))
	 (url (format "https://api.geoapify.com/v1/ipinfo?&apiKey=%s" key))
	 (url-request-method "GET")
	 (url-request-extra-headers '(("Content-Type" . "application/json")))
	 (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      ;; Skip HTTP headers.
      (re-search-forward "^$")
      (let* ((json (json-read))
	     (location (alist-get 'location json))
	     (lat (alist-get 'latitude location))
	     (lon (alist-get 'longitude location))
	     (city (alist-get 'city json))
	     (names (alist-get 'names city))
	     (name (alist-get 'en names)))
	(list :lat lat :lon lon :city name)))))

;;;###autoload
(defun calendar-extras-set-geolocation ()
  "Set location variables from IP address."
  (interactive)
  (setq calendar-extras-personal-geolocation (calendar-extras-get-geolocation))
  (setq calendar-extras-location-name (plist-get calendar-extras-personal-geolocation :city))
  (message "Variables set: %s" calendar-extras-personal-geolocation))

;;;;; Misc

(defun calendar-extras-time-last-day-of-last-month ()
  "Insert the last day of the most recent month."
  (interactive)
  (let* ((date (calendar-current-date))
	 (is-january-p (when (eq (calendar-extract-month date) 1) t))
	 (raw-year (calendar-extract-year date))
	 (year (if is-january-p
		   (- raw-year 1)
		 raw-year))
	 (month (if is-january-p
		    12
		  (- (calendar-extract-month date) 1)))
	 (day (calendar-last-day-of-month month year)))
    (insert (format-time-string
	     "%Y-%m-%d"
	     (encode-time 0 0 0 day month year)))))

(autoload 'cfw:open-calendar-buffer "cfw:org")
(autoload 'cfw:org-create-source "cfw:org")
(defun calendar-extras-calfw-block-agenda ()
  "Display today’s agenda as visual time blocks."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source "medium purple"))
   :view 'block-day))

(defun calendar-extras-get-dates-in-range (start-date end-date)
  "Return a list of date strings from START-DATE to END-DATE inclusive."
  (let ((current (date-to-time start-date))
        (end (date-to-time end-date))
        result)
    (while (time-less-p current end)
      (push (format-time-string "%Y-%m-%d" current) result)
      (setq current (time-add current (days-to-time 1))))
    (push end-date result)  ; Include the end-date in the result
    (nreverse result)))

;;;;; Archive

;;;###autoload
(defun calendar-extras-archive-old-entries ()
  "Archive calendar entries older than `calendar-extras-archive-age' days.
Move top-level entries from `paths-file-calendar' whose deadline
is more than `calendar-extras-archive-age' days in the past to an
archive file in `paths-dir-archive'."
  (interactive)
  (require 'org)
  (require 'paths)
  (let ((archive-file (file-name-concat paths-dir-archive "calendar.org_archive.org"))
	(cutoff (time-subtract (current-time)
			       (days-to-time calendar-extras-archive-age)))
	(archived 0)
	entries-to-archive)
    (with-current-buffer (find-file-noselect paths-file-calendar)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward "^\\* " nil t)
	    (beginning-of-line)
	    (let* ((deadline (org-get-deadline-time (point)))
		   (subtree-end (save-excursion
				  (org-end-of-subtree t t)
				  (point))))
	      (when (and deadline (time-less-p deadline cutoff))
		(push (cons (point) subtree-end) entries-to-archive))
	      (goto-char subtree-end)))))
      ;; Delete in reverse order to preserve buffer positions.
      (setq entries-to-archive
	    (sort entries-to-archive (lambda (a b) (> (car a) (car b)))))
      (dolist (entry entries-to-archive)
	(let ((text (buffer-substring (car entry) (cdr entry))))
	  (with-current-buffer (find-file-noselect archive-file)
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (insert text))
	  (delete-region (car entry) (cdr entry))
	  (setq archived (1+ archived))))
      (save-buffer))
    (when (> archived 0)
      (with-current-buffer (find-file-noselect archive-file)
	(save-buffer)))
    (message "Archived %d %s" archived
	     (if (= archived 1) "entry" "entries"))))

(provide 'calendar-extras)

;;; calendar-extras.el ends here
