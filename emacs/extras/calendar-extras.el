;;; calendar-extras.el --- Extensions for calendar -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/calendar-extras.el
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

;; Extensions for `calendar'.

;;; Code:

(require 'calendar)
(require 'auth-source-pass)

;;;; User options

(defgroup calendar-extras ()
  "Extensions for `calendar'."
  :group 'calendar)

(defcustom calendar-extras-location-name ""
  "Name of the current location.
The value can be set manually. It can also be set via
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

;;;; Functions
;;;;; Geolocation

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

(declare-function cfw:open-calendar-buffer "cfw:org")
(declare-function cfw:org-create-source "cfw:org")
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

(provide 'calendar-extras)

;;; calendar-extras.el ends here
