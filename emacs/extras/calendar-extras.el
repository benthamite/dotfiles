;;; calendar-extras.el --- Extensions for calendar -*- lexical-binding: t -*-

;; Copyright (C) 2023

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
(require 'json)
(require 'url-vars)

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

(defun calendar-extras-get-local-ip-address ()
  "Get local IP address."
  (let ((local-ip (shell-command-to-string
		   "timeout 1 curl -s ifconfig.me | awk '/[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*/{print $0}'")))
    (string-trim local-ip "\"" "\n")))

(defun calendar-extras-get-geolocation-from-ip (&optional ip)
  "Get geolocation from IP address.
If IP is non-nil, use the local IP address."
  (let* ((ip (or ip (calendar-extras-get-local-ip-address)))
	 (url (format "http://ip-api.com/json/%s" ip))
	 (url-request-method "GET")
	 (url-request-extra-headers '(("Content-Type" . "application/json")))
	 (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      ;; Skip HTTP headers.
      (re-search-forward "^$")
      (let* ((json (json-read))
	     (lat (alist-get 'lat json))
	     (lon (alist-get 'lon json))
	     (city (alist-get 'city json))
	     (timezone (alist-get 'timezone json)))
	(list :lat lat :lon lon :city city :timezone timezone)))))

  "Set location variables from IP address.
If IP is non-nil, use the local IP address."
(defun calendar-extras-set-geolocation ()
  (interactive)
  (when-let ((ip (or ip (calendar-extras-get-local-ip-address))))
    (setq calendar-extras-personal-geolocation (calendar-extras-get-geolocation-from-ip ip))
    (setq calendar-extras-location-name (plist-get calendar-extras-personal-geolocation :city))
    (message "Variables set: %s" calendar-extras-personal-geolocation)))

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

(provide 'calendar-extras)

;;; calendar-extras.el ends here
