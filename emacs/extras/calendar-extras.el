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
(require 'paths)

;;;; User options

(defgroup calendar-extras ()
  "Extensions for `calendar'."
  :group 'calendar)

(defcustom calendar-extras-personal-geolocation '()
  "Personal geolocation."
  :type '(alist :key-type (string :tag "City")
		:value-type (plist :tag "Geolocation"
				   (float :tag "Latitude")
				   (float :tag "Longitude")
				   (string :tag "Timezone")))
  :group 'calendar-extras)

;;;; Functions

(defun calendar-extras-get-local-ip-address ()
  "Get local IP address."
  (interactive)
  (let ((local-ip (shell-command-to-string
                   "dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com")))
    (string-trim local-ip "\"" "\"\n")))

(defun calendar-extras-get-geolocation-from-ip (&optional ip)
  "Get geolocation from IP address."
  (require 'url-vars)
  (require 'json)
  (let* ((ip (or ip (calendar-extras-get-local-ip-address)))
         (url (format "http://ip-api.com/json/%s" ip))
         (url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (response-buffer (url-retrieve-synchronously url))
         (json-object-type 'plist))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      ;; Skip HTTP headers.
      (re-search-forward "^$")
      (let* ((json (json-read))
             (lat (plist-get json :lat))
             (lon (plist-get json :lon))
             (city (plist-get json :city))
             (timezone (plist-get json :timezone)))
        (list :lat lat :lon lon :city city :timezone timezone)))))

(defun calendar-extras-time-last-day-of-last-month ()
  "Insert the last day of the most recent month."
  (interactive)
  (let* ((date (calendar-current-date))
         (year (calendar-extract-year date))
         (month (- (calendar-extract-month date) 1))
         (day (calendar-last-day-of-month month year)))
    (insert (format-time-string
             "%Y-%m-%d"
             (encode-time 0 0 0 day month year)))))

(defun calendar-extras-calfw-block-agenda ()
  "Display today’s agenda as visual time blocks."
  (interactive)
  (require 'calfw-blocks)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'block-day))

(provide 'calendar-extras)

;;; calendar-extras.el ends here
