;;; org-gcal-extras.el --- Extensions for org-gcal -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-gcal-extras.el
;; Version: 0.2
;; Package-Requires: ((org-gcal "0.4") (el-patch "1.1"))

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

;; Extensions for `org-gcal'.

;;; Code:

(require 'org-gcal)
(require 'el-patch)
(require 'transient)

;;;; Functions

;;;###autoload
(defun org-gcal-extras-open-at-point ()
  "Open the event at point in a Google Calendar."
  (interactive)
  (if-let ((id (org-entry-get nil "entry-id")))
      (browse-url
       (concat
	"https://calendar.google.com/calendar/u/0/r/eventedit/"
	(replace-regexp-in-string "\n" ""
				  (base64-encode-string
				   (replace-regexp-in-string "/" " " id))))))
  (user-error "No id found"))

(transient-define-prefix org-gcal-extras-menu ()
  "Dispatch an `org-gcal' command."
  [["Fetch"
    ("f" "fetch all" org-gcal-fetch)
    ("F" "fetch buffer" org-gcal-fetch-buffer)]
   ["Sync"
    ("s" "sync all" org-gcal-sync)
    ("S" "sync buffer" org-gcal-sync-buffer)]
   ["Act"
    ("p" "post at point" org-gcal-post-at-point)
    ("o" "open at point" org-gcal-extras-open-at-point)
    ("d" "delete at point" org-gcal-delete-at-point)]
   ["Setup"
    ("u" "unlock sync" org-gcal--sync-unlock)
    ("c" "clear token" org-gcal-sync-tokens-clear)
    ("t" "toggle debug" org-gcal-toggle-debug)
    ("l" "reload secret" org-gcal-reload-client-id-secret)]])

;;;;; Patched functions

(el-patch-defun org-gcal--get-time-and-desc ()
  "Get the timestamp and description of the event at point.

  Return a plist with :start, :end, and :desc keys. The value for a key is nil
  if not present."
  (let (start end desc tobj elem)
    (save-excursion
      (org-gcal--back-to-heading)
      (setq elem (org-element-at-point))
      ;; Parse :org-gcal: drawer for event time and description.
      (when
	  (re-search-forward
	   (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
	   (save-excursion (outline-next-heading) (point))
	   'noerror)
	;; First read any event time from the drawer if present. It's located
	;; at the beginning of the drawer.
	(save-excursion
	  (when
	      (re-search-forward "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
				 (save-excursion (outline-next-heading) (point))
				 'noerror)
	    (goto-char (match-beginning 0))
	    (setq tobj (org-element-timestamp-parser))))
	;; Lines after the timestamp contain the description. Skip leading
	;; blank lines.
	(forward-line)
	(beginning-of-line)
	(re-search-forward
	 "\\(?:^[ \t]*$\\)*\\([^z-a]*?\\)\n?[ \t]*:END:"
	 (save-excursion (outline-next-heading) (point)))
	(setq desc (match-string-no-properties 1))
	(setq desc
	      (if (string-match-p "\\‘\n*\\’" desc)
		  nil
		(replace-regexp-in-string
		 "^✱" "*"
		 (replace-regexp-in-string
		  "\\`\\(?: *<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*?>$\\)\n?\n?"
		  ""
		  (replace-regexp-in-string
		   " *:PROPERTIES:\n *\\(.*\\(?:\n.*\\)*?\\) *:END:\n+"
		   ""
		   desc)))))))
    ;; Prefer to read event time from the SCHEDULED property if present.
    (setq tobj (or (el-patch-swap (org-element-property :scheduled elem)
				  (org-element-property :deadline elem))
		   tobj))
    (when tobj
      (when (plist-get (cadr tobj) :year-start)
	(setq
	 start
	 (org-gcal--format-org2iso
	  (plist-get (cadr tobj) :year-start)
	  (plist-get (cadr tobj) :month-start)
	  (plist-get (cadr tobj) :day-start)
	  (plist-get (cadr tobj) :hour-start)
	  (plist-get (cadr tobj) :minute-start)
	  (when (plist-get (cadr tobj) :hour-start) t))))
      (when (plist-get (cadr tobj) :year-end)
	(setq
	 end
	 (org-gcal--format-org2iso
	  (plist-get (cadr tobj) :year-end)
	  (plist-get (cadr tobj) :month-end)
	  (plist-get (cadr tobj) :day-end)
	  (plist-get (cadr tobj) :hour-end)
	  (plist-get (cadr tobj) :minute-end)
	  (when (plist-get (cadr tobj) :hour-end) t)))))
    (list :start start :end end :desc desc)))

;; Do not create a new `org-gcal' drawer; insert description as part of the heading contents;
;; set date as a deadline.
(with-no-warnings
  (el-patch-defun org-gcal--update-entry (calendar-id event &optional update-mode)
    "Update the entry at the current heading with information from EVENT.

EVENT is parsed from the Calendar API JSON response using ‘org-gcal--json-read’.
CALENDAR-ID must be passed as well. Point must be located on an Org-mode heading
line or an error will be thrown. Point is not preserved.

If UPDATE-MODE is passed, then the functions in
‘org-gcal-after-update-entry-functions' are called in order with the same
arguments as passed to this function and the point moved to the beginning of the
heading."
    (unless (org-at-heading-p)
      (user-error "Must be on Org-mode heading."))
    (let* ((smry  (plist-get event :summary))
	   (desc  (plist-get event :description))
	   (loc   (plist-get event :location))
	   (source (plist-get event :source))
	   (transparency   (plist-get event :transparency))
	   (_link  (plist-get event :htmlLink))
	   (meet  (plist-get event :hangoutLink))
	   (etag (plist-get event :etag))
	   (event-id    (plist-get event :id))
	   (stime (plist-get (plist-get event :start)
			     :dateTime))
	   (etime (plist-get (plist-get event :end)
			     :dateTime))
	   (sday  (plist-get (plist-get event :start)
			     :date))
	   (eday  (plist-get (plist-get event :end)
			     :date))
	   (start (if stime (org-gcal--convert-time-to-local-timezone stime org-gcal-local-timezone) sday))
	   (end   (if etime (org-gcal--convert-time-to-local-timezone etime org-gcal-local-timezone) eday))
	   (old-time-desc (org-gcal--get-time-and-desc))
	   (old-start (plist-get old-time-desc :start))
	   (old-end (plist-get old-time-desc :start))
	   (recurrence (plist-get event :recurrence))
	   (elem))
      (when loc (replace-regexp-in-string "\n" ", " loc))
      (org-edit-headline
       (cond
	;; Don’t update headline if the new summary is the same as the CANCELLED
	;; todo keyword.
	((equal smry org-gcal-cancelled-todo-keyword) (org-gcal--headline))
	(smry smry)
	;; Set headline to “busy” if there is no existing headline and no summary
	;; from server.
	((or (null (org-gcal--headline))
	     (string-empty-p (org-gcal--headline)))
	 "busy")
	(t (org-gcal--headline))))
      (org-entry-put (point) org-gcal-etag-property etag)
      (when recurrence (org-entry-put (point) "recurrence" (format "%s" recurrence)))
      (when loc (org-entry-put (point) "LOCATION" loc))
      (when source
	(let ((roam-refs
	       (org-entry-get-multivalued-property (point) "ROAM_REFS"))
	      (link (org-entry-get (point) "link")))
	  (cond
	   ;; ROAM_REFS can contain multiple references, but only bare URLs are
	   ;; supported. To make sure we can round-trip between ROAM_REFS and
	   ;; Google Calendar, only import to ROAM_REFS if there is no title in
	   ;; the source, and if ROAM_REFS has at most one entry.
	   ((and (null link)
		 (<= (length roam-refs) 1)
		 (or (null (plist-get source :title))
		     (string-empty-p (plist-get source :title))))
	    (org-entry-put (point) "ROAM_REFS"
			   (plist-get source :url)))
	   (t
	    (org-entry-put (point) "link"
			   (org-link-make-string
			    (plist-get source :url)
			    (plist-get source :title)))))))
      (when transparency (org-entry-put (point) "TRANSPARENCY" transparency))
      (when meet
	(org-entry-put
	 (point)
	 "HANGOUTS"
	 (format "[[%s][%s]]"
		 meet
		 "Join Hangouts Meet")))
      (org-entry-put (point) org-gcal-calendar-id-property calendar-id)
      (org-gcal--put-id (point) calendar-id event-id)
      ;; Insert event time and description in :ORG-GCAL: drawer, erasing the
      ;; current contents.
      (org-gcal--back-to-heading)
      (setq elem (org-element-at-point))
      (save-excursion
	(when (re-search-forward
	       (format
		"^[ \t]*:%s:[^z-a]*?\n[ \t]*:END:[ \t]*\n?"
		(regexp-quote org-gcal-drawer-name))
	       (save-excursion (outline-next-heading) (point))
	       'noerror)
	  (replace-match "" 'fixedcase)))
      (unless (re-search-forward ":PROPERTIES:[^z-a]*?:END:"
				 (save-excursion (outline-next-heading) (point))
				 'noerror)
	(message "PROPERTIES not found: %s (%s) %d"
		 (buffer-name) (buffer-file-name) (point)))
      (end-of-line)
      (newline)
      (el-patch-remove (insert (format ":%s:" org-gcal-drawer-name)))
      (el-patch-remove (newline))
      ;; Keep existing timestamps for parent recurring events.
      (when (and recurrence old-start old-end)
	(setq start old-start
	      end old-end))
      (let*
	  ((timestamp
	    (if (or (string= start end) (org-gcal--alldayp start end))
		(org-gcal--format-iso2org start)
	      (if (and
		   (= (plist-get (org-gcal--parse-date start) :year)
		      (plist-get (org-gcal--parse-date end)   :year))
		   (= (plist-get (org-gcal--parse-date start) :mon)
		      (plist-get (org-gcal--parse-date end)   :mon))
		   (= (plist-get (org-gcal--parse-date start) :day)
		      (plist-get (org-gcal--parse-date end)   :day)))
		  (format "<%s-%s>"
			  (org-gcal--format-date start "%Y-%m-%d %a %H:%M")
			  (org-gcal--format-date end "%H:%M"))
		(format "%s--%s"
			(org-gcal--format-iso2org start)
			(org-gcal--format-iso2org
			 (if (< 11 (length end))
			     end
			   (org-gcal--iso-previous-day end))))))))
	(if (el-patch-swap (org-element-property :scheduled elem)
			   (org-element-property :deadline elem))
	    (unless (and recurrence old-start)
	      ;; Ensure CLOSED timestamp isn’t wiped out by ‘org-gcal-sync’ (see
	      ;; https://github.com/kidd/org-gcal.el/issues/218).
	      (let ((org-closed-keep-when-no-todo t))
		(el-patch-swap (org-schedule nil timestamp)
			       (org-deadline nil timestamp))))
	  (el-patch-swap (insert timestamp) (org-deadline nil timestamp))
	  (el-patch-remove (newline))
	  (when desc (newline))))
      ;; Insert event description if present.
      (when desc
	(insert (replace-regexp-in-string "^\*" "✱" desc))
	(insert (if (string= "\n" (org-gcal--safe-substring desc -1)) "" "\n")))
      (el-patch-remove (insert ":END:"))
      (when (org-gcal--event-cancelled-p event)
	(save-excursion
	  (org-back-to-heading t)
	  (org-gcal--handle-cancelled-entry)))
      (when update-mode
	(cl-dolist (f org-gcal-after-update-entry-functions)
	  (save-excursion
	    (org-back-to-heading t)
	    (funcall f calendar-id event update-mode)))))))

(provide 'org-gcal-extras)
;;; org-gcal-extras.el ends here
