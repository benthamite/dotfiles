;;; ledger-mode-extras.el --- Extensions for ledger-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ledger-mode-extras.el
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

;; Extensions for `ledger-mode'.

;;; Code:

(require 'crux)
(require 'f)
(require 'ledger-mode)
(require 'paths)
(require 's)

;;;; Variables

(defgroup ledger-extras ()
  "Extensions for `ledger'."
  :group 'ledger-extras)

;;;; Functions

(defun ledger-extras-import-polymarket (file)
  "Import Polymarket CSV FILE into the current ledger file."
  (interactive "f")
  (let (token-alist)
    (dolist (raw (s-split "\n" (f-read file) t))
      (let* ((clean (split-string (replace-regexp-in-string "\"" "" raw) "," t))
	     (sign (if (string= (nth 1 clean) "Buy") 1 -1))
	     (proceeds (float (string-to-number (nth 2 clean))))
	     (quantity (float (* sign (string-to-number (nth 3 clean)))))
	     (price (abs (/ proceeds quantity)))
	     (token-name (string-trim (nth 0 clean)))
	     (token-symbol
	      (if-let ((match (alist-get token-name token-alist nil nil #'string=)))
		  match
		(read-string (format "Token symbol for `%s': " token-name))))
	     (date (format-time-string "%Y-%m-%d" (seconds-to-time (string-to-number (nth 5 clean)))))
	     (payee "Polymarket")
	     (account "Assets:Polymarket"))
	(push (cons token-name token-symbol) token-alist)
	(ledger-mode-extras-insert-transaction (list payee date account quantity token-symbol price))))))

(defun ledger-mode-extras-insert-transaction (fields &optional file)
  "Insert a new transaction with FIELDS at the end of FILE.
If FILE is nil, use `paths-file-ledger'."
  (let ((file (or file paths-file-ledger)))
    (cl-destructuring-bind (payee date account quantity token-symbol price) fields
      (with-current-buffer (find-file-noselect file)
	(goto-char (point-max))
	(insert (format "%s %s\n     %3$s  %s %s @ %s USD\n     %3$s"
			payee date account quantity token-symbol price))
	(ledger-mode-extras-align-and-next)
	(insert "\n\n")))))

(defun ledger-mode-extras-new-entry-below ()
  "Create new entry below one at point."
  (interactive)
  (require 'crux)
  (indent-for-tab-command)
  (ledger-navigate-next-xact-or-directive)
  (crux-smart-open-line-above))

(defun ledger-mode-extras-align-and-next ()
  "Align transaction at point and move point to next entry."
  (interactive)
  (ledger-post-align-xact (point))
  (ledger-navigate-next-xact-or-directive))

(defun ledger-mode-extras-report-account ()
  "Run an `account' report from `ledger-reports'."
  (interactive)
  (ledger-report "account" nil))

(defun ledger-mode-extras-report-net-worth ()
  "Run an `net worth' report from `ledger-reports'."
  (interactive)
  (ledger-report "net worth" nil))

(defun ledger-mode-extras-report-net-worth-USD ()
  "Run an `net worth (USD)' report from `ledger-reports'."
  (interactive)
  (ledger-report "net worth (USD)" nil))

(defun mode-mode-extras-extras-report-net-worth-USD ()
  "Run a `payee' report from `ledger-reports'."
  (interactive)
  (ledger-report "payee" nil))

(defun ledger-mode-extras-update-commodities ()
  "Update `commodities.py'."
  (interactive)
  (async-shell-command
   (format "python3 %s"
	   (file-name-concat paths-dir-ledger "commodities.py"))))

(defun ledger-mode-extras-update-coin-prices ()
  "Update `coinprices.py'."
  (interactive)
  (async-shell-command
   (format "python3 %s >> %s"
	   (file-name-concat paths-dir-ledger "coinprices/coinprices.py")
	   paths-file-ledger-db)))

(defun ledger-mode-extras-sort-region-reversed (beg end)
  "Sort the region from BEG to END in reverse chronological order."
  (interactive "r") ;; load beg and end from point and mark
  ;; automagically
  (let* ((new-beg beg)
	 (new-end end)
	 (bounds (ledger-navigate-find-xact-extents (point)))
	 (point-delta (- (point) (car bounds)))
	 (target-xact (buffer-substring (car bounds) (cadr bounds)))
	 (inhibit-modification-hooks t))
    (save-excursion
      (save-restriction
	(goto-char beg)
	;; make sure beg of region is at the beginning of a line
	(beginning-of-line)
	;; make sure point is at the beginning of a xact
	(unless (looking-at ledger-payee-any-status-regex)
	  (ledger-navigate-next-xact))
	(setq new-beg (point))
	(goto-char end)
	(ledger-navigate-next-xact)
	;; make sure end of region is at the beginning of next record
	;; after the region
	(setq new-end (point))
	(narrow-to-region new-beg new-end)
	(goto-char new-beg)

	(let ((inhibit-field-text-motion t))
	  (sort-subr
	   t
	   'ledger-navigate-next-xact
	   'ledger-navigate-end-of-xact
	   'ledger-sort-startkey))))

    (goto-char (point-min))
    (re-search-forward (regexp-quote target-xact))
    (goto-char (+ (match-beginning 0) point-delta))))

(defun ledger-mode-extras-sort-buffer-reversed ()
  "Sort the entire buffer in reverse chronological order."
  (interactive)
  (let (sort-start
	sort-end)
    (save-excursion
      (goto-char (point-min))
      (setq sort-start (ledger-sort-find-start)
	    sort-end (ledger-sort-find-end)))
    (ledger-mode-extras-sort-region-reversed (or sort-start (point-min))
					     (or sort-end (point-max)))))

(defun ledger-mode-extras-sort-region-or-buffer ()
  "Sort a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (call-interactively #'ledger-sort-region)
    (ledger-sort-buffer)))

(defun ledger-mode-extras-sort-region-or-buffer-reversed ()
  "Sort in reverse chronological order a region if selected, else whole buffer."
  (interactive)
  (if (region-active-p)
      (call-interactively #'ledger-mode-extras-sort-region-reversed)
    (ledger-mode-extras-sort-buffer-reversed)))

(defun ledger-mode-extras-copy-or-kill-transaction-at-point (action)
  "Copy or kill transaction at point, depending on ACTION."
  (save-excursion
    (ledger-navigate-next-xact-or-directive)
    (let ((end (point))
	  (fun (pcase action
		 ('copy #'copy-region-as-kill)
		 ('kill #'kill-region))))
      (ledger-navigate-prev-xact-or-directive)
      (funcall fun (point) end))))

(defun ledger-mode-extras-kill-transaction-at-point ()
  "Kill transaction at point."
  (interactive)
  (ledger-mode-extras-copy-or-kill-transaction-at-point 'kill))

(defun ledger-mode-extras-copy-transaction-at-point ()
  "Copy transaction at point."
  (interactive)
  (ledger-mode-extras-copy-or-kill-transaction-at-point 'copy))

(defun ledger-mode-extras-narrow-to-xact ()
  "Narrow to the current transaction."
  (interactive)
  (let ((xact-begins (ledger-navigate-beginning-of-xact))
	(xact-ends (ledger-navigate-end-of-xact)))
    (narrow-to-region xact-begins xact-ends)))

(defun ledger-mode-extras--increase-date (days)
  "Increase the date of transaction at point by DAYS.
DAYS can be positive or negative."
  (save-excursion
    (ledger-mode-extras-narrow-to-xact)
    (let* ((date (ledger-xact-date))
	   (timestamp (date-to-time date))
	   (date-minus-days (format-time-string "%Y-%m-%d" (time-add timestamp (days-to-time days)))))
      (goto-char (point-min))
      (re-search-forward ledger-iso-date-regexp nil t)
      (replace-match date-minus-days)
      (widen))))

(defun ledger-mode-extras-increase-date-by-one-day ()
  "Increase the date of transaction at point by one day."
  (interactive)
  (ledger-mode-extras--increase-date 1))

(defun ledger-mode-extras-decrease-date-by-one-day ()
  "Decrease the date of transaction at point by one day."
  (interactive)
  (ledger-mode-extras--increase-date -1))

(provide 'ledger-mode-extras)
;;; ledger-mode-extras.el ends here
