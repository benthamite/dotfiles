;;; ledger-mode-extras.el --- Extensions for ledger-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

(require 'ledger-mode)
(require 'paths)

;;;; Variables

(defgroup ledger-mode-extras ()
  "Extensions for `ledger-mode'."
  :group 'ledger-mode)

(defcustom ledger-mode-extras-currencies '()
  "List of currencies to get prices for."
  :group 'ledger-mode-extras
  :type 'list)

;;;; Functions

(declare-function f-read "f")
(declare-function s-split "s")
;; TODO: remove first line and "rewards" lines
(defun ledger-extras-import-polymarket (file)
  "Import Polymarket CSV FILE into the current ledger file.
To download the CSV file, go to <https://polymarket.com/portfolio?tab=history>,
click on \"Filter\", set a range from one day after the most recent Polymarket
transaction on Ledger to today. Remove the first row of the CSV file and the
‘Reward’ lines before importing it. Note that if you held a contract until
expiration, you must set its resolution value manually."
  (interactive (list (read-file-name "Polymarket CSV file: " paths-dir-downloads)))
  (let (token-alist)
    (dolist (raw (s-split "\n" (f-read file) t))
      (let* ((clean (split-string (replace-regexp-in-string "\"" "" raw) "," t))
	     (payee "Polymarket")
	     (date (format-time-string "%Y-%m-%d" (seconds-to-time (string-to-number (nth 5 clean)))))
	     (account "Assets:Polymarket")
	     (sign (pcase (nth 1 clean) ("Buy" 1) ("Sell" -1)
			  (_ (user-error "Unknown transaction type `%s'" (nth 1 clean)))))
	     (quantity (float (* sign (string-to-number (nth 3 clean)))))
	     (token-name (string-trim (nth 0 clean)))
	     (token-symbol
	      (if-let ((match (alist-get token-name token-alist nil nil #'string=)))
		  match
		(read-string (format "Token symbol for `%s': " token-name))))
	     (proceeds (float (string-to-number (nth 2 clean))))
	     (price (abs (/ proceeds quantity))))
	(push (cons token-name token-symbol) token-alist)
	(ledger-mode-extras-insert-transaction (list payee date account quantity token-symbol price nil))))))

(defun ledger-extras-import-interactive-brokers (file)
  "Import Interactive Brokers CSV FILE into the current ledger file.
To download the CSV file, go to the IBKR site, click on \"performance & reports
> flex queries > trade history > run >\", then select your desired period. Note
that IBKR sometimes adds an extra line with the total amount of a transaction
broken down into multiple trades. You must remove these lines manually."
  (interactive (list (read-file-name "IBKR CSV file: " paths-dir-downloads)))
  (dolist (raw (s-split "\n" (f-read file) t))
    (let* ((clean (split-string (replace-regexp-in-string "\"" "" raw) "," t))
	   (payee "Interactive Brokers")
	   (date (ledger-extras-convert-interactive-brokers-date (nth 0 clean)))
	   (account "Assets:Interactive Brokers")
	   (quantity (format "%.2f" (string-to-number (nth 2 clean))))
	   (token-symbol (nth 1 clean))
	   (price (nth 3 clean))
	   (fees (float (* -1 (string-to-number (nth 5 clean))))))
      (ledger-mode-extras-insert-transaction (list payee date account quantity token-symbol price fees)))))

(defun ledger-extras-convert-interactive-brokers-date (string)
  "Convert an Interactive Brokers date STRING to the YYYY-MM-DD date format."
  (let ((year (substring string 0 4))
        (month (substring string 4 6))
        (day (substring string 6 8)))
    (concat year "-" month "-" day)))

(defun ledger-mode-extras-insert-transaction (fields &optional file)
  "Insert a new transaction with FIELDS at the end of FILE.
If FILE is nil, use `paths-file-ledger'."
  (let ((file (or file paths-file-ledger)))
    (cl-destructuring-bind (payee date account quantity token-symbol price fees) fields
      (let ((first-line (format "%s %s" date payee))
	    (second-line (format "     %s  %s %s @ %s USD" account quantity token-symbol price))
	    (third-line (when fees (format "    Expenses:Fees  %s USD" fees)))
	    (last-line (format "     %s" account)))
	(with-current-buffer (find-file-noselect file)
	  (goto-char (point-max))
	  (insert (mapconcat 'identity (delq nil (list first-line second-line third-line last-line)) "\n"))
	  (ledger-mode-extras-align-and-next)
	  (insert "\n\n"))))))

(declare-function crux-smart-open-line-above "crux")
(defun ledger-mode-extras-new-entry-below ()
  "Create new entry below one at point."
  (interactive)
  (require 'crux)
  (indent-for-tab-command)
  (ledger-navigate-next-xact-or-directive)
  (crux-smart-open-line-above))

;;;###autoload
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
