;;; auth-source-extras.el --- Extensions for auth-source -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/auth-source-extras.el
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

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

;; Extensions for `auth-source'.
;;
;; Read secrets from a 1Password "Automation" vault with a read-only service
;; account whose token lives in the macOS Keychain, so background reads never
;; prompt.  Items are fetched in one batch per account and kept in memory for
;; the rest of the session.

;;; Code:

(require 'seq)
(require 'subr-x)

;;;; User options

(defgroup auth-source-extras ()
  "Extensions for `auth-source'."
  :group 'auth-source)

(defcustom auth-source-extras-op-program "/opt/homebrew/bin/op"
  "Path to the 1Password CLI used with service-account tokens."
  :type 'file
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-accounts
  '((personal . "op-service-account/personal-automation")
    (tlon . "op-service-account/tlon-automation"))
  "Alist of 1Password accounts and the Keychain services holding their tokens.
Each key is a symbol naming the account; each value is the service name of a
generic password in the login Keychain whose secret is a read-only service
account token for that account's automation vault."
  :type '(alist :key-type symbol :value-type string)
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-vault "Automation"
  "Name of the vault the service accounts can read."
  :type 'string
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-prefetch nil
  "Alist of accounts and the item titles to fetch together on first use.
When a secret from an account is first requested, every listed item of that
account is fetched in the same batch, so a session needs one round trip per
account instead of one per item."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-timeout 30
  "Seconds to wait for a batch of item fetches before giving up."
  :type 'number
  :group 'auth-source-extras)

;;;; Variables

(defvar auth-source-extras--op-cache (make-hash-table :test #'equal)
  "Fields of fetched items, keyed by (ACCOUNT . TITLE).
Each value is an alist of field labels and IDs to values, or the symbol
`missing' when the item could not be fetched.")

(defvar auth-source-extras--op-tokens (make-hash-table :test #'eq)
  "Service-account tokens by account, or `missing' when none is stored.")

;;;; Functions

(defun auth-source-extras-op-get (item field &optional account)
  "Return FIELD of ITEM in ACCOUNT's automation vault, or nil.
ITEM is the item title and FIELD a field label or ID, such as \"credential\"
or \"password\".  ACCOUNT is a key of `auth-source-extras-op-accounts' and
defaults to `personal'.  A missing token or item is reported once as a warning
and then returns nil."
  (let ((account (or account 'personal)))
    (unless (gethash (cons account item) auth-source-extras--op-cache)
      (auth-source-extras--op-fetch
       account (auth-source-extras--op-titles-to-fetch account item)))
    (let ((fields (gethash (cons account item) auth-source-extras--op-cache)))
      (when (listp fields)
        (cdr (assoc field fields))))))

(defun auth-source-extras--op-titles-to-fetch (account item)
  "Return ITEM plus the uncached prefetch titles of ACCOUNT."
  (seq-uniq
   (cons item
         (seq-remove (lambda (title)
                       (gethash (cons account title) auth-source-extras--op-cache))
                     (alist-get account auth-source-extras-op-prefetch)))))

(defun auth-source-extras--op-fetch (account titles)
  "Fetch TITLES from ACCOUNT's automation vault into the cache."
  (condition-case err
      (let ((items (auth-source-extras--op-items (auth-source-extras--op-token account)
                                                 titles)))
        (dolist (title titles)
          (let ((fields (alist-get title items nil nil #'equal)))
            (unless fields
              (auth-source-extras--op-warn "No item `%s' in the %s %s vault"
                                           title account auth-source-extras-op-vault))
            (puthash (cons account title) (or fields 'missing)
                     auth-source-extras--op-cache))))
    (error
     (auth-source-extras--op-warn "Cannot read the %s %s vault: %s" account
                                  auth-source-extras-op-vault (error-message-string err))
     (dolist (title titles)
       (puthash (cons account title) 'missing auth-source-extras--op-cache)))))

(defun auth-source-extras--op-token (account)
  "Return the service-account token of ACCOUNT from the Keychain.
Signal an error when ACCOUNT is unknown or has no stored token."
  (let ((token (or (gethash account auth-source-extras--op-tokens)
                   (puthash account (auth-source-extras--op-keychain-token account)
                            auth-source-extras--op-tokens))))
    (if (eq token 'missing)
        (error "No service-account token for `%s' in the Keychain" account)
      token)))

(defun auth-source-extras--op-keychain-token (account)
  "Read ACCOUNT's token from the login Keychain, or return `missing'."
  (let ((service (alist-get account auth-source-extras-op-accounts)))
    (or (and service
             (with-temp-buffer
               (when (zerop (call-process "/usr/bin/security" nil '(t nil) nil
                                          "find-generic-password" "-a" (user-login-name)
                                          "-s" service "-w"))
                 (string-trim (buffer-string)))))
        'missing)))

(defun auth-source-extras--op-items (token titles)
  "Return an alist of item titles to field alists for TITLES, using TOKEN.
Each item is fetched by its own CLI process, all running in parallel, because
the CLI fetches the items of a single invocation one after another.  Titles
that cannot be fetched are omitted."
  (let* ((procs (mapcar (lambda (title) (auth-source-extras--op-start token title))
                        titles))
         (deadline (+ (float-time) auth-source-extras-op-timeout)))
    (while (and (seq-some #'process-live-p procs) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (delq nil (mapcar #'auth-source-extras--op-collect procs))))

(defun auth-source-extras--op-start (token title)
  "Start a CLI process that prints item TITLE as JSON, using TOKEN."
  (let* ((process-environment (cons (concat "OP_SERVICE_ACCOUNT_TOKEN=" token)
                                    process-environment))
         (err (generate-new-buffer " *auth-source-extras-op-err*" t))
         (proc (make-process :name "auth-source-extras-op"
                             :buffer (generate-new-buffer " *auth-source-extras-op*" t)
                             :stderr err
                             :command (list auth-source-extras-op-program "item" "get" title
                                            "--vault" auth-source-extras-op-vault
                                            "--format" "json")
                             :connection-type 'pipe
                             :noquery t
                             :sentinel #'ignore)))
    (process-put proc 'stderr-buffer err)
    proc))

(defun auth-source-extras--op-collect (proc)
  "Return (TITLE . FIELDS) from finished PROC, or nil, and kill its buffers."
  (let ((out (process-buffer proc))
        (err (process-get proc 'stderr-buffer)))
    (unwind-protect
        (when (and (eq (process-status proc) 'exit) (zerop (process-exit-status proc)))
          (accept-process-output proc 0 nil t)
          (auth-source-extras--op-item-fields
           (with-current-buffer out
             (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer out)
      (when-let* ((stderr-proc (get-buffer-process err)))
        (delete-process stderr-proc))
      (kill-buffer err))))

(defun auth-source-extras--op-item-fields (item)
  "Return (TITLE . FIELDS) for parsed ITEM, keying values by label and ID."
  (cons (alist-get 'title item)
        (mapcan (lambda (field)
                  (let ((value (or (alist-get 'value field) "")))
                    (delq nil (list (when-let* ((label (alist-get 'label field)))
                                      (cons label value))
                                    (when-let* ((id (alist-get 'id field)))
                                      (cons id value))))))
                (alist-get 'fields item))))

(defun auth-source-extras--op-warn (format-string &rest args)
  "Display a warning built from FORMAT-STRING and ARGS."
  (display-warning 'auth-source-extras (apply #'format format-string args)))

;;;;; Commands

;;;###autoload
(defun auth-source-extras-op-clear-cache ()
  "Forget all cached 1Password secrets and tokens."
  (interactive)
  (clrhash auth-source-extras--op-cache)
  (clrhash auth-source-extras--op-tokens)
  (message "Cleared cached 1Password secrets"))

(provide 'auth-source-extras)
;;; auth-source-extras.el ends here
