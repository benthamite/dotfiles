;;; auth-source-extras.el --- Extensions for auth-source -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/auth-source-extras.el
;; Version: 0.2
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
;; Read secrets from 1Password "Automation" vaults through read-only service
;; accounts, so background reads never prompt.  Items are fetched in parallel,
;; one batch per account, and kept in memory for the rest of the session.  An
;; `auth-source' backend answers generic host/user lookups from the same
;; vaults.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; User options

(defgroup auth-source-extras ()
  "Extensions for `auth-source'."
  :group 'auth-source)

(defcustom auth-source-extras-op-program "op-automations"
  "Service-account wrapper for the 1Password CLI.
It is called as PROGRAM @ACCOUNT OP-ARGS..., and must read ACCOUNT's read-only
service-account token itself, so that no read can ever prompt."
  :type 'string
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-vault "Automation"
  "Name of the vault the service accounts can read.
Accounts listed in `auth-source-extras-op-account-vaults' use the name given
there instead."
  :type 'string
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-account-vaults '((epoch . "Automations"))
  "Alist of accounts and the names of their automation vaults.
Accounts not listed use `auth-source-extras-op-vault'."
  :type '(alist :key-type symbol :value-type string)
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-prefetch nil
  "Alist of accounts and the item titles to fetch together on first use.
When a secret from an account is first requested, every listed item of that
account is fetched in the same batch, so a session needs one round of requests
per account instead of one per item."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-timeout 30
  "Seconds to wait for a batch of item fetches before giving up."
  :type 'number
  :group 'auth-source-extras)

(defcustom auth-source-extras-op-search-accounts '(personal tlon)
  "Accounts the `auth-source' backend searches, in order."
  :type '(repeat symbol)
  :group 'auth-source-extras)

;;;; Variables

(defvar auth-source-extras--op-cache (make-hash-table :test #'equal)
  "Fields of fetched items, keyed by (ACCOUNT . TITLE).
Each value is an alist of field labels and IDs to values, or the symbol
`missing' when the item could not be fetched.")

(defvar auth-source-extras--op-titles (make-hash-table :test #'eq)
  "Item titles of each account's automation vault, or `missing'.")

;;;; Functions

;;;;; Reading items

(defun auth-source-extras-op-get (item field &optional account)
  "Return FIELD of ITEM in ACCOUNT's automation vault, or nil.
ITEM is the item title and FIELD a field label or ID, such as \"credential\"
or \"password\".  ACCOUNT is an account symbol understood by
`auth-source-extras-op-program', such as `personal' or `tlon', and defaults to
`personal'.  An item that cannot be read is reported once as a warning and then
returns nil."
  (let ((fields (auth-source-extras--op-item (or account 'personal) item)))
    (when (listp fields)
      (cdr (assoc field fields)))))

(defun auth-source-extras--op-item (account item)
  "Return the field alist of ITEM in ACCOUNT's vault, or `missing'."
  (unless (gethash (cons account item) auth-source-extras--op-cache)
    (auth-source-extras--op-fetch
     account (auth-source-extras--op-titles-to-fetch account item)))
  (gethash (cons account item) auth-source-extras--op-cache))

(defun auth-source-extras--op-titles-to-fetch (account item)
  "Return ITEM plus the uncached prefetch titles of ACCOUNT."
  (seq-uniq
   (cons item
         (seq-remove (lambda (title)
                       (gethash (cons account title) auth-source-extras--op-cache))
                     (alist-get account auth-source-extras-op-prefetch)))))

(defun auth-source-extras--op-fetch (account titles)
  "Fetch TITLES from ACCOUNT's automation vault into the cache.
Titles that cannot be read are cached as `missing' and reported in one warning
per distinct reason."
  (let (failures)
    (dolist (result (auth-source-extras--op-items account titles))
      (let ((title (car result))
            (fields (cdr result)))
        (if (eq (car-safe fields) :error)
            (push title (alist-get (cdr fields) failures nil nil #'equal))
          (puthash (cons account title) fields auth-source-extras--op-cache))))
    (dolist (failure failures)
      (dolist (title (cdr failure))
        (puthash (cons account title) 'missing auth-source-extras--op-cache))
      (auth-source-extras--op-warn "Cannot read %s from the %s %s vault: %s"
                                   (string-join (reverse (cdr failure)) ", ")
                                   account (auth-source-extras--op-vault account) (car failure)))))

(defun auth-source-extras--op-items (account titles)
  "Return an alist of TITLES to their fields in ACCOUNT's automation vault.
Each value is a field alist, or (:error . REASON) when the item could not be
read.  Each item is fetched by its own CLI process, all running in parallel,
because the CLI fetches the items of a single invocation one after another."
  (let* ((procs (mapcar (lambda (title) (auth-source-extras--op-start account title))
                        titles))
         (deadline (+ (float-time) auth-source-extras-op-timeout)))
    (while (and (seq-some #'process-live-p procs) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (mapcar #'auth-source-extras--op-collect procs)))

(defun auth-source-extras--op-start (account title)
  "Start a CLI process that prints item TITLE of ACCOUNT as JSON."
  (let* ((err (generate-new-buffer " *auth-source-extras-op-err*" t))
         ;; An explicit pipe with a silent sentinel, so that the default
         ;; sentinel's "Process ... finished" line does not land in ERR and
         ;; masquerade as the CLI's error message.
         (err-pipe (make-pipe-process :name "auth-source-extras-op-err" :buffer err
                                      :noquery t :sentinel #'ignore))
         (proc (make-process :name "auth-source-extras-op"
                             :buffer (generate-new-buffer " *auth-source-extras-op*" t)
                             :stderr err-pipe
                             :command (list auth-source-extras-op-program
                                            (format "@%s" account) "item" "get" title
                                            "--vault" (auth-source-extras--op-vault account)
                                            "--format" "json")
                             :connection-type 'pipe
                             :noquery t
                             :sentinel #'ignore)))
    (process-put proc 'stderr-buffer err)
    (process-put proc 'title title)
    proc))

(defun auth-source-extras--op-vault (account)
  "Return the name of ACCOUNT's automation vault."
  (alist-get account auth-source-extras-op-account-vaults auth-source-extras-op-vault))

(defun auth-source-extras--op-collect (proc)
  "Return (TITLE . FIELDS) or (TITLE :error . REASON) for PROC; kill its buffers.
REASON is the last line of the CLI's error output, which names the problem but
never contains item values."
  (let ((out (process-buffer proc))
        (err (process-get proc 'stderr-buffer))
        (title (process-get proc 'title)))
    (unwind-protect
        (cons title
              (cond ((process-live-p proc) '(:error . "timed out"))
                    ((zerop (process-exit-status proc))
                     (accept-process-output proc 0 nil t)
                     (auth-source-extras--op-exact-fields
                      title (with-current-buffer out
                              (json-parse-string (buffer-string)
                                                 :object-type 'alist :array-type 'list))))
                    (t (cons :error (auth-source-extras--op-error-reason proc err)))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer out)
      (when-let* ((stderr-proc (get-buffer-process err)))
        (delete-process stderr-proc))
      (kill-buffer err))))

(defun auth-source-extras--op-exact-fields (title item)
  "Return the fields of parsed ITEM, or an error unless it is titled TITLE.
When no item has the requested title, the CLI may return another item whose
website matches it; accepting that would hand out the wrong secret."
  (let ((parsed (auth-source-extras--op-item-fields item)))
    (if (equal (car parsed) title)
        (cdr parsed)
      (cons :error (format "no item titled exactly `%s'" title)))))

(defun auth-source-extras--op-error-reason (proc err)
  "Return the last line of PROC's error output in buffer ERR, or its status."
  (when-let* ((stderr-proc (get-buffer-process err)))
    (accept-process-output stderr-proc 0 nil t))
  (let ((lines (split-string (with-current-buffer err (buffer-string)) "\n" t "[ \t]+")))
    (if lines
        (replace-regexp-in-string "\\`\\[ERROR\\] [0-9/]+ [0-9:]+ " "" (car (last lines)))
      (format "exit status %s" (process-exit-status proc)))))

(defun auth-source-extras--op-item-fields (item)
  "Return (TITLE . FIELDS) for ITEM, with field IDs preceding display labels.
Template labels can repeat, including empty fields named password.  Stable
IDs must take precedence so these cannot hide the actual password field."
  (let ((fields (alist-get 'fields item)))
    (cons (alist-get 'title item)
          (append
           (delq nil (mapcar (lambda (field)
                              (when-let* ((id (alist-get 'id field)))
                                (cons id (or (alist-get 'value field) ""))))
                            fields))
           (delq nil (mapcar (lambda (field)
                              (when-let* ((label (alist-get 'label field))
                                          (value (alist-get 'value field))
                                          ((not (string-empty-p value))))
                                (cons label value)))
                            fields))))))

(defun auth-source-extras--op-warn (format-string &rest args)
  "Display a warning built from FORMAT-STRING and ARGS."
  (display-warning 'auth-source-extras (apply #'format format-string args)))

;;;;; auth-source backend

(defun auth-source-extras-op-backend-parse (entry)
  "Return the 1Password backend when ENTRY in `auth-sources' names it.
The backend is selected by the symbol `1password-automation'."
  (when (eq entry '1password-automation)
    (auth-source-backend
     :source "1Password Automation vaults"
     :type '1password-automation
     :search-function #'auth-source-extras-op-search)))

(cl-defun auth-source-extras-op-search (&rest spec &key host user port max create delete
                                              &allow-other-keys)
  "Search automation vaults for HOST, USER and PORT, returning MAX results.
Existing credentials are returned even when CREATE is non-nil, but new
entries and DELETE are unsupported.  The rest of SPEC is ignored."
  (ignore spec create)
  (unless delete
    (let (results seen)
      (catch 'done
        (dolist (account auth-source-extras-op-search-accounts)
          (dolist (cached '(t nil))
            (dolist (candidate (auth-source-extras--op-candidates
                               host user port account cached))
              (unless (member candidate seen)
                (push candidate seen)
                (when-let* ((result (auth-source-extras--op-search-result candidate user)))
                  (push result results)
                  (when (>= (length results) (or max 1))
                    (throw 'done nil))))))))
      (nreverse results))))

(defun auth-source-extras--op-candidates (hosts users ports account cached)
  "Return (ACCOUNT TITLE HOST PORT) candidates for HOSTS, USERS and PORTS.
With CACHED non-nil, use only known items, without invoking the CLI."
  (let ((titles (if cached
                    (auth-source-extras--op-cached-titles account)
                  (auth-source-extras--op-account-titles account)))
        candidates)
    (dolist (host (auth-source-extras--op-strings hosts))
      (dolist (port (or (auth-source-extras--op-strings ports) '(nil)))
        (dolist (title (auth-source-extras--op-title-patterns
                       host (auth-source-extras--op-strings users) port))
          (when (member title titles)
            (push (list account title host port) candidates)))))
    (seq-uniq (nreverse candidates))))

(defun auth-source-extras--op-cached-titles (account)
  "Return known titles in ACCOUNT, preserving available listing precedence."
  (let ((titles (let ((listed (gethash account auth-source-extras--op-titles)))
                  (and (listp listed) (copy-sequence listed)))))
    (maphash (lambda (key fields)
               (when (and (eq (car key) account) (consp fields))
                 (push (cdr key) titles)))
             auth-source-extras--op-cache)
    titles))

(defun auth-source-extras--op-strings (value)
  "Return VALUE, a string, number, symbol or list of them, as a list of strings."
  (mapcar (lambda (v) (format "%s" v))
          (delq nil (if (listp value) value (list value)))))

(defun auth-source-extras--op-title-patterns (host users port)
  "Return the item titles that can hold a secret for HOST, USERS and PORT."
  (let ((host-port (and port (format "%s:%s" host port))))
    (append (mapcan (lambda (user)
                      (delq nil (list (and host-port (format "%s/%s" host-port user))
                                      (format "%s/%s" host user)
                                      (format "%s@%s" user host))))
                    users)
            (delq nil (list host-port host)))))

(defun auth-source-extras--op-account-titles (account)
  "Return ACCOUNT's cached vault listing, retrying a previously failed lookup."
  (let ((titles (gethash account auth-source-extras--op-titles 'unlisted)))
    (when (memq titles '(unlisted missing))
      (remhash account auth-source-extras--op-titles)
      (setq titles (auth-source-extras--op-list-titles account))
      (unless (eq titles 'missing)
        (puthash account titles auth-source-extras--op-titles)))
    (unless (eq titles 'missing) titles)))

(defun auth-source-extras--op-list-titles (account)
  "List ACCOUNT's automation vault titles, or return `missing' with a warning."
  (with-temp-buffer
    (if (zerop (call-process auth-source-extras-op-program nil '(t nil) nil
                             (format "@%s" account) "item" "list"
                             "--vault" (auth-source-extras--op-vault account) "--format" "json"))
        (mapcar (lambda (entry) (alist-get 'title entry))
                (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))
      (auth-source-extras--op-warn "Cannot list the %s %s vault" account
                                   (auth-source-extras--op-vault account))
      'missing)))

(defun auth-source-extras--op-search-result (candidate users)
  "Return an `auth-source' result for CANDIDATE, or nil if USERS rule it out.
CANDIDATE is (ACCOUNT TITLE HOST PORT)."
  (pcase-let* ((`(,account ,title ,host ,port) candidate)
               (fields (auth-source-extras--op-item account title))
               (users (auth-source-extras--op-strings users))
               (stored-user (and (listp fields)
                                 (or (cdr (assoc "username" fields))
                                     (cdr (assoc "user" fields)))))
               (user (or (and (not (string-empty-p (or stored-user ""))) stored-user)
                         (car users))))
    ;; A title naming the user already matched it; a bare HOST or HOST:PORT
    ;; title matches only if its stored user, when it has one, is wanted.
    (when (and (listp fields)
               (or (null users) (null stored-user) (string-empty-p stored-user)
                   (member stored-user users)
                   (not (member title (list host (format "%s:%s" host port))))))
      (let ((secret (cdr (assoc "password" fields))))
        (when (and user (stringp secret) (not (string-empty-p secret)))
          (list :host host :port port :user user
                :secret (lambda () secret)))))))

;;;;; git-crypt keys

;;;###autoload
(defun auth-source-extras-git-crypt-unlock (&optional repo key account)
  "Unlock the `git-crypt' repository REPO with KEY from ACCOUNT's automation vault.
KEY is the title of a Document item holding the binary key; ACCOUNT defaults
to `tlon'.  REPO defaults to `default-directory'.  The key passes through a
private temporary file that is deleted whatever the outcome."
  (interactive)
  (let* ((account (or account 'tlon))
         (default-directory (or repo default-directory))
         (key (or key (completing-read "git-crypt key: "
                                       (auth-source-extras--op-document-titles account)
                                       nil t)))
         (file (with-file-modes #o600 (make-temp-file "git-crypt-key"))))
    (unwind-protect
        (progn
          (auth-source-extras--op-save-document account key file)
          (unless (zerop (call-process "git-crypt" nil nil nil "unlock" file))
            (user-error "Could not unlock `%s' with `%s'; perhaps the repository is dirty"
                        default-directory key))
          (message "Unlocked `%s' with `%s'" default-directory key))
      (delete-file file))))

(defun auth-source-extras--op-document-titles (account)
  "Return the titles of the Document items in ACCOUNT's automation vault."
  (with-temp-buffer
    (unless (zerop (call-process auth-source-extras-op-program nil '(t nil) nil
                                 (format "@%s" account) "item" "list" "--vault"
                                 (auth-source-extras--op-vault account) "--categories" "Document"
                                 "--format" "json"))
      (user-error "Cannot list the documents in the %s %s vault"
                  account (auth-source-extras--op-vault account)))
    (mapcar (lambda (entry) (alist-get 'title entry))
            (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))))

(defun auth-source-extras--op-save-document (account title file)
  "Write the Document TITLE of ACCOUNT's automation vault to FILE, byte for byte."
  (unless (zerop (call-process auth-source-extras-op-program nil nil nil
                               (format "@%s" account) "document" "get" title "--vault"
                               (auth-source-extras--op-vault account) "--out-file" file "--force"))
    (user-error "Cannot read the document `%s' from the %s %s vault"
                title account (auth-source-extras--op-vault account))))

;;;;; Commands

;;;###autoload
(defun auth-source-extras-op-clear-cache ()
  "Forget all cached 1Password secrets and vault listings."
  (interactive)
  (clrhash auth-source-extras--op-cache)
  (clrhash auth-source-extras--op-titles)
  (auth-source-forget-all-cached)
  (message "Cleared cached 1Password secrets"))

(add-hook 'auth-source-backend-parser-functions #'auth-source-extras-op-backend-parse)

(provide 'auth-source-extras)
;;; auth-source-extras.el ends here
