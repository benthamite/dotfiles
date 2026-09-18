;;; elpaca-cooldown-lockfile.el --- Lockfile reader and writer for elpaca-cooldown -*- lexical-binding: t -*-

;;; Commentary:

;; Batch helper of `bin/elpaca-cooldown'.  It only reads and prints Lisp data:
;; no recipe, package or configuration code is evaluated.
;;
;;   emacs --batch -Q -l elpaca-cooldown-lockfile.el -f elpaca-cooldown-export LOCKFILE
;;   emacs --batch -Q -l elpaca-cooldown-lockfile.el -f elpaca-cooldown-rewrite LOCKFILE REFS OUT
;;
;; `export' prints the lockfile's recipes as JSON.  `rewrite' copies LOCKFILE to
;; OUT with the `:ref' of every package named in the JSON object REFS replaced;
;; a package mapped to null is left out of OUT.

;;; Code:

(require 'json)
(require 'seq)

(defun elpaca-cooldown-export ()
  "Print the recipes of the lockfile named on the command line as JSON."
  (let ((entries (elpaca-cooldown--read (pop command-line-args-left))))
    (princ (json-encode (vconcat (mapcar #'elpaca-cooldown--entry-json entries))))
    (terpri)))

(defun elpaca-cooldown--read (file)
  "Return the entry list stored in the lockfile FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((entries (read (current-buffer))))
      (skip-chars-forward " \t\n")
      (unless (eobp)
        (error "%s: trailing content after the entry list" file))
      entries)))

(defun elpaca-cooldown--entry-json (entry)
  "Return the JSON object describing the lockfile ENTRY.
The host, repo, url and branch are those of the remote Elpaca checks out: the
first of `:remotes' when it carries its own recipe, and the recipe otherwise."
  (let* ((recipe (plist-get (cdr entry) :recipe))
         (remote (car-safe (plist-get recipe :remotes)))
         (tracked (if (consp remote) (cdr remote) recipe))
         (repo (plist-get tracked :repo)))
    `((id . ,(symbol-name (car entry)))
      (host . ,(elpaca-cooldown--string (or (plist-get tracked :host)
                                            (plist-get tracked :fetcher))))
      (repo . ,(elpaca-cooldown--string (if (consp repo) (car repo) repo)))
      (url . ,(elpaca-cooldown--string (plist-get tracked :url)))
      (branch . ,(elpaca-cooldown--string (plist-get tracked :branch)))
      (tag . ,(elpaca-cooldown--string (plist-get recipe :tag)))
      (ref . ,(elpaca-cooldown--string (plist-get recipe :ref))))))

(defun elpaca-cooldown--string (value)
  "Return VALUE as a string, or nil when VALUE is nil."
  (and value (format "%s" value)))

(defun elpaca-cooldown-rewrite ()
  "Write the lockfile named on the command line with the refs of a JSON file.
The command line holds the source lockfile, the JSON object mapping package
names to refs, and the destination."
  (let* ((entries (elpaca-cooldown--read (pop command-line-args-left)))
         (refs (json-read-file (pop command-line-args-left)))
         (out (pop command-line-args-left))
         (known (mapcar #'car entries)))
    (dolist (pair refs)
      (unless (memq (car pair) known)
        (error "%s is not in the lockfile" (car pair))))
    (dolist (entry entries)
      (when-let* ((ref (cdr (assq (car entry) refs))))
        (plist-put (plist-get (cdr entry) :recipe) :ref ref)))
    (elpaca-cooldown--write (seq-remove (lambda (entry)
                                          (elpaca-cooldown--dropped-p entry refs))
                                        entries)
                            out)))

(defun elpaca-cooldown--dropped-p (entry refs)
  "Return non-nil when REFS maps the package of ENTRY to a JSON null."
  (let ((pair (assq (car entry) refs)))
    (and pair (null (cdr pair)))))

(defun elpaca-cooldown--write (entries file)
  "Write the lockfile ENTRIES to FILE, printed as Elpaca prints them."
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil)
        (coding-system-for-write 'utf-8))
    (with-temp-file file
      (pp entries (current-buffer)))))

(provide 'elpaca-cooldown-lockfile)
;;; elpaca-cooldown-lockfile.el ends here
