;;; elpaca-cooldown-lockfile.el --- Lockfile reader and writer for elpaca-cooldown -*- lexical-binding: t -*-

;;; Commentary:

;; Batch helper of `bin/elpaca-cooldown'.  It only reads and prints Lisp data:
;; no recipe, package or configuration code is evaluated.
;;
;;   emacs --batch -Q -l elpaca-cooldown-lockfile.el -f elpaca-cooldown-export LOCKFILE
;;   emacs --batch -Q -l elpaca-cooldown-lockfile.el -f elpaca-cooldown-rewrite LOCKFILE REFS OUT
;;
;;   emacs --batch -Q -l elpaca-cooldown-lockfile.el -f elpaca-cooldown-config-pins CONFIG
;;
;; `export' prints the lockfile's recipes as JSON.  `rewrite' copies LOCKFILE to
;; OUT with the `:ref' of every package named in the JSON object REFS replaced;
;; a package mapped to null is left out of OUT.  `config-pins' prints, as a
;; JSON object, the `:ref' or `:tag' that the Org configuration CONFIG pins in a
;; package's recipe; Elpaca honours such a pin over the lockfile.

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

(defun elpaca-cooldown-config-pins ()
  "Print the recipe pins of the Org configuration named on the command line.
The output maps each package whose recipe carries `:ref' or `:tag' to an object
with those two keys."
  (let ((pins nil))
    (dolist (form (elpaca-cooldown--config-forms (pop command-line-args-left)))
      (elpaca-cooldown--collect-pins form (lambda (name pin)
                                            (setq pins (elpaca-cooldown--add-pin name pin pins)))))
    (princ (if pins (json-encode (nreverse pins)) "{}"))
    (terpri)))

(defun elpaca-cooldown--config-forms (file)
  "Return the forms read from the Emacs Lisp source blocks of the Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (forms nil))
      (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+emacs-lisp\\b.*\n" nil t)
        (let ((start (point)))
          (unless (re-search-forward "^[ \t]*#\\+end_src" nil t)
            (error "%s: unterminated source block at line %d" file (line-number-at-pos start)))
          (setq forms (nconc forms (elpaca-cooldown--block-forms
                                    (buffer-substring-no-properties start (match-beginning 0))
                                    file (line-number-at-pos start))))))
      forms)))

(defun elpaca-cooldown--block-forms (text file line)
  "Return the forms read from the source block TEXT at LINE of FILE."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file
         (skip-chars-forward " \t\n")
         (unless (or (eobp) (looking-at-p ";"))
           (error "%s: unreadable source block at line %d" file line))))
      (nreverse forms))))

(defun elpaca-cooldown--collect-pins (form function)
  "Call FUNCTION with the name and pin of every pinned recipe in FORM.
Recipes are the `:ensure' argument of `use-package' and the order of `elpaca'."
  (when (consp form)
    (pcase form
      (`(use-package ,(and name (pred symbolp)) . ,rest)
       (when-let* ((recipe (plist-get (elpaca-cooldown--keyword-tail rest) :ensure)))
         (elpaca-cooldown--recipe-pin name recipe function)))
      (`(elpaca (,(and name (pred symbolp)) . ,recipe) . ,_)
       (elpaca-cooldown--recipe-pin name recipe function)))
    (while (consp form)
      (elpaca-cooldown--collect-pins (car form) function)
      (setq form (cdr form)))))

(defun elpaca-cooldown--keyword-tail (arguments)
  "Return ARGUMENTS from their first keyword on, as a property list."
  (let ((tail arguments))
    (while (and tail (not (keywordp (car tail))))
      (setq tail (cdr tail)))
    (let ((plist nil))
      (while tail
        (if (keywordp (car tail))
            (setq plist (nconc plist (list (car tail) (cadr tail)))
                  tail (cddr tail))
          (setq tail (cdr tail))))
      plist)))

(defun elpaca-cooldown--recipe-pin (name recipe function)
  "Call FUNCTION with NAME and the pin of RECIPE when RECIPE pins a revision."
  (when (and (consp recipe) (keywordp (car recipe)) (proper-list-p recipe))
    (let ((ref (plist-get recipe :ref))
          (tag (plist-get recipe :tag)))
      (when (or ref tag)
        (funcall function name `((ref . ,(elpaca-cooldown--string ref))
                                 (tag . ,(elpaca-cooldown--string tag))))))))

(defun elpaca-cooldown--add-pin (name pin pins)
  "Return the alist PINS with PIN added for NAME, rejecting conflicting pins."
  (let ((existing (assq name pins)))
    (cond ((null existing) (cons (cons name pin) pins))
          ((equal (cdr existing) pin) pins)
          (t (error "%s is pinned to two different revisions" name)))))

(provide 'elpaca-cooldown-lockfile)
;;; elpaca-cooldown-lockfile.el ends here
