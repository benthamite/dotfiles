;;; elpaca-extras.el --- Extensions for elpaca -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/elpaca-extras.el
;; Version: 0.2
;; Package-Requires: ((elpaca "0.0.1"))

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

;; Extensions for `elpaca'.

;;; Code:

(require 'elpaca)
(require 'eieio)
(require 'lisp-mode)
(require 'loadhist)

;;;; Variables

(defgroup elpaca-extras ()
  "Extensions for `elpaca'."
  :group 'elpaca)

(defcustom elpaca-extras-write-lock-file-excluded nil
  "List of package identifiers that must never be written to a lock file."
  :type '(repeat symbol))

(defvar elpaca-extras--build-reload-statuses (make-hash-table :test #'equal)
  "Status table for asynchronous build-and-reload requests.")

(define-error 'elpaca-extras-restart-required
  "A package class layout changed; restart Emacs before loading it"
  'user-error)

(defun elpaca-extras--without-print-limits (function &rest args)
  "Call FUNCTION with printer limits disabled.
Elpaca serializes subprocess forms while asynchronous build steps run.  User
values of `print-length' or `print-level' must not truncate those forms."
  (let ((print-length nil)
        (print-level nil))
    (apply function args)))

;; `elpaca-rebuild' only enqueues work.  These functions serialize the actual
;; subprocess forms later, after the enqueue-time dynamic bindings have ended.
(dolist (function '(elpaca-build-autoloads elpaca-build-compile))
  (unless (advice-member-p #'elpaca-extras--without-print-limits function)
    (advice-add function :around #'elpaca-extras--without-print-limits)))

;;;; Functions

(defun elpaca-extras--source-feature-info (file &optional artifact)
  "Return FILE's features, requirements, and EIEIO class layouts.
ARTIFACT is the exact file that a subsequent `load' will evaluate."
  (let (classes features requirements)
    (dolist (source-form (elpaca-extras--source-forms file))
      (dolist (form (elpaca-extras--source-feature-forms source-form))
        (pcase (car form)
          ('provide
           (when-let* ((name (elpaca-extras--quoted-symbol (cadr form))))
             (cl-pushnew name features)))
          ('require
           (when-let* ((name (elpaca-extras--quoted-symbol (cadr form))))
             (cl-pushnew name requirements)))))
      (dolist (form
               (elpaca-extras--source-class-forms
                source-form
                (or (null artifact)
                    (string-match-p (rx ".el" eos) artifact))))
        (push (elpaca-extras--source-class-layout form) classes)))
    (list :source file
          :artifact (or artifact file)
          :features (nreverse features)
          :requires (nreverse requirements)
          :classes (nreverse classes))))

(defun elpaca-extras--source-forms (file)
  "Read and return all top-level Lisp forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let (forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (push (read (current-buffer)) forms))
      (nreverse forms))))

(defun elpaca-extras--source-feature-forms (form)
  "Return definite load-time feature forms nested in FORM."
  (cond
   ((not (consp form)) nil)
   ((memq (car form) '(provide require)) (list form))
   ((memq (car form) '(eval-and-compile progn))
    (mapcan #'elpaca-extras--source-feature-forms (cdr form)))
   (t nil)))

(defun elpaca-extras--source-class-forms (form &optional sourcep)
  "Return definite load-time EIEIO class forms nested in FORM.
SOURCEP means the selected load artifact is interpreted source, so forms in
`eval-when-compile' will execute."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'defclass) (list form))
   ((eq (car form) 'eval-when-compile)
    (when sourcep
      (mapcan (lambda (nested)
                (elpaca-extras--source-class-forms nested sourcep))
              (cdr form))))
   ((or (memq (car form) '(function quote))
        (eq (car form) (intern "`")))
    nil)
   ((memq (car form) '(eval-and-compile progn))
    (mapcan (lambda (nested)
              (elpaca-extras--source-class-forms nested sourcep))
            (cdr form)))
   ((elpaca-extras--contains-source-class-p form)
    (error "Cannot inspect defclass wrapped by %s" (car form)))
   (t nil)))

(defun elpaca-extras--contains-source-class-p (form)
  "Return non-nil when executable FORM contains a class declaration."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'defclass) t)
   ((or (memq (car form)
              '(cl-defmacro cl-defmethod cl-defsubst cl-defun defmacro
                 defsubst defun eval-when-compile function lambda quote))
        (eq (car form) (intern "`")))
    nil)
   (t (cl-some #'elpaca-extras--contains-source-class-p (cdr form)))))

(defun elpaca-extras--source-class-layout (form)
  "Return the EIEIO class layout declared by defclass FORM."
  (let ((name (cadr form))
        (parents (caddr form))
        (slots (cadddr form)))
    (unless (and (symbolp name)
                 (proper-list-p parents)
                 (cl-every #'symbolp parents)
                 (proper-list-p slots))
      (error "Cannot determine class layout from %S" form))
    (list :name name
          :parents parents
          :slots (mapcar #'elpaca-extras--source-slot-layout slots))))

(defun elpaca-extras--source-slot-layout (slot)
  "Return SLOT's name and normalized allocation."
  (let* ((name (if (consp slot) (car slot) slot))
         (options (and (consp slot) (cdr slot)))
         (allocation (plist-get options :allocation)))
    (unless (and (symbolp name)
                 (proper-list-p options)
                 (memq allocation '(nil :class :instance)))
      (error "Cannot determine slot layout from %S" slot))
    (cons name (if (eq allocation :class) :class :instance))))

(defun elpaca-extras--quoted-symbol (value)
  "Return VALUE's quoted symbol, or nil when VALUE is not one."
  (and (eq (car-safe value) 'quote) (symbolp (cadr value)) (cadr value)))

(defun elpaca-extras--assert-reloadable-class-layouts (package file-info)
  "Reject unsafe EIEIO layouts before reloading PACKAGE's FILE-INFO."
  (let ((layouts (elpaca-extras--class-layouts file-info)))
    ;; Literal declarations can be checked before any package code changes.
    ;; A class previously created by one of these exact files but absent from
    ;; the literal declarations came from a macro or computed form.  Refuse it
    ;; here rather than mistaking an unknown layout for a safe one.
    (dolist (class-name
             (elpaca-extras--loaded-class-names-for-files file-info))
      (unless (assq class-name layouts)
        (signal
         'elpaca-extras-restart-required
         (list
          (format
           "Cannot reload %s: class %s is not statically inspectable; restart Emacs"
           package class-name)))))
    (dolist (layout (mapcar #'cdr layouts))
      (elpaca-extras--assert-class-layout package layout layouts))))

(defun elpaca-extras--assert-class-layout (package layout layouts)
  "Reject LAYOUT when it would invalidate a live class from PACKAGE.
LAYOUTS indexes other prospective class definitions that will load with it."
  (let* ((class-name (plist-get layout :name))
         (current-class (find-class class-name nil))
         (source-parents (plist-get layout :parents)))
    (when (and current-class
               (or
                (not
                 (equal source-parents
                        (elpaca-extras--class-parent-names current-class)))
                (not
                 (equal
                  (plist-get
                   (elpaca-extras--source-effective-layout layout layouts)
                   :instance)
                  (plist-get
                   (elpaca-extras--class-effective-layout current-class)
                   :instance)))))
      (signal
       'elpaca-extras-restart-required
       (list
        (format "Cannot reload %s: class %s changed layout; restart Emacs"
                package class-name))))))

(defun elpaca-extras--class-layouts (file-info)
  "Index selected class layouts in FILE-INFO by class name."
  (let (layouts)
    (dolist (info file-info)
      (dolist (layout (plist-get info :classes))
        (let ((existing (assq (plist-get layout :name) layouts)))
          (if existing
              (unless (equal (cdr existing) layout)
                (signal
                 'elpaca-extras-restart-required
                 (list
                  (format "Class %s has divergent source declarations"
                          (plist-get layout :name)))))
            (push (cons (plist-get layout :name) layout) layouts)))))
    (nreverse layouts)))

(defun elpaca-extras--loaded-class-names-for-files (file-info)
  "Return live EIEIO classes previously defined by FILE-INFO's files."
  (let ((history-entries (elpaca-extras--history-entries-for-files file-info))
        result)
    (dolist (entry history-entries)
      (dolist (item (cdr entry))
        (when (and (eq (car-safe item) 'define-type)
                   (find-class (cdr item) nil))
          (cl-pushnew (cdr item) result))))
    (nreverse result)))

(defun elpaca-extras--history-entries-for-files (file-info)
  "Return the `load-history' entries recorded for FILE-INFO's files.
Scan `load-history' once and match on an extension-less absolute name,
falling back to a file-system comparison only for an entry whose base
name matches one of FILE-INFO's files.  A per-file scan that compared
every entry with `elpaca-extras--same-library-file-p' cost three
file-system calls per comparison, which dominated reload time: a
long-running session holds well over a thousand entries, and the work
grew with both that count and the package's file count."
  (let ((by-key (make-hash-table :test #'equal))
        (by-base (make-hash-table :test #'equal))
        history-entries)
    (dolist (info file-info)
      (dolist (file (list (plist-get info :source) (plist-get info :artifact)))
        (when-let* ((key (elpaca-extras--library-key file)))
          (puthash key t by-key)
          (push file (gethash (file-name-nondirectory key) by-base))))
      (dolist (feature (plist-get info :features))
        (when (featurep feature)
          (when-let* ((loaded-file (feature-file feature))
                      (entry (assoc loaded-file load-history)))
            (cl-pushnew entry history-entries :test #'eq)))))
    (dolist (entry load-history)
      (when (elpaca-extras--history-entry-matches-p (car entry) by-key by-base)
        (cl-pushnew entry history-entries :test #'eq)))
    history-entries))

(defun elpaca-extras--history-entry-matches-p (file by-key by-base)
  "Return non-nil when FILE names a library indexed by BY-KEY or BY-BASE.
BY-KEY maps an extension-less absolute name to t.  BY-BASE maps an
extension-less base name to the files carrying it, which are compared
with `elpaca-extras--same-library-file-p' so a symbolic link under
another name still matches."
  (when-let* ((key (elpaca-extras--library-key file)))
    (or (gethash key by-key)
        (cl-some (lambda (candidate)
                   (elpaca-extras--same-library-file-p file candidate))
                 (gethash (file-name-nondirectory key) by-base)))))

(defun elpaca-extras--library-key (file)
  "Return the extension-less absolute name of FILE, or nil when not a name."
  (and (stringp file)
       (file-name-sans-extension (expand-file-name file))))

(defun elpaca-extras--same-library-file-p (left right)
  "Return non-nil when LEFT and RIGHT name the same Lisp library artifact."
  (and (stringp left)
       (stringp right)
       (or
        (and (file-exists-p left)
             (file-exists-p right)
             (file-equal-p left right))
        (equal
         (file-name-sans-extension (expand-file-name left))
         (file-name-sans-extension (expand-file-name right))))))

(defun elpaca-extras--guarded-eieio-defclass
    (package original &rest arguments)
  "Call ORIGINAL class definition with a live-layout guard for PACKAGE.
ARGUMENTS are those passed to `eieio-defclass-internal'."
  (let* ((class-name (nth 0 arguments))
         (current-class (find-class class-name nil)))
    (when current-class
      (let ((layout
             (list :name class-name
                   :parents (nth 1 arguments)
                   :slots (mapcar #'elpaca-extras--source-slot-layout
                                  (nth 2 arguments)))))
        (elpaca-extras--assert-class-layout
         package layout (list (cons class-name layout)))))
    (apply original arguments)))

(defun elpaca-extras--call-with-class-layout-guard (package function)
  "Call FUNCTION while guarding every runtime class definition in PACKAGE."
  (let ((original (symbol-function 'eieio-defclass-internal)))
    (cl-letf (((symbol-function 'eieio-defclass-internal)
               (lambda (&rest arguments)
                 (apply #'elpaca-extras--guarded-eieio-defclass
                        package original arguments))))
      (funcall function))))

(defun elpaca-extras--class-parent-names (class)
  "Return CLASS's direct parent names in precedence order."
  (mapcar #'eieio-class-name (eieio-class-parents class)))

(defun elpaca-extras--class-effective-layout (class)
  "Return CLASS's effective instance and class slot names."
  (list
   :instance
   (mapcar #'eieio-slot-descriptor-name (eieio-class-slots class))
   :class
   (mapcar #'eieio-slot-descriptor-name
           (eieio--class-class-slots class))))

(defun elpaca-extras--source-effective-layout
    (layout layouts &optional visiting)
  "Return the prospective effective slot LAYOUT.
LAYOUTS indexes selected source layouts.  VISITING detects cyclic source
inheritance."
  (let ((class-name (plist-get layout :name))
        (effective (list :instance nil :class nil)))
    (when (memq class-name visiting)
      (error "Cyclic source inheritance involving %s" class-name))
    (dolist (parent-name (plist-get layout :parents))
      (let* ((source-parent (cdr (assq parent-name layouts)))
             (parent-layout
              (if source-parent
                  (elpaca-extras--source-effective-layout
                   source-parent layouts (cons class-name visiting))
                (when-let* ((parent (find-class parent-name nil)))
                  (elpaca-extras--class-effective-layout parent)))))
        (unless parent-layout
          (error "Cannot determine parent class layout for %s" parent-name))
        (dolist (slot (plist-get parent-layout :instance))
          (setq effective
                (elpaca-extras--add-slot-to-layout
                 effective (cons slot :instance))))
        (dolist (slot (plist-get parent-layout :class))
          (setq effective
                (elpaca-extras--add-slot-to-layout
                 effective (cons slot :class))))))
    (dolist (slot (plist-get layout :slots))
      (setq effective
            (elpaca-extras--add-slot-to-layout effective slot)))
    effective))

(defun elpaca-extras--add-slot-to-layout (layout slot)
  "Return LAYOUT after adding SLOT according to EIEIO allocation rules."
  (let ((instance-slots (plist-get layout :instance))
        (class-slots (plist-get layout :class))
        (name (car slot))
        (allocation (cdr slot)))
    (when (memq name class-slots)
      (setq allocation :class))
    (if (eq allocation :class)
        (unless (memq name class-slots)
          (setq class-slots (nconc class-slots (list name))))
      (unless (memq name instance-slots)
        (setq instance-slots (nconc instance-slots (list name)))))
    (list :instance instance-slots :class class-slots)))

(defun elpaca-extras--order-features (package-features requirements)
  "Order PACKAGE-FEATURES after their dependencies in REQUIREMENTS.
REQUIREMENTS maps each feature to the features it directly requires."
  (let ((selected (copy-sequence package-features))
        (visiting (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq))
        ordered)
    (cl-labels
        ((visit (feature)
           (unless (gethash feature visited)
             (unless (gethash feature visiting)
               (puthash feature t visiting)
               (dolist (dependency (gethash feature requirements))
                 (when (memq dependency selected)
                   (visit dependency)))
               (remhash feature visiting)
               (puthash feature t visited)
               (push feature ordered)))))
      (dolist (feature selected)
        (visit feature)))
    (nreverse ordered)))

(defun elpaca-extras--source-artifact (source package-dir)
  "Return the exact load artifact for SOURCE within PACKAGE-DIR."
  (or (locate-file
       (file-name-sans-extension (file-name-nondirectory source))
       (list package-dir) (get-load-suffixes))
      source))

;; github.com/progfolio/elpaca/issues/250
(defun elpaca-extras-reload (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

New definitions overwrite old ones; existing variable values are
preserved unless the new code changes their defaults."
  (interactive
   (list (let ((elpaca-overriding-prompt "Reload package: "))
           (elpaca--read-queued))
         current-prefix-arg))
  (message "Reloading: %s" package)
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (let* ((package-name (symbol-name package))
         (located (locate-file package-name load-path (get-load-suffixes)))
         (package-dir (and located (file-name-directory located)))
         (package-files (and package-dir
                             (directory-files package-dir 'full (rx ".el" eos))))
         (feature-info
          (mapcar
           (lambda (source)
             (let ((artifact
                    (elpaca-extras--source-artifact source package-dir)))
               (elpaca-extras--source-feature-info source artifact)))
           package-files))
         (requirements (make-hash-table :test #'eq))
         (package-features
          (cl-loop for info in feature-info
                   append
                   (cl-loop for feature in (plist-get info :features)
                            do (puthash feature (plist-get info :requires)
                                        requirements)
                            collect feature))))
    (unless allp
      (setf package-features (seq-intersection package-features features))
      ;; Always include the main feature: when the user explicitly
      ;; rebuilds a package, the main module must be loaded even if
      ;; it was only set up via autoloads and never fully loaded.
      (cl-pushnew package package-features))
    (let ((selected-info
           (cl-remove-if-not
            (lambda (info)
              (or (seq-intersection
                   (plist-get info :features) package-features)
                  (elpaca-extras--same-library-file-p
                   (plist-get info :artifact) located)))
            feature-info)))
      (elpaca-extras--assert-reloadable-class-layouts package selected-info))
    ;; Preserve the main feature's established first-load behavior, then load
    ;; subfeatures after their in-package requirements.  In particular, an
    ;; unchanged EIEIO subclass must be redefined after its superclass.
    (let ((main-feature-p (memq package package-features)))
      (setq package-features
            (elpaca-extras--order-features
             (delq package package-features) requirements))
      (when main-feature-p
        (push package package-features)))
    ;; Force-load each exact file via `load' rather than `require'.
    ;; `require' is a no-op when the feature is in `features', and
    ;; elpaca's rebuild can re-add features (via autoloads) before
    ;; we get here.  Loading by file also handles a module that provides more
    ;; than one feature without evaluating the same module twice.
    (let ((feature-artifacts (make-hash-table :test #'eq))
          artifacts)
      (dolist (info feature-info)
        (dolist (feature (plist-get info :features))
          (puthash feature (plist-get info :artifact) feature-artifacts)))
      (puthash package located feature-artifacts)
      (dolist (feature package-features)
        (when-let* ((artifact
                     (or (gethash feature feature-artifacts)
                         (locate-file (symbol-name feature) load-path
                                      (get-load-suffixes)))))
          (unless (cl-find artifact artifacts
                           :test #'elpaca-extras--same-library-file-p)
            (setq artifacts (nconc artifacts (list artifact))))))
      (elpaca-extras--call-with-class-layout-guard
       package
       (lambda ()
         (dolist (artifact artifacts)
           (load artifact nil 'nomessage)))))
    (when package-features
      (message "Reloaded: %s" (mapconcat #'symbol-name package-features " ")))))

;;;###autoload
(defun elpaca-extras-update-and-reload (&optional pkg)
  "Update PKG and reload its features.
If PKG is nil, prompt for it."
  (interactive (list (elpaca--read-queued "Update and reload package: ")))
  (elpaca-extras--build-and-reload pkg #'elpaca-update "Updated"))

;;;###autoload
(defun elpaca-extras-rebuild-and-reload (&optional pkg)
  "Rebuild PKG and reload its features.
If PKG is nil, prompt for it."
  (interactive (list (elpaca--read-queued "Rebuild and reload package: ")))
  (elpaca-extras--build-and-reload pkg #'elpaca-rebuild "Rebuilt"))

;;;###autoload
(defun elpaca-extras-build-reload-status (token)
  "Return the build-and-reload status plist for TOKEN."
  (when-let* ((status (gethash token elpaca-extras--build-reload-statuses)))
    (copy-sequence status)))

;;;###autoload
(defun elpaca-extras-format-build-reload-status (token)
  "Return a compact status string for build-and-reload TOKEN.
The result is formatted as STATE:MESSAGE so shell hooks can poll
completion with short `emacsclient' calls."
  (let* ((status (elpaca-extras-build-reload-status token))
         (state (or (plist-get status :state) 'missing))
         (message (or (plist-get status :message) "")))
    (format "%s:%s" state message)))

(defun elpaca-extras--build-and-reload (pkg build-fn verb)
  "Build PKG asynchronously using BUILD-FN, then reload it.
VERB is a past-tense verb for the success message (e.g., \"Updated\").

Completion is driven entirely by `elpaca-post-queue-hook', which elpaca
runs from its build-process sentinels.  Nothing blocks the command loop:
the build is enqueued and this function returns immediately, and the
reload happens once the build process actually finishes.  This avoids
`elpaca-wait', whose `sit-for' loop pumps the event loop and can wedge a
daemon that is concurrently serving `emacsclient' requests."
  (letrec ((token (elpaca-extras--build-reload-token pkg))
           (callback
            (lambda ()
              (elpaca-extras--handle-build-complete pkg callback verb token))))
    (elpaca-extras--record-build-reload-status
     token :package pkg :state 'queued :message "Build queued")
    (add-hook 'elpaca-post-queue-hook callback)
    (let ((print-length nil)
          (print-level nil))
      (funcall build-fn pkg t))
    token))

(defun elpaca-extras--build-reload-token (pkg)
  "Return a unique build-and-reload token for PKG."
  (format "%s-%s-%s" pkg (float-time) (random most-positive-fixnum)))

(defun elpaca-extras--record-build-reload-status (token &rest status)
  "Record STATUS under build-and-reload TOKEN."
  (when token
    (puthash token status elpaca-extras--build-reload-statuses)))

(defun elpaca-extras--handle-build-complete (pkg callback verb &optional token)
  "Handle build completion for PKG, removing CALLBACK from hook.
VERB is a past-tense verb for the success message.

TOKEN, when non-nil, identifies the status entry to update."
  (let* ((e (elpaca-get pkg))
         (status (and e (elpaca--status e))))
    (when (memq status '(finished failed))
      (remove-hook 'elpaca-post-queue-hook callback)
      (pcase status
        ('finished
         (condition-case reload-error
             (let ((current-load-list nil))
               (elpaca-extras-reload pkg)
               (elpaca-extras--record-build-reload-status
                token :package pkg :state 'finished
                :message (format "%s and reloaded: %s" verb pkg))
               (message "%s and reloaded: %s" verb pkg)
               'finished)
           (error
            (let ((message
                   (format "Reload failed for %s: %s"
                           pkg (error-message-string reload-error))))
              (elpaca-extras--record-build-reload-status
               token :package pkg :state 'failed :message message)
              (message "%s" message)
              'failed))))
        ('failed
         (let ((message (format "Build failed for %s: %s" pkg
                                (or (and e (nth 2 (car (elpaca<-log e))))
                                    "unknown error"))))
           (elpaca-extras--record-build-reload-status
            token :package pkg :state 'failed :message message)
           (message "%s" message)
           'failed))))))

;;;;; Lock file

;;;###autoload
(defun elpaca-extras-write-lock-file-excluding (path &optional elpacas)
  "Write a lock file to PATH, excluding selected packages.
The list of exclusions is defined in `elpaca-extras-write-lock-file-excluded'.
PATH is the destination file.

ELPACAS, when non-nil, should be a queue-like list as accepted by
`elpaca-write-lock-file'.  When it is nil the current queue is used."
  (interactive "FWrite lock-file to: ")
  (let* ((elpacas (or elpacas (elpaca--queued)))
         (filtered (cl-remove-if
                    (lambda (cell)
                      (memq (car cell) elpaca-extras-write-lock-file-excluded))
                    elpacas)))
    (elpaca-write-lock-file path filtered)))

(provide 'elpaca-extras)
;;; elpaca-extras.el ends here
