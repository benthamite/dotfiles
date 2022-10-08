;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)          ;; we initialize this later.
(setq gc-cons-threshold most-positive-fixnum) ;; Set garbage collection to highest threshold
(setq message-log-max 16384)                  ;; Turn up logging settings
(set-language-environment "UTF-8")


(unless (daemonp)
  (defvar init-file-name-handler-alist file-name-handler-alist)
  ;; Crank garbage collection to 11 for initialization.
  ;; Reset after init
  (setq file-name-handler-alist nil))

(defun init-reset-file-handler-alist ()
  "Reset `file-handler-alist' to initial value after startup."
  (setq file-name-handler-alist init-file-name-handler-alist))

(defun init-reset-garbage-collection ()
  "Reset garbage collection settings after startup."
  (setq gc-cons-threshold 16777216 ;; 16mb
        gc-cons-percentage 0.1
        message-log-max 1024))

(defun init-reset-startup-settings ()
  (init-reset-file-handler-alist)
  (init-reset-garbage-collection))

(setq byte-compile-warnings '(cl-functions)) ; https://github.com/kiwanami/emacs-epc/issues/35#issuecomment-724749625

(add-hook 'emacs-startup-hook #'init-reset-startup-settings)

(put 'list-threads 'disabled nil)
