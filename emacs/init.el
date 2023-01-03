;;; init.el --- Init File -*- lexical-binding: t -*-

(message "Loading up Emacs...")
(defvar ps/core-start-time (current-time))

(defun ps/report-startup-time ()
  "Report startup time."
  (interactive)
  (message "Emacs is ready, finished loading after %.03fs."
           (float-time (time-subtract after-init-time before-init-time))))

(add-hook 'emacs-startup-hook #'ps/report-startup-time)

(defvar ps/computer-hostname-pablo "Pablos-MacBook-Pro.local")
(defvar ps/computer-hostname-leo "cartagos-MacBook-Pro.local")
(defvar ps/computer-hostname-fede "luminous-mbp.local")
  (condition-case nil
      (cond
       ;; Pablo
       ((equal (system-name) ps/computer-hostname-pablo)
        (load-file "~/Dropbox/dotfiles/emacs/variables.el"))
       ;; Leo
       ((equal (system-name) ps/computer-hostname-leo)
        (load-file "~/Dropbox/emacs/variables.el"))
       ;; Fede
       ((equal (system-name) ps/computer-hostname-fede)
        (load-file (expand-file-name "variables.el" user-emacs-directory)))
       (t
        (user-error "System not recognized")))
    (error (message "No `variables.el' file found. Your config file will not work correctly.")))

;; From Gonçalo Santos (github.com/weirdNox/dotfiles/blob/master/config/.config/emacs/config.org#helpers)
(defmacro lambda! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(setq straight-repository-branch "develop") ; must precede bootstrap
;; Bootstrap `straight'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-default-vc 'git)
(setq straight-check-for-modifications '(find-at-startup watch-files find-when-checking)) ; github.com/raxod502/straight.el#my-init-time-got-slower
(setq straight-profiles `((nil . ,ps/file-straight-profile)))
(setq straight-use-package-by-default t)
(require 'straight-x) ; github.com/raxod502/straight.el#how-do-i-uninstall-a-package

;; Early load Org from Git version instead of Emacs built-in version
(straight-use-package 'org)
(straight-use-package 'org-contrib)

(straight-use-package 'use-package)
(setq use-package-verbose nil                ; setting back to nil (default) as it was producing too much output
      use-package-compute-statistics t       ; compute stats
      use-package-always-defer t             ; always defer loading
      use-package-always-ensure nil          ; essential for `straight'
      use-package-hook-name-suffix nil       ; use real name for hooks, i.e. do not omit the `-hook' bit
      use-package-minimum-reported-time 0.1) ; report if loading a package takes longer than 100 ms
;; github.com/raxod502/radian/blob/develop/emacs/radian.el
(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(use-package use-package-ensure-system-package
  :demand t)

(setq mac-option-modifier 'meta
      mac-control-modifier 'control
      mac-command-modifier 'hyper
      mac-function-modifier 'none
      mac-right-option-modifier 'none
      mac-right-control-modifier 'super
      mac-right-command-modifier 'alt)

(setq iso-transl-char-map nil) ; https://emacs.stackexchange.com/questions/17508/

(global-unset-key (kbd "C-a"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-S-f"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-,"))
(global-unset-key (kbd "C-."))
(global-unset-key (kbd "s-a"))
(global-unset-key (kbd "s-b"))
(global-unset-key (kbd "s-c"))
(global-unset-key (kbd "s-d"))
(global-unset-key (kbd "s-e"))
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "s-g"))
(global-unset-key (kbd "s-h"))
(global-unset-key (kbd "s-i"))
(global-unset-key (kbd "s-j"))
(global-unset-key (kbd "s-k"))
(global-unset-key (kbd "s-l"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-o"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-r"))
(global-unset-key (kbd "s-s"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-u"))
(global-unset-key (kbd "s-v"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-x"))
(global-unset-key (kbd "s-y"))
(global-unset-key (kbd "s-z"))
(global-unset-key (kbd "s-SPC"))
(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-n"))
(global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-q"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "M-y"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-,"))
(global-unset-key (kbd "M-."))
(global-unset-key (kbd "H-n"))

(define-key esc-map (kbd "A-a") nil)
(define-key esc-map (kbd "A-b") nil)
(define-key esc-map (kbd "A-c") nil)
(define-key esc-map (kbd "A-d") nil)
(define-key esc-map (kbd "A-e") nil)
(define-key esc-map (kbd "A-f") nil)
(define-key esc-map (kbd "A-g") nil)
(define-key esc-map (kbd "A-h") nil)
(define-key esc-map (kbd "A-i") nil)
(define-key esc-map (kbd "A-j") nil)
(define-key esc-map (kbd "A-k") nil)
(define-key esc-map (kbd "A-l") nil)
(define-key esc-map (kbd "A-m") nil)
(define-key esc-map (kbd "A-n") nil)
(define-key esc-map (kbd "A-o") nil)
(define-key esc-map (kbd "A-p") nil)
(define-key esc-map (kbd "A-q") nil)
(define-key esc-map (kbd "A-r") nil)
(define-key esc-map (kbd "A-s") nil)
(define-key esc-map (kbd "A-t") nil)
(define-key esc-map (kbd "A-u") nil)
(define-key esc-map (kbd "A-v") nil)
(define-key esc-map (kbd "A-w") nil)
(define-key esc-map (kbd "A-x") nil)
(define-key esc-map (kbd "A-y") nil)
(define-key esc-map (kbd "A-z") nil)
(define-key esc-map (kbd "A-RET") nil)
(define-key esc-map (kbd "A-DEL") nil)
(define-key esc-map (kbd "A-SPC") nil)
(define-key esc-map (kbd "A-.") nil)
(define-key esc-map (kbd "A-,") nil)
(define-key esc-map (kbd "A-/") nil)
(define-key esc-map (kbd "A-(") nil)
(define-key esc-map (kbd "A-=") nil)
(define-key esc-map (kbd "A--") nil)
(define-key esc-map (kbd "A-'") nil)
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-j"))

(use-package no-littering
  :demand t
  :custom
  ;; github.com/emacscollective/no-littering#auto-save-settings
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package exec-path-from-shell
  :defer 5
  :config
  (dolist (var '("NVM_DIR"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package general
  :demand t
  :custom
  (general-describe-priority-keymaps '(local global org-mode-map))
  (general-describe-keybinding-sort-function #'general-sort-by-car)
  :bind
  ("C-A-b" . 'general-describe-keybindings))

(use-package el-patch
  :demand t)


