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
        (load-file "~/Library/CloudStorage/Dropbox/dotfiles/emacs/variables.el"))
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
(setq use-package-verbose t                ; setting back to nil (default) as it was producing too much output
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

(use-package paradox
  :disabled
  ;; :defer 600
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  (paradox-github-token
   (auth-source-pass-get 'secret "auth-sources/api.github.com"))
  :config
  ;; (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(use-feature ns-win
  :custom
  (mac-option-modifier 'meta)
  (mac-control-modifier 'control)
  (mac-command-modifier 'hyper)
  (mac-function-modifier 'none)
  (mac-right-option-modifier 'none)
  (mac-right-control-modifier 'super)
  (mac-right-command-modifier 'alt))

(use-feature iso-transl
  :config
  (setq iso-transl-char-map nil) ; emacs.stackexchange.com/questions/17508/

  (unless (version< emacs-version "29.0")
  (let ((map key-translation-map))
    (keymap-unset map "A-c")
    (keymap-unset map "A-o")
    (keymap-unset map "A-u")
    (keymap-unset map "A-m")
    (keymap-unset map "A-x"))))

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

(use-package hydra
  :general
  ("H-d" 'hydra-dired/body
   "H-l" 'hydra-org-notes/body
   "H-o" 'hydra-major-modes/body
   "M-t" 'hydra-straight/body
   "M-y" 'hydra-yasnippet/body)
  (org-mode-map
   "A-s-r" 'hydra-org-rating/body))

(defhydra hydra-major-modes
  (:exit t
         :idle 0.5)
  "Major modes"
  ("a" (ps/switch-to-most-recent-buffer-in-mode 'org-agenda-mode) "Agenda")
  ("c" (ps/switch-to-most-recent-buffer-in-mode 'calendar-mode) "Calendar")
  ("d" (ps/switch-to-most-recent-buffer-in-mode 'dired-mode) "Dired")
  ("e" (ps/switch-to-most-recent-buffer-in-mode 'elfeed-search-mode) "Elfeed")
  ("f" (ps/switch-to-most-recent-buffer-in-mode 'fundamental-mode) "Fundamental")
  ("h" (ps/switch-to-most-recent-buffer-in-mode 'helpful-mode) "Helpful")
  ("i" (ps/switch-to-most-recent-buffer-in-mode 'Info-mode) "Info")
  ("j" (ps/switch-to-most-recent-buffer-in-mode 'ledger-mode) "Ledger")
  ("k" (ps/switch-to-most-recent-buffer-in-mode 'slack-message-buffer-mode) "Slack")
  ("l" (ps/switch-to-most-recent-buffer-in-mode 'emacs-lisp-mode) "Emacs Lisp")
  ("n" (ps/switch-to-most-recent-buffer-in-mode 'snippet-mode) "Snippet")
  ("o" (ps/switch-to-most-recent-buffer-in-mode 'org-mode) "Org")
  ("p" (ps/switch-to-most-recent-buffer-in-mode 'pdf-view-mode) "PDF")
  ("s" (ps/switch-to-most-recent-buffer-in-mode 'eshell-mode) "Eshell")
  ("z" (ps/switch-to-most-recent-buffer-in-mode 'special-mode) "Special")
  ("t" (ps/switch-to-most-recent-buffer-in-mode 'twittering-mode) "Twittering")
  ("w" (ps/switch-to-most-recent-buffer-in-mode 'eww-mode) "Eww")
  ("x" (ps/switch-to-most-recent-buffer-in-mode 'mhtml-mode) "XHTML+")
  ("y" (ps/switch-to-most-recent-buffer-in-mode 'python-mode) "Python"))

(general-define-key
 "H-M-s-." (lambda! (ps/visit-file-or-switch-to-buffer ps/file-inbox-mobile))
 "H-M-s-," (lambda! (ps/visit-file-or-switch-to-buffer ps/file-inbox-desktop))
 "H-M-s-a" (lambda! (ps/org-agenda-switch-to-agenda-current-day))
 "H-M-s-c" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-config))
 "H-M-s-d" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-tlon-docs))
 "H-M-s-h" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-tlon-ledger))
 "H-M-s-i" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-anki))
 "H-M-s-j" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-ledger))
 "H-M-s-k" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-karabiner))
 "H-M-s-m" 'view-echo-area-messages
 "H-M-s-o" (lambda! (switch-to-buffer "*notes*"))
 "H-M-s-q" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-quotes))
 "H-M-s-r" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-calendar))
 "H-M-s-s" (lambda! (switch-to-buffer "*scratch*"))
 "H-M-s-v" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-films))
 "H-M-s-w" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-work))
 "H-M-s-z" (lambda! (ps/visit-file-or-switch-to-buffer ps/file-variables)))

(defhydra hydra-org-notes
  (:exit t
         :idle 0.5)
  "Org headings"
  ("SPC" (ps/org-id-goto "B67C920B-D855-4A27-A35C-1DAC56580DA7") "Anki habit")
  ("i" (ps/org-id-goto "50BAC203-6A4D-459B-A6F6-461E6908EDB1") "Anki")
  ("p" (ps/org-id-goto "0070312F-6233-4BED-98F4-A2BAAEE8DAFF") "audiobooks")
  ("b" (ps/org-id-goto "7A788F19-30F5-4504-B47F-CE693AF3EA7E") "books")
  ("u" (ps/org-id-goto "78577411-554E-4EEC-B669-C014A9581540") "Current book")
  ("r" (ps/org-id-goto "1C5DCC5A-DA18-4CBD-8E2E-205766A656D6") "Documentaries")
  ("z" (ps/org-id-goto "8F8E5495-A0D8-451A-B1F1-0A8706CBF6A0") "eablogs.net")
  ("e" (ps/org-id-goto "96BBA849-B4CF-41C0-ABA3-A5D901BCDB18") "Email")
  ("d" (ps/org-id-goto "6504C81B-28F0-44C3-BFC0-2F3E648974F0") "Feeds")
  ("v" (ps/org-id-goto "E821F19E-C619-4895-A084-54D0A2772BAE") "films")
  ("f" (ps/org-id-goto "EB812B59-BBFB-4E06-865A-ACF5A4DE5A5C") "finance")
  ("/" (ps/org-id-goto "D9D71BF0-6BD6-40A5-9896-E58C7D9556B7") "inbox")
  ("m" (ps/org-id-goto "E65E393D-8694-4E23-994E-BA59A8063FCF") "Keyboard Maestro")
  ("k" (ps/org-id-goto "6F0A4889-C303-4930-8512-757AAD310535") "Khan Academy")
  ("l" (ps/org-id-goto "C308562B-222E-47E2-9A5F-B31EDB29569A") "Leonardo")
  ("," (ps/org-id-goto "E1C1F691-8358-4DDF-AC71-F46B883411BB") "morning routine")
  ("." (ps/org-id-goto "ADAA1E78-2904-4EF4-938C-F599A5C90822") "night routine")
  ("c" (ps/org-id-goto "7CE82ABB-A23F-41F6-A29E-0B95553A8FEE") "podcasts")
  ("s" (ps/org-id-goto "3513061C-5868-4EBC-9F77-9814AB776011") "Slack")
  ("j" (ps/org-id-goto "356B7595-EC5B-4DF4-949C-A637537128E4") "sleep")
  ("y" (ps/org-id-goto "FBDB7FC0-7650-48A0-933D-AE9606C2B621") "Spotify")
  ("t" (hydra-org-work/body) "tlon")
  ("n" (ps/org-id-goto "9696939D-A8B7-4179-A5C8-FEBB017DC9EF") "Telegram")
  ("q" (ps/org-id-goto "14915C82-8FF3-460D-83B3-148BB2CA7B7E") "YouTube")
  ;; ("RET'" (ps/org-id-goto "") "")
  ;; ("TAB'" (ps/org-id-goto "") "")
  ;; ("='" (ps/org-id-goto "") "")
  ;; ("-'" (ps/org-id-goto "") "")
  ;; ("('" (ps/org-id-goto "") "")
  ("H-a" (ps/org-id-notes-with-clock "a"))
  ("H-b" (ps/org-id-notes-with-clock "b"))
  ("H-c" (ps/org-id-notes-with-clock "c"))
  ("H-d" (ps/org-id-notes-with-clock "d"))
  ("H-e" (ps/org-id-notes-with-clock "e"))
  ("H-f" (ps/org-id-notes-with-clock "f"))
  ("H-g" (ps/org-id-notes-with-clock "g"))
  ("H-h" (ps/org-id-notes-with-clock "h"))
  ("H-i" (ps/org-id-notes-with-clock "i"))
  ("H-j" (ps/org-id-notes-with-clock "j"))
  ("H-k" (ps/org-id-notes-with-clock "k"))
  ("H-l" (ps/org-id-notes-with-clock "l"))
  ("H-m" (ps/org-id-notes-with-clock "m"))
  ("H-n" (ps/org-id-notes-with-clock "n"))
  ("H-o" (ps/org-id-notes-with-clock "o"))
  ("H-p" (ps/org-id-notes-with-clock "p"))
  ("H-q" (ps/org-id-notes-with-clock "q"))
  ("H-r" (ps/org-id-notes-with-clock "r"))
  ("H-s" (ps/org-id-notes-with-clock "s"))
  ("H-t" (ps/org-id-notes-with-clock "t"))
  ("H-u" (ps/org-id-notes-with-clock "u"))
  ("H-v" (ps/org-id-notes-with-clock "v"))
  ("H-w" (ps/org-id-notes-with-clock "w"))
  ("H-x" (ps/org-id-notes-with-clock "x"))
  ("H-y" (ps/org-id-notes-with-clock "y"))
  ("H-z" (ps/org-id-notes-with-clock "z"))
  ("H-," (ps/org-id-notes-with-clock ","))
  ("H-." (ps/org-id-notes-with-clock "."))
  ("H-/" (ps/org-id-notes-with-clock "/"))
  ("H-=" (ps/org-id-notes-with-clock "="))
  ("H--" (ps/org-id-notes-with-clock "-"))
  ("H-(" (ps/org-id-notes-with-clock "("))
  ("H-'" (ps/org-id-notes-with-clock "'"))
  ("H-SPC" (ps/org-id-notes-with-clock "SPC"))
  ("H-RET" (ps/org-id-notes-with-clock "RET"))
  ("H-TAB" (ps/org-id-notes-with-clock "TAB"))
  )

(defhydra hydra-dired-tlon
  (:exit t)
  "Org Headings: Tlön"
  ("d" (ps/org-id-goto "") "LBDLH")
  ("c" (ps/org-id-goto "") "core")
  ("f" (ps/org-id-goto "") "fede")
  ("g" (ps/org-id-goto "") "Dropbox: GPE")
  ("h" (ps/org-id-goto "") "Dropbox: HEAR")
  ("l" (ps/org-id-goto "") "Dropbox: leo")
  ("r" (ps/org-id-goto "") "Dropbox: RAE")
  ("s" (ps/org-id-goto "") "Dropbox: FM")
  ("t" (ps/org-id-goto "") "Dropbox: tlon")
  ("u" (ps/org-id-goto "") "Dropbox: EAN"))

(defhydra hydra-org-notes-with-clock
  (:exit t
         :idle 0.5)
  "Org headings"

  )

  (defun ps/org-id-notes-with-clock (key)
  (funcall (intern (concat "hydra-org-notes/lambda-" key "-and-exit")))
  (org-clock-in))

(defhydra hydra-org-notes-only-clock
  (:exit t
         :idle 0.5)
  "Org headings"
  ("a" (ps/org-id-notes-only-clock "a"))
  ("b" (ps/org-id-notes-with-clock "b"))
  ("c" (ps/org-id-notes-only-clock "c"))
  ("d" (ps/org-id-notes-only-clock "d"))
  ("e" (ps/org-id-notes-only-clock "e"))
  ("f" (ps/org-id-notes-only-clock "f"))
  ("g" (ps/org-id-notes-only-clock "g"))
  ("h" (ps/org-id-notes-only-clock "h"))
  ("i" (ps/org-id-notes-only-clock "i"))
  ("j" (ps/org-id-notes-only-clock "j"))
  ("k" (ps/org-id-notes-only-clock "k"))
  ("l" (ps/org-id-notes-only-clock "l"))
  ("m" (ps/org-id-notes-only-clock "m"))
  ("n" (ps/org-id-notes-only-clock "n"))
  ("o" (ps/org-id-notes-only-clock "o"))
  ("p" (ps/org-id-notes-only-clock "p"))
  ("q" (ps/org-id-notes-only-clock "q"))
  ("r" (ps/org-id-notes-only-clock "r"))
  ("s" (ps/org-id-notes-only-clock "s"))
  ("t" (ps/org-id-notes-only-clock "t"))
  ("u" (ps/org-id-notes-only-clock "u"))
  ("v" (ps/org-id-notes-only-clock "v"))
  ("w" (ps/org-id-notes-only-clock "w"))
  ("x" (ps/org-id-notes-only-clock "x"))
  ("y" (ps/org-id-notes-only-clock "y"))
  ("z" (ps/org-id-notes-only-clock "z"))
  ("," (ps/org-id-notes-only-clock ","))
  ("." (ps/org-id-notes-only-clock "."))
  ("/" (ps/org-id-notes-only-clock "/"))
  ("=" (ps/org-id-notes-only-clock "="))
  ("-" (ps/org-id-notes-only-clock "-"))
  ("(" (ps/org-id-notes-only-clock "("))
  ("'" (ps/org-id-notes-only-clock "'"))
  ("SPC" (ps/org-id-notes-only-clock "SPC"))
  ("RET" (ps/org-id-notes-only-clock "RET"))
  ("TAB" (ps/org-id-notes-only-clock "TAB"))
  )

;; save-excursion wasn't restoring point, so using this custom
;; function, from stackoverflow.com/a/24283996/4479455
(defmacro ps/save-excursion (&rest forms)
  (let ((old-point (gensym "old-point"))
        (old-buff (gensym "old-buff")))
    `(let ((,old-point (point))
           (,old-buff (current-buffer)))
       (prog1
           (progn ,@forms)
         (unless (eq (current-buffer) ,old-buff)
           (switch-to-buffer ,old-buff))
         (goto-char ,old-point)))))
(defun ps/org-id-notes-only-clock (key)
  (ps/save-excursion
   (funcall (intern (concat "hydra-org-notes/lambda-" key "-and-exit")))
   (org-clock-in)
   ))

(defhydra global-priorities-encyclopedia
  (:exit t
         :idle 0.5)
  "Org headings: global-priorities-encyclopedia.org"
  ("a" (ps/org-id-goto "") "")
  ("b" (ps/org-id-goto "") "")
  ("c" (ps/org-id-goto "") "")
  ("d" (ps/org-id-goto "") "")
  ("e" (ps/org-id-goto "") "")
  ("f" (ps/org-id-goto "E7A7125B-F14B-44FD-AB23-59A3031F0FD9") "Fede")
  ("h" (ps/org-id-goto "") "")
  ("o" (ps/org-id-goto "") "")
  ("g" (ps/org-id-goto "") "")
  ("i" (ps/org-id-goto "") "")
  ("l" (ps/org-id-goto "A37A6AED-A64F-4845-94F1-9EE08F58DED2") "Leo")
  ("n" (ps/org-id-goto "") "")
  ("p" (ps/org-id-goto "") "")
  ("s" (ps/org-id-goto "") "")
  ("t" (ps/org-id-goto "") "")
  ("w" (ps/org-id-goto "") "")
  ("x" (ps/org-id-goto "") "")
  ("z" (ps/org-id-goto "") "")
  ("'" (ps/org-id-goto "") "")
  ("," (ps/org-id-goto "") "")
  ("." (ps/org-id-goto "") "")
  ("H-a" (ps/org-id-wiki-with-clock "a"))
  ("H-b" (ps/org-id-wiki-with-clock "b"))
  ("H-c" (ps/org-id-wiki-with-clock "c"))
  ("H-d" (ps/org-id-wiki-with-clock "d"))
  ("H-e" (ps/org-id-wiki-with-clock "e"))
  ("H-f" (ps/org-id-wiki-with-clock "f"))
  ("H-g" (ps/org-id-wiki-with-clock "g"))
  ("H-h" (ps/org-id-wiki-with-clock "h"))
  ("H-i" (ps/org-id-wiki-with-clock "i"))
  ("H-j" (ps/org-id-wiki-with-clock "j"))
  ("H-k" (ps/org-id-wiki-with-clock "k"))
  ("H-l" (ps/org-id-wiki-with-clock "l"))
  ("H-m" (ps/org-id-wiki-with-clock "m"))
  ("H-n" (ps/org-id-wiki-with-clock "n"))
  ("H-o" (ps/org-id-wiki-with-clock "o"))
  ("H-p" (ps/org-id-wiki-with-clock "p"))
  ("H-q" (ps/org-id-wiki-with-clock "q"))
  ("H-r" (ps/org-id-wiki-with-clock "r"))
  ("H-s" (ps/org-id-wiki-with-clock "s"))
  ("H-t" (ps/org-id-wiki-with-clock "t"))
  ("H-u" (ps/org-id-wiki-with-clock "u"))
  ("H-v" (ps/org-id-wiki-with-clock "v"))
  ("H-w" (ps/org-id-wiki-with-clock "w"))
  ("H-x" (ps/org-id-wiki-with-clock "x"))
  ("H-y" (ps/org-id-wiki-with-clock "y"))
  ("H-z" (ps/org-id-wiki-with-clock "z"))
  ("H-," (ps/org-id-wiki-with-clock ","))
  ("H-." (ps/org-id-wiki-with-clock "."))
  ("H-/" (ps/org-id-wiki-with-clock "/"))
  ("H-=" (ps/org-id-wiki-with-clock "="))
  ("H--" (ps/org-id-wiki-with-clock "-"))
  ("H-(" (ps/org-id-wiki-with-clock "("))
  ("H-'" (ps/org-id-wiki-with-clock "'"))
  ("H-SPC" (ps/org-id-wiki-with-clock "SPC"))
  ("H-RET" (ps/org-id-wiki-with-clock "RET"))
  ("H-TAB" (ps/org-id-wiki-with-clock "TAB"))
  )

(defhydra hydra-org-work
  (:hint nil
  :idle 0
  :color blue)
  "Tlön dashboard"
  ;; ("a" (ps/org-id-goto "") "")
  ;; ("b" (ps/org-id-goto "") "")
  ;; ("c" (ps/org-id-goto "") "")
  ("r" (ps/org-id-goto "15A1803F-EAA7-4FB9-BA77-74154EB8CA5D") "RAE" :column "Main")
  ("b" (ps/org-id-goto "DFE45995-7935-4F19-80DA-FB9C11FE9E24") "BAE" :column "Main")
  ("m" (ps/org-id-goto "9066D77E-7F2B-4176-9533-243060F11276") "FM" :column "Main")
  ("d" (ps/org-id-goto "CE8A5497-1BF9-4340-9853-5ADA4605ECB5") "LBDLHD" :column "Main")
  ("u" (ps/org-id-goto "B4B9E95A-ABE1-4121-AE0B-E920E6917CBC") "EAN" :column "Main")
  ("i" (ps/org-id-goto "6C7F53ED-B43E-401D-BEEA-DB38CAE367FD") "EAI" :column "Main")
  ("w" (ps/org-id-goto "72EE8B25-D847-49F5-B6D9-E3B67BEB071A") "PW" :column "Other")
  ("v" (ps/org-id-goto "7333FEC5-90A7-423D-9C45-2D5333593F87") "Samotsvety" :column "Other")
  ("x" (ps/org-id-goto "E13198C9-8F3F-46D8-B052-6F6ADF6B4D99") "Misc" :column "Other")
  ("a" (ps/org-id-goto "830A5DA5-AB9A-483A-B8AC-C5CCBD3A02FD") "EA Archive" :column "Someday")
  ("n" (ps/org-id-goto "177F4865-3B25-41C0-999B-B9B67DFAC110") "EA Nomad" :column "Someday")
  ("h" (ps/org-id-goto "1BBBA5F1-11FA-4C7B-8D08-5DC84233B8E2") "HEAR" :column "On hold")
  ("g" (ps/org-id-goto "DA0B3751-6B25-4F53-AE27-7B6CBC29B6C1") "GPE" :column "On hold")
  ("" (ps/org-id-goto "2514AA39-CFBF-4E5A-B18E-147497E31C8F") "LP" :column "Done")
  ("" (ps/org-id-goto "470C263E-40F8-4567-83BC-85DE6E5F8D5A") "RCGs" :column "Done")
  ("" (ps/org-id-goto "AE8F5AD4-B85A-4EE2-8A94-AA7B2AFF3E7F") "Regranting" :column "Done")
  ("e" (ps/org-id-goto "EA0B83B2-8A4A-417A-8318-56B4EDC75FF5") "email" :column "Comms")
  ("s" (ps/org-id-goto "A45FEDFB-1928-4571-97F3-03D20A78883C") "slack" :column "Comms")
  ("t" (ps/org-id-goto "DF643B0F-1956-44AB-90DD-749D849C285D") "telegram" :column "Comms")
  ("f" (ps/org-id-goto "AED9330C-1673-4669-A367-4B87614965F6") "fede" :column "People")
  ("F" (ps/tlon-meeting-with-fede) "fede: meeting" :column "People")
  ("H-f" (ps/org-id-goto "CE0C7638-97F1-4509-8212-5B77F4A4AF29") "fede: tareas" :column "People")
  ("l" (ps/org-id-goto "4EF48AB3-44B4-4791-BDFC-537F3B636FDA") "leo" :column "People")
  ("L" (ps/tlon-meeting-with-leo) "leo: meeting" :column "People")
  ("H-l" (ps/org-id-goto "76A01EAA-74BC-41FC-9050-E6BDC0D56029") "leo: tareas" :column "People")
  ("RET" (ps/org-id-goto "843EE71C-4D50-4C2F-82E6-0C0AA928C72A"))
  )

(defhydra hydra-org-work-with-clock
  (:exit t
         :idle 0.5)
  "Org headings"

  )

  (defun ps/org-id-work-with-clock (key)
  (funcall (intern (concat "hydra-org-work/lambda-" key "-and-exit")))
  (org-clock-in))

(defhydra hydra-org-tlon-docs
  (:exit 1)
  "Buffer-local commands"
  ("." (call-interactively #'ps/telega-docs-change-notify) "Notify of changes"))

(defhydra hydra-org-config
  (:exit 1)
  "Org headings: config.org"
  ("c" (ps/org-id-goto "50FAD2F3-E501-408E-A9A2-8358FAA87C1C") "Calc")
  ("d" (ps/org-id-goto "617F5323-6518-4751-948B-3E8032D93130") "Dired")
  ("e" (ps/org-id-goto "FF5DDBC3-ABB6-48A9-9B47-BC9A18F532D5") "Elfeed")
  ("f" (ps/org-id-goto "B29F4586-2B8D-41FE-82DE-FEDCD863C74B") "Files & buffers")
  ("g" (ps/org-id-goto "AACAE0F4-0B25-475B-831B-3F1E91E6349D") "Graveyard")
  ("h" (hydra-org-config-hydra/body) "Hydra")
  ("i" (ps/org-id-goto "A7940400-DD17-4B0B-A9B2-565A207D680C") "Introduction")
  ("k" (ps/org-id-goto "4373E661-B19D-4E6C-B7DE-C2A26619A515") "Wiki")
  ("l" (ps/org-id-goto "DE6D2307-9EBD-4E0F-B873-003C9813CA27") "Display")
  ("m" (ps/org-id-goto "E83EC00B-0C94-44CD-9EC0-355992C99234") "Completion ")
  ("n" (ps/org-id-goto "179BB021-8B2A-4BF0-B3AA-43AF5A212D4B") "Text manipulation")
  ("o" (hydra-org-config-org/body) "Org")
  ("p" (ps/org-id-goto "7F0CBD06-FDB3-4889-91CE-D8A25D4F2613") "Help")
  ("s" (ps/org-id-goto "9FDBBF3E-724F-4402-9DDB-F9349F65AB0E") "Search")
  ("t" (ps/org-id-goto "1E8F4417-5D5F-4406-BB70-AA272F714EF2") "Text movement")
  ("u" (ps/org-id-goto "AA460F4A-4035-4C96-A3A1-078A43F7892D") "user-init")
  ("v" (ps/org-id-goto "10E891D3-9DF5-472A-8E3C-1DE30EE8C81F") "Variables")
  ("w" (ps/org-id-goto "7E9A81E0-CAEB-4029-AD2C-B2416439FCDA") "Windows & frames")
  ("y" (ps/org-id-goto "6405B8E7-6612-4D71-8C2C-A51F8808F4C6") "Yasnippets"))

(defhydra hydra-org-config-hydra
  (:exit 1)
  "Org headings: config.org > hydra"
  ("c" (ps/org-id-goto "F19DA0AC-B303-4A6B-8B4E-6E94FC98BC78") "Hydra config")
  ("e" (ps/org-id-goto "CC88D9BE-6617-4D53-BCCF-02097C2A81E1") "Hydra wiki entries")
  ("f" (ps/org-id-goto "39E06A29-2AEC-4EB4-A0D0-7E1A64832B18") "Hydra files")
  ("h" (ps/org-id-goto "00E7E217-E02E-489B-968D-E49431FD5ECC") "Hydra main")
  ("n" (ps/org-id-goto "C3A44EA2-5523-45DD-8100-6228D80ECAC8") "Hydra notes")
  ("o" (ps/org-id-goto "E6290A57-7035-4ADB-89F6-9CCADF2D74DB") "Hydra mode buffers")
  ("r" (ps/org-id-goto "F6AA197E-73A7-4688-986F-4A1D583BBA99") "Hydra org ratings")
  ("s" (ps/org-id-goto "40207396-12B9-4374-9341-713E88772275") "Hydra Straight")
  ("w" (ps/org-id-goto "B7B8956F-08D9-49E0-873C-4513F6FD44B8") "Hydra wiki notes")
  ("y" (ps/org-id-goto "5CC3B9AA-629B-407A-899D-529E66A7D057") "Hydra yasnippets"))

(defhydra hydra-org-config-org
  (:exit 1)
  "Org headings: config.org > org"
  ("a" (ps/org-id-goto "E03F4142-C90D-4550-8990-15391E27AD77") "org-agenda")
  ("b" (ps/org-id-goto "52C959E4-54F4-4499-AE3A-5251F6337FA0") "org key bindings")
  ("c" (ps/org-id-goto "14F93A83-0BE7-42E3-891E-F6806192296B") "org-capture")
  ("m" (ps/org-id-goto "2F2E4C1E-4D9B-4A28-B08F-B381E83CFE17") "org-roam")
  ("n" (ps/org-id-goto "A1BA5ED1-BF56-4C33-81F8-19D2AFC7F6D7") "org-noter")
  ("o" (ps/org-id-goto "268B60E4-708C-4372-A59D-5DD876E493CA") "org-mode")
  ("f" (ps/org-id-goto "35FB5BB5-6552-48C6-983A-F90011CCA908") "org-ref")
  ("r" (ps/org-id-goto "3FAE7C0D-FB22-4175-A0A4-FFA392539743") "org-refile")
  ("t" (ps/org-id-goto "8AF25840-AC38-4FF7-A45F-F01B96C5DF5A") "org-cite")
  ("x" (ps/org-id-goto "EC73B84D-530E-4179-BB67-F19110A543DF") "org-roam-bibtex"))

(defhydra hydra-dired
  (:exit t)
  "Dired folders"
  ("a" (hydra-dired-google-drive/body) "apps")
  ("b" (dired ps/dir-bibliography) "bibliography")
  ("d" (dired ps/dir-dotfiles) "dotfiles")
  ("e" (dired ps/dir-emacs) "Emacs")
  ("i" (dired ps/dir-anki) "Anki")
  ("j" (dired ps/dir-health) "Health")
  ("k" (dired ps/dir-PW) "PW")
  ("m" (hydra-dired-music/body) "Music")
  ("n" (dired ps/dir-notes) "Notes")
  ("o" (dired ps/dir-google-drive) "Google Drive")
  ("p" (dired ps/dir-people) "people")
  ;; ("q" (dired ps/dir-youtube))
  ("t" (hydra-dired-tlon/body) "Tlön")
  ("H-t" (hydra-dired-tlon/body) "Tlön")
  ("U" (dired ps/dir-audiobooks) "Audiobooks")
  ("v" (dired ps/dir-movies) "movies")
  ("w" (dired ps/dir-downloads) "downloads")
  ("x" (dired ps/dir-dropbox) "Dropbox")
  ("y" (dired ps/dir-library-pdf) "Library: PDF")
  ("z" (dired ps/dir-library-html) "Library: HTML")
  ;; ("'" (dired ps/dir-) "")
  ;; ("-" (dired ) "")
  ("." (dired-at-point) "File at point")
  ("/" (dired "/") "Root")
  ("SPC" (dired "~/") "user")
  (";" (dired-jump) "Current buffer")
  ("H-;" (dired-jump-other-window) "Current buffer in other window"))

(defhydra hydra-dired-tlon
  (:exit t)
  "Dired folders: Tlön"
  ("b" (dired ps/dir-dropbox-tlon-BAE) "Dropbox: BAE")
  ("H-b" (dired ps/dir-google-drive-tlon-BAE) "Google Drive: BAE")
  ("c" (dired ps/dir-dropbox-tlon-core) "core")
  ("H-c" (dired ps/dir-google-drive-tlon-core) "Google Drive: core")
  ("d" (dired ps/dir-dropbox-tlon-LBDLH) "Dropbox: LBDLH")
  ("H-d" (dired ps/dir-google-drive-tlon-LBDLH) "Google Drive: LBDLH")
  ("f" (dired ps/dir-dropbox-tlon-fede) "fede")
  ("H-f" (dired ps/dir-google-drive-tlon-fede) "Google Drive: fede")
  ("g" (dired ps/dir-dropbox-tlon-GPE) "Dropbox: GPE")
  ("H-g" (dired ps/dir-google-drive-tlon-GPE) "Google Drive: GPE")
  ("h" (dired ps/dir-dropbox-tlon-HEAR) "Dropbox: HEAR")
  ("H-h" (dired ps/dir-google-drive-tlon-HEAR) "Google Drive: HEAR")
  ("l" (dired ps/dir-dropbox-tlon-leo) "Dropbox: leo")
  ("H-l" (dired ps/dir-google-drive-tlon-leo) "Google Drive: leo")
  ("p" (dired ps/dir-dropbox-tlon-LP) "Dropbox: LP")
  ("H-p" (dired ps/dir-google-drive-tlon-LP) "Google Drive: LP")
  ("r" (dired ps/dir-dropbox-tlon-RAE) "Dropbox: RAE")
  ("H-r" (dired ps/dir-google-drive-tlon-RAE) "Google Drive: RAE")
  ("s" (dired ps/dir-dropbox-tlon-FM) "Dropbox: FM")
  ("H-s" (dired ps/dir-google-drive-tlon-FM) "Google Drive: FM")
  ("t" (dired ps/dir-dropbox-tlon) "Dropbox: tlon")
  ("H-t" (dired ps/dir-google-drive-tlon) "Google Drive: tlon")
  ("u" (dired ps/dir-dropbox-tlon-EAN) "Dropbox: EAN")
  ("H-u" (dired ps/dir-google-drive-tlon-EAN) "Google Drive: EAN"))

(defhydra hydra-dired-google-drive
  (:exit t)
  "Dired folders: apps"
  ;; TODO: parametrize
  ("i" (dired "~/Google Drive/Apps/Anki") "Anki")
  ("c" (dired "~/Google Drive/Apps/Emacs") "Emacs")
  ("m" (dired "~/Google Drive/Apps/Keyboard Maestro") "Keyboard Maestro")
  ("'" (hydra-dired/body) "back"))
(defhydra hydra-dired-music
  (:exit t)
  "Dired folders: music"
  ("c" (dired ps/dir-music-classical) "classical")
  ("p" (dired ps/dir-music-popular) "popular")
  ("t" (dired ps/dir-music-tango) "tango")
  ("s" (dired ps/dir-music-to-sort) "to sort")
  ("'" (hydra-dired/body) "back"))

(defhydra hydra-org-rating
  (:exit t
         :idle 0.5)
  "Org ratings"
  ("1" (org-set-property "RATING" "1") "1")
  ("2" (org-set-property "RATING" "2") "2")
  ("3" (org-set-property "RATING" "3") "3")
  ("4" (org-set-property "RATING" "4") "4")
  ("5" (org-set-property "RATING" "5") "5")
  ("6" (org-set-property "RATING" "6") "6")
  ("7" (org-set-property "RATING" "7") "7")
  ("8" (org-set-property "RATING" "8") "8")
  ("9" (org-set-property "RATING" "9") "9")
  ("0" (org-set-property "RATING" "10") "10"))

(defhydra hydra-straight (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("q" nil))

(defhydra hydra-wiki
  (:exit t
         :idle 0.3)
  "Wiki sections"
  ("a" (ps/yasnippet-expand-by-key "wber") "bibliography, external links, related entries (all)")
  ("b" (ps/yasnippet-expand-by-key "wbib") "bibliography")
  ("c" (ps/yasnippet-expand-by-key "wnec") "new entry checklist")
  ("e" (ps/yasnippet-expand-by-key "wel") "external links")
  ("l" (ps/yasnippet-expand-by-key "wil") "internal link")
  ("w" (ps/yasnippet-expand-by-key "wow") "official website")
  ("o" (ps/yasnippet-expand-by-key "woe") "online entry")
  ("p" (ps/yasnippet-expand-by-key "wpe") "published entry")
  ("r" (ps/yasnippet-expand-by-key "wre") "related entries")
  ("u" (ps/org-append-unpublished-heading) "unpublished"))

(defun ps/capitalize-first-char (&optional string)
  "Capitalize only the first character of STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun ps/replace-spaces-with-underscores (&optional string)
  "Replace spaces in string with underscores in STRING."
  (when (and string (> (length string) 0))
    (replace-regexp-in-string "[ ,]" "_" string)))

(use-package bug-hunter
  :general
  ("<f5>" 'bug-hunter-init-file))

(use-feature warnings
  :demand t
  :config
  (dolist (element '((comp)
                     (yasnippet backquote-change)))
    (add-to-list 'warning-suppress-types element)))

(use-feature comp
  :defer 60
  :custom
  (native-comp-async-report-warnings-errors nil))

(use-feature bytecomp
  :demand t
  :custom
  (byte-compile-warnings '(cl-functions)))

(use-feature startup
  :custom
  (user-full-name ps/personal-name)
  (user-mail-address ps/personal-gmail)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-buffer-choice 'remember-notes)
  (initial-major-mode 'emacs-lisp-mode))

(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package async
  :commands dired-async-mode)

(setq use-dialog-box nil)
(setq default-directory ps/dir-dropbox)
(setq use-short-answers t)
(setq message-log-max 10000)
(setq ring-bell-function 'ignore) ; silence bell when mistake is made
(setq x-stretch-cursor t) ; make curor the width of the character under it
;; emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
;; UTF8 stuff.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default line-spacing 2)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start Emacs maximized

(use-package mixed-pitch
  :demand t
  :custom
  (mixed-pitch-set-height t)

  :config
  (set-face-attribute 'variable-pitch nil :family ps/face-variable-pitch :height 1.4)

  :hook
  (mu4e-view-mode-hook . mixed-pitch-mode)
  (org-mode-hook . mixed-pitch-mode)
  (outline-mode-hook . mixed-pitch-mode))

(use-package fontaine
  :disabled
  :demand t
  :config
  (fontaine-mode))

(use-feature face-remap
  :demand t
  :general
  (eww-mode-map
   "+" 'text-scale-increase
   "-" 'text-scale-decrease))

(use-feature fringe
  :init
  (setq-default fringe-indicator-alist
                '((truncation nil nil)
                  (continuation nil nil)
                  (overlay-arrow . right-triangle)
                  (up . up-arrow)
                  (down . down-arrow)
                  (top top-left-angle top-right-angle)
                  (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                  (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                  (empty-line . empty-line)
                  (unknown . question-mark))))

(use-package org-modern
  :demand t
  :custom
  (org-modern-table nil)
  (org-modern-statistics nil)
  (org-modern-star '("◉" "◉" "◉" "◉" "◉"))
  (org-modern-list '((42 . "○")
                     (43 . "○")
                     (45 . "○")))
  :config
  (global-org-modern-mode))

(use-feature org-faces
  :after org-modern

  :custom
  (org-fontify-quote-and-verse-blocks t)

  :config
  (defun ps/org-faces-custom-faces ()
    "My custom faces, to be used in conjunction with theme."
    (set-face-attribute 'default nil :family ps/face-fixed-pitch :height 115)
    (set-face-attribute 'fixed-pitch nil :family ps/face-fixed-pitch :height 1.1)
    (set-face-attribute 'org-drawer nil :foreground "LightSkyBlue" :family ps/face-fixed-pitch :height 0.8)
    (set-face-attribute 'org-property-value nil :family ps/face-fixed-pitch :height 0.8)
    (set-face-attribute 'org-todo nil :family ps/face-fixed-pitch :height 1.0)
    (set-face-attribute 'org-archived nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-document-title nil :family ps/face-fixed-pitch :height 1.0)
    (set-face-attribute 'org-special-keyword nil :family ps/face-fixed-pitch :height 0.8)
    (set-face-attribute 'org-tag nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-code nil :family ps/face-fixed-pitch :height 1.1)
    (set-face-attribute 'org-level-1 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-2 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-3 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-4 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-5 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-6 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-7 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-level-8 nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-date nil :family ps/face-fixed-pitch :height 0.8)
    (set-face-attribute 'org-modern-date-active nil :family ps/face-fixed-pitch :height 0.8)
    (set-face-attribute 'org-modern-date-inactive nil :family ps/face-fixed-pitch :height 0.8)
    (set-face-attribute 'org-modern-tag nil :family ps/face-fixed-pitch :height 0.9)
    (set-face-attribute 'org-quote nil :family ps/face-variable-pitch :height 1.3)
    (set-face-attribute 'corfu-default nil :family ps/face-fixed-pitch :height 1)))

(use-feature theme-loaddefs
  :config
  (defvar ps/theme-loaddefs-light 'modus-operandi)
  (defvar ps/theme-loaddefs-dark 'modus-vivendi))

(use-package modus-themes
  :straight (modus-themes
             :host sourcehut
             :repo "protesilaos/modus-themes")
  :demand t

  :custom
  (modus-themes-org-blocks 'gray-background)

  :init
  (defun ps/modus-themes-highlight-parentheses ()
    (modus-themes-with-colors
      (setq highlight-parentheses-background-colors (list bg-cyan-intense
                                                          bg-magenta-intense
                                                          bg-green-intense
                                                          bg-yellow-intense)
            highlight-parentheses-colors (list cyan
                                               magenta
                                               green
                                               yellow))))

  :config
  (defun ps/modus-themes-load-theme-emacs-mac ()
    "Load modus theme that matches system."
    (interactive)
    (if (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua")
        (modus-themes-load-theme ps/theme-loaddefs-light)
      (modus-themes-load-theme ps/theme-loaddefs-dark)))

  (defun ps/modus-themes-load-theme-emacs-plus (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (modus-themes-load-theme ps/theme-loaddefs-light))
      ('dark (modus-themes-load-theme ps/theme-loaddefs-dark))))

  (defun ps/modus-themes-load-theme-conditionally ()
    "Load themes conditional on which distribution of Emacs is
installed."
    (cond ((boundp 'mac-effective-appearance-change-hook)
           ;; `emacs-mac'
           (ps/modus-themes-load-theme-emacs-mac))
          ;; `emacs-plus'
          ((boundp 'ns-system-appearance-change-functions)
           (add-hook 'ns-system-appearance-change-functions
                     #'ps/modus-themes-load-theme-emacs-plus))))

  (setq modus-themes-common-palette-overrides
        `(
          ;; hide the fringe
          (fringe unspecified)
          ;; additional customizations can be added here

          ;; for the rest, use the predefined intense values

          ,@modus-themes-preset-overrides-intense))

  (ps/modus-themes-load-theme-conditionally)

  :hook
  (modus-themes-after-load-theme-hook . ps/org-faces-custom-faces)
  (modus-themes-after-load-theme-hook . ps/modus-themes-highlight-parentheses)

  :general
  ("A-d" 'modus-themes-toggle))

(use-package highlight-parentheses
  :demand t
  :custom
  (highlight-parentheses-delay 0)

  :config
  (global-highlight-parentheses-mode)

  :hook
  (minibuffer-setup-hook . highlight-parentheses-minibuffer-setup))

(use-package emojify
  :config
  (emojify-set-emoji-styles '(unicode))
  :hook
  (after-init . global-emojify-mode)
  :general
  ("H-e" 'emojify-insert-emoji))

(use-package lin
  :demand t
  :custom
  (lin-face 'lin-blue)
  (lin-mode-hooks
   '(dired-mode-hook
     elfeed-search-mode-hook
     git-rebase-mode-hook
     grep-mode-hook
     ibuffer-mode-hook
     ilist-mode-hook
     ledger-report-mode-hook
     log-view-mode-hook
     magit-log-mode-hook
     mu4e-headers-mode
     occur-mode-hook
     org-agenda-mode-hook
     pdf-outline-buffer-mode-hook
     proced-mode-hook
     tabulated-list-mode-hook))

  :config
  (lin-global-mode))

(use-feature image
  :config
  ;; Use imagemagick, if available.
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))

(use-feature paren
  :custom
  (show-paren-delay 0)

  :config
  (show-paren-mode))

(use-package doom-modeline
  :demand t
  :init
  (doom-modeline-mode)
  :custom
  ;; we disable the display of time since the tab-bar already does
  (doom-modeline-time nil)
  (doom-modeline-mu4e t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-irc t)
  (doom-modeline-irc-buffers t)
  ;; (doom-modeline--flycheck-icon t)
  (doom-modeline-github t)
  (doom-modeline-github-interval (* 10 60))
  (doom-modeline-buffer-file-name-style 'truncate-from-project)

  :config
  (remove-hook 'display-time-mode-hook 'doom-modeline-override-display-time-modeline)
  (remove-hook 'display-battery-mode-hook 'doom-modeline-override-battery-modeline)
  (remove-hook 'doom-modeline-mode-hook 'doom-modeline-override-display-time-modeline)
  (remove-hook 'doom-modeline-mode-hook 'doom-modeline-override-battery-modeline)
  (add-hook 'doom-modeline-before-github-fetch-notification-hook #'auth-source-pass-enable))

(use-feature tab-bar
  :demand t
  :custom
  (tab-bar-format '(tab-bar-align-right
                    tab-bar-format-global))
  (auto-resize-tab-bar nil)

  :config
  (setf mode-line-misc-info
        ;; When the tab-bar is active, don't show global-mode-string
        ;; in mode-line-misc-info, because we now show that in the
        ;; tab-bar using `tab-bar-format-align-right' and
        ;; `tab-bar-format-global'.
        (remove '(global-mode-string ("" global-mode-string))
                mode-line-misc-info))

  (run-with-timer 5 nil (lambda () (setq global-mode-string '(" "
                             (:eval (propertize display-time-string 'face 'ps/display-time))
                             " | "
                             fancy-battery-mode-line
                             " | "
                             telega-mode-line-format
                             " | "))))

  (tab-bar-mode))

(use-package fancy-battery
  :defer 10
  :custom
  (fancy-battery-show-percentage t)

  :config
  (fancy-battery-mode))

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-completion
  :demand t
  :after (all-the-icons marginalia vertico)
  :config
  (all-the-icons-completion-mode)

  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

;; (use-package gnuplot-mode
;; :demand t)
(use-package gnuplot)

(use-feature delsel
  :demand t
  :config
  (delete-selection-mode))

(use-feature hl-line
  :config
  (global-hl-line-mode))

(use-feature jit-lock
  :defer 10
  :custom
  ;; I had to hand-code the value because `(*
  ;; (window-max-chars-per-line) (window-body-height))' evaluated on
  ;; startup outputs a value much lower than when the expression is
  ;; evaluated manually (~2500 vs. ~12000), not sure why.
  ;;
  (jit-lock-chunk-size 120000 "emacs.stackexchange.com/a/72439/32089"))

(use-feature profiler
  :config
  (defvar ps/profiler-toggle nil)
  (defun ps/profiler-toggle ()
    (interactive)
    "Starts Emacs profiler if not already running. Otherwise stops it
and generates profiling report."
    (if (not ps/profiler-toggle)
        (profiler-start 'cpu+mem)
      (profiler-report)
      (profiler-stop))
    (setq ps/profiler-toggle (not ps/profiler-toggle)))

(defun ps/profiler-report-toggle-entry-global ()
"Expand all subentries below entry at point."
(interactive)
(profiler-report-toggle-entry '(4)))

  :general
  ("A-C-p" 'ps/profiler-toggle)
  (profiler-report-mode-map
   "<backtab>" 'ps/profiler-report-toggle-entry-global))

(use-feature so-long
  :config
  (global-so-long-mode))

(use-package gcmh
  :demand t
  :config
  (gcmh-mode))

(use-feature emacs
  :custom
  (bidi-display-reordering nil)
  (inhibit-compacting-font-caches t)
  (redisplay-skip-fontification-on-input t)
  (bidi-inhibit-bpa t)

  :config
   ;; emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ; 1mb.

  (setq-default bidi-paragraph-direction 'left-to-right))

(use-feature bindings
  :general
  (w3m-minor-mode-map
   "<left>" 'left-char
   "<right>" 'right-char))

(use-feature simple
  :general
   ("A-C-s-p" 'forward-word
   "A-C-s-u" 'backward-word))

(use-feature simple
  :general
  ("A-C-s-m" 'move-beginning-of-line
   ;; karabiner maps `/' to `z'; otherwise I can't trigger the command while holding `shift'
   "A-C-s-z" 'move-end-of-line)
  ((custom-mode-map ebib-index-mode-map ebib-entry-mode-map help-mode-map helpful-mode-map Info-mode-map Man-node-map org-lint--report-mode-map osa-chrome-mode-map mu4e-view-mode-map eww-mode-map elfeed-search-mode-map elfeed-show-mode-map pass-mode-map elisp-refs-mode-map special-mode-map twittering-mode-map)
   "k" 'previous-line
   "l" 'next-line)
  ((eshell-hist-mode-map w3m-minor-mode-map)
   "<up>" 'previous-line
   "<down>" 'next-line))

(use-feature paragraphs
  :general
  ("A-C-s-i" 'backward-sentence
   "A-C-s-o" 'forward-sentence))

(use-feature paragraphs
  :general
  ("A-C-s-," 'backward-paragraph
   "A-C-s-." 'forward-paragraph))

(use-feature lisp
  :general
  ("A-C-s-e" 'backward-sexp
   "A-H-M-s-d" 'forward-sexp ; nonstandard binding because otherwise intercepted by OSX
   ))

(use-feature lisp
  :general
  ("A-C-s-w" 'beginning-of-defun
   "A-C-s-s" 'end-of-defun))

(use-feature simple
  :general
  ("A-C-s-<tab>" 'beginning-of-buffer
   "A-C-s-SPC" 'end-of-buffer))

(defmacro ps/delete-instead-of-kill (&rest body)
  "Replaces `kill-region' with `delete-region' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end)
                (delete-region beg end))))
     ,@body))

(defmacro ps/copy-instead-of-kill (&rest body)
  "Replaces `kill-region' with `kill-ring-save' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end)
                (kill-ring-save beg end)
                (setq this-command 'kill-region))))
     ,@body))

(defun ps/kill-whole-thing (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(general-define-key
 "C-H-M-g" 'append-next-kill
 "A-M-n" 'ps/remove-newlines-from-region)

(use-feature simple
  :config
(defun ps/backward-zap-to-char ()
  (interactive)
  (zap-to-char -1 (read-char-from-minibuffer "Zap to char: "
                                             nil 'read-char-history)))

(defun ps/zap-copy-to-char (arg char)
  "Copy up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char-from-minibuffer "Zap to char: "
                                                nil 'read-char-history)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (copy-region-as-kill (point) (progn
                                 (search-forward (char-to-string char) nil nil arg)
                                 (point))))

(defun ps/backward-zap-copy-to-char ()
  (interactive)
  (ps/zap-copy-to-char -1 (read-char-from-minibuffer "Zap to char: "
                                                     nil 'read-char-history)))

(defun ps/zap-delete-to-char (arg char)
  "Copy up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
See also `zap-up-to-char'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char-from-minibuffer "Zap to char: "
                                                nil 'read-char-history)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (delete-region (point) (progn
                           (search-forward (char-to-string char) nil nil arg)
                           (point))))

(defun ps/backward-zap-delete-to-char ()
  (interactive)
  (ps/zap-delete-to-char -1 (read-char-from-minibuffer "Zap to char: "
                                                       nil 'read-char-history)))

(defun ps/transpose-chars-backward ()
  "Interchange characters around point, moving backward one character."
  (interactive)
  (transpose-chars -1))

:general
( "A-H-M-d" 'transpose-chars
 "A-H-M-s" 'ps/transpose-chars-backward
 "C-H-M-s" 'delete-backward-char
 "C-H-M-d" 'delete-forward-char
 "C-H-M-t" 'just-one-space
 "C-H-M-f" 'zap-to-char
 "C-H-M-a" 'ps/backward-zap-to-char
 "A-C-H-M-S-s-f" 'ps/zap-delete-to-char
 "A-C-H-M-S-s-a" 'ps/backward-zap-delete-to-char
 "C-H-M-s-A-f" 'ps/zap-copy-to-char
 "C-H-M-s-A-a" 'ps/backward-zap-copy-to-char
 "C-H-M-=" 'overwrite-mode)
(org-mode-map
 "C-H-M-s" 'org-delete-backward-char))

(use-feature simple
  :config
  (defun ps/delete-word (&optional arg)
    "Like `kill-word', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (kill-word arg)))

  (defun ps/backward-delete-word (&optional arg)
    "Like `backward-kill-word', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (backward-kill-word arg)))

  (defun ps/copy-word (&optional arg)
    "Like `kill-word', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (kill-word arg)))

  ;; The macro wasn't working for `backward-kill-word', so using a custom function.
  (defun ps/backward-copy-word ()
    "Like `backward-kill-word', but copies instead of killing."
    (interactive)
    (copy-region-as-kill (point) (progn (backward-word) (point))))

  (defun ps/kill-whole-word ()
    "Kill the word at point."
    (interactive)
    (ps/kill-whole-thing 'word))

  (defun ps/delete-whole-word ()
    "Like `kill-whole-word', but deletes instead of killing."
    (interactive)
    (ps/delete-instead-of-kill (ps/kill-whole-word)))

  (defun ps/copy-whole-word (&optional arg)
    "Like `kill-whole-word', but copies instead of killing."
    (interactive)
    (ps/copy-instead-of-kill (ps/kill-whole-word)))

  (defun ps/transpose-words-backward ()
    "Interchange words around point, leaving point at beginning."
    (interactive)
    (transpose-words -1))

  :general
  ("C-<delete>" nil
   "M-DEL" nil
   "C-H-M-r" 'kill-word
   "C-H-M-q" 'backward-kill-word
   "A-C-H-M-S-s-r" 'ps/delete-word
   "A-C-H-M-S-s-q" 'ps/backward-delete-word
   "C-H-M-s-A-r" 'ps/copy-word
   "C-H-M-s-A-q" 'ps/backward-copy-word
   "A-H-C-r" 'ps/delete-whole-word
   "A-H-C-q" 'ps/copy-whole-word
   "A-H-C-u" 'ps/kill-whole-word
   "A-H-M-r" 'transpose-words
   "A-H-M-q" 'ps/transpose-words-backward))

(use-feature simple
  :config
(defun ps/delete-line (&optional arg)
  "Like `kill-line', but deletes instead of killing."
  (interactive "p")
  (ps/delete-instead-of-kill (kill-line arg)))

(defun ps/backward-delete-line (&optional arg)
  "Like `backward-kill-line', but deletes instead of killing."
  (interactive "p")
  (ps/delete-instead-of-kill (kill-line 0)))

(defun ps/copy-line (&optional arg)
  "Like `kill-line', but copies instead of killing."
  (interactive "P")
  (ps/copy-instead-of-kill (kill-line arg)))

(defun ps/backward-copy-line (&optional arg)
  "Like `backward-kill-line', but copies instead of killing."
  (interactive "P")
  (ps/copy-instead-of-kill (kill-line 0)))

(defun ps/kill-whole-line ()
  "Kill the line at point."
  (interactive)
  (ps/kill-whole-thing 'line))

(defun ps/delete-whole-line ()
  "Like `kill-whole-line', but deletes instead of killing."
  (interactive)
  (ps/delete-instead-of-kill (ps/kill-whole-line)))

(defun ps/copy-whole-line ()
  "Like `kill-whole-line', but copies instead of killing."
  (interactive)
  (ps/copy-instead-of-kill (ps/kill-whole-line)))

(defun ps/transpose-lines-backward ()
  "Exchange current line and previous line, leaving point
between the two."
  (interactive)
  (transpose-lines -1))

:general
("C-H-M-v" 'kill-line
 "C-H-M-z" 'crux-kill-line-backwards
 "A-C-H-M-S-s-v" 'ps/delete-line
 "A-C-H-M-S-s-z" 'ps/backward-delete-line
 "C-H-M-s-A-v" 'ps/copy-line
 "C-H-M-s-A-z" 'ps/backward-copy-line
 "A-H-C-v" 'ps/delete-whole-line
 "A-H-C-m" 'ps/kill-whole-line
 "A-H-C-z" 'ps/copy-whole-line
 "A-H-M-v" 'transpose-lines
 "A-H-M-z" 'ps/transpose-lines-backward))

(use-feature paragraphs
  :config
  (defun ps/delete-sentence (&optional arg)
    "Like `kill-sentence', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (kill-sentence arg)))

  (defun ps/backward-delete-sentence (&optional arg)
    "Like `backward-kill-sentence', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (backward-kill-sentence arg)))

  (defun ps/copy-sentence (&optional arg)
    "Like `kill-sentence', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (kill-sentence arg)))

  (defun ps/backward-copy-sentence (&optional arg)
    "Like `backward-kill-sentence', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (backward-kill-sentence arg)))

  (defun ps/kill-whole-sentence ()
    "Kill the sentence at point."
    (interactive)
    (ps/kill-whole-thing 'sentence))

  (defun ps/delete-whole-sentence ()
    "Like `kill-whole-sentence', but deletes instead of killing."
    (interactive)
    (ps/delete-instead-of-kill (ps/kill-whole-sentence)))

  (defun ps/copy-whole-sentence ()
    "Like `kill-whole-sentence', but copies instead of killing."
    (interactive)
    (ps/copy-instead-of-kill (ps/kill-whole-sentence)))

  (defun ps/transpose-sentences-backward ()
    "Interchange the current sentence with the previous one."
    (interactive)
    (transpose-sentences -1))

  :general
  ;; :keymaps '(text-mode-map org-mode-map outline-mode-map telega-chat-mode-map)
  ("C-H-M-e" 'kill-sentence
   "C-H-M-w" 'backward-kill-sentence
   "A-C-H-M-S-s-e" 'ps/delete-sentence
   "A-C-H-M-S-s-w" 'ps/backward-delete-sentence
   "C-H-M-s-A-e" 'ps/copy-sentence
   "C-H-M-s-A-w" 'ps/backward-copy-sentence
   "A-H-C-e" 'ps/delete-whole-sentence
   "A-H-C-w" 'ps/copy-whole-sentence
   "A-H-C-i" 'ps/kill-whole-sentence
   "A-H-M-e" 'transpose-sentences
   "A-H-M-w" 'ps/transpose-sentences-backward))

(use-feature emacs
  :custom
  (sentence-end-double-space nil)

  :config
  (defun ps/delete-paragraph (&optional arg)
    "Like `kill-paragraph', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (kill-paragraph arg)))

  (defun ps/backward-delete-paragraph (&optional arg)
    "Like `backward-kill-paragraph', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (backward-kill-paragraph arg)))

  (defun ps/copy-paragraph (&optional arg)
    "Like `kill-paragraph', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (kill-paragraph arg)))

  (defun ps/backward-copy-paragraph (&optional arg)
    "Like `backward-kill-paragraph', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (backward-kill-paragraph arg)))

  (defun ps/kill-whole-paragraph ()
    "Kill the paragraph at point."
    (interactive)
    (ps/kill-whole-thing 'paragraph))

  (defun ps/delete-whole-paragraph ()
    "Like `kill-whole-paragraph', but deletes instead of killing."
    (interactive)
    (ps/delete-instead-of-kill (ps/kill-whole-paragraph)))

  (defun ps/copy-whole-paragraph ()
    "Like `kill-whole-paragraph', but copies instead of killing."
    (interactive)
    (ps/copy-instead-of-kill (ps/kill-whole-paragraph)))

  (defun ps/transpose-paragraphs-backward ()
    "Interchange the current paragraph with the previous one."
    (interactive)
    (transpose-paragraphs -1))

  :general
  ("M-k" nil)
  ((text-mode-map org-mode-map outline-mode-map telega-chat-mode-map)
   "C-H-M-c" 'kill-paragraph
   "C-H-M-x" 'backward-kill-paragraph
   "A-C-H-M-S-s-c" 'ps/delete-paragraph
   "A-C-H-M-S-s-x" 'ps/backward-delete-paragraph
   "C-H-M-s-A-c" 'ps/copy-paragraph
   "C-H-M-s-A-x" 'ps/backward-copy-paragraph
   "A-H-C-c" 'ps/delete-whole-paragraph
   "A-H-C-x" 'ps/copy-whole-paragraph
   "A-H-C-," 'ps/kill-whole-paragraph
   "A-H-M-c" 'transpose-paragraphs
   "A-H-M-x" 'ps/transpose-paragraphs-backward))

(use-feature emacs
  :config
  (defun ps/delete-sexp (&optional arg)
    "Like `kill-sexp', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (kill-sexp arg)))

  (defun ps/backward-delete-sexp (&optional arg)
    "Like `backward-kill-sexp', but deletes instead of killing."
    (interactive "p")
    (ps/delete-instead-of-kill (backward-kill-sexp arg)))

  (defun ps/copy-sexp (&optional arg)
    "Like `kill-sexp', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (kill-sexp arg)))

  (defun ps/backward-copy-sexp (&optional arg)
    "Like `backward-kill-sexp', but copies instead of killing."
    (interactive "P")
    (ps/copy-instead-of-kill (backward-kill-sexp arg)))

  (defun ps/kill-whole-sexp ()
    "Kill the sexp at point."
    (interactive)
    (ps/kill-whole-thing 'sexp))

  (defun ps/delete-whole-sexp ()
    "Like `kill-whole-sexp', but deletes instead of killing."
    (interactive)
    (ps/delete-instead-of-kill (ps/kill-whole-sexp)))

  (defun ps/copy-whole-sexp ()
    "Like `kill-whole-sexp', but copies instead of killing."
    (interactive)
    (ps/copy-instead-of-kill (ps/kill-whole-sexp)))

  (defun ps/transpose-sexps-backward ()
    "Like `transpose-sexps', but in reverse order."
    (interactive)
    (transpose-sexps -1))

  :general
  ("C-M-k" nil
   "C-M-<backspace>" nil
   "C-H-M-f" 'kill-sexp
   "C-H-M-a" 'backward-kill-sexp
   "A-C-H-M-S-s-f" 'ps/delete-sexp
   "A-C-H-M-S-s-a" 'ps/backward-delete-sexp
   "C-H-M-s-A-f" 'ps/copy-sexp
   "C-H-M-s-A-a" 'ps/backward-copy-sexp
   "A-H-C-a" 'ps/copy-whole-sexp
   "A-H-C-f" 'ps/delete-whole-sexp
   "A-H-C-j" 'ps/kill-whole-sexp
   "A-H-M-f" 'transpose-sexps
   "A-H-M-a" 'ps/transpose-sexps-backward))

(use-package org
  :general
  (org-mode-map
   "C-H-M-s-z" 'org-shiftleft
   "C-H-M-s-x" 'org-shiftup
   "C-H-M-s-c" 'org-shiftdown
   "C-H-M-s-v" 'org-shiftright
   "C-H-M-s-a" 'org-metaleft
   "C-H-M-s-s" 'org-metaup
   "C-H-M-s-d" 'org-metadown
   "C-H-M-s-f" 'org-metaright
   "C-H-M-s-q" 'org-shiftmetaleft
   "C-H-M-s-w" 'org-shiftmetaup
   "C-H-M-s-e" 'org-shiftmetadown
   "C-H-M-s-r" 'org-shiftmetaright))

(defun ps/smart-kill-region ()
  "kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun ps/smart-delete-region ()
  "kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-region)
    (call-interactively 'ps/delete-whole-line)))

(defun ps/smart-copy-region ()
  "kill region if active, else kill line."
  (interactive)
  (if (region-active-p)
      (call-interactively 'copy-region-as-kill)
    (call-interactively 'ps/copy-whole-line)))

(general-define-key
 "H-c" 'ps/smart-copy-region
 "H-x" 'ps/smart-kill-region
 "H-X" 'ps/smart-delete-region)

;; github.com/typester/emacs/blob/master/lisp/url/url-util.el
(defun ps/get-url-at-point (&optional pt)
  "Get the URL closest to point, but don't change position.
Has a preference for looking backward when not directly on a symbol."
  ;; Not at all perfect - point must be right in the name.
  (save-excursion
    (if pt (goto-char pt))
    (let (start url)
      (save-excursion
        ;; first see if you're just past a filename
        (if (not (eobp))
            (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
                (progn
                  (skip-chars-backward " \n\t\r({[]})")
                  (if (not (bobp))
                      (backward-char 1)))))
        (if (and (char-after (point))
                 (string-match (eval-when-compile
                                 (concat "[" "-%.?@a-zA-Z0-9()_/:~=&" "]"))
                               (char-to-string (char-after (point)))))
            (progn
              (skip-chars-backward "-%.?@a-zA-Z0-9()_/:~=&")
              (setq start (point))
              (skip-chars-forward "-%.?@a-zA-Z0-9()_/:~=&"))
          (setq start (point)))
        (setq url (buffer-substring-no-properties start (point))))
      (if (and url (string-match "^(.*)\\.?$" url))
          (setq url (match-string 1 url)))
      (if (and url (string-match "^URL:" url))
          (setq url (substring url 4 nil)))
      (if (and url (string-match "\\.$" url))
          (setq url (substring url 0 -1)))
      (if (and url (string-match "^www\\." url))
          (setq url (concat "http://" url)))
      (if (and url (not (string-match url-nonrelative-link url)))
          (setq url nil))
      url)))

(defun ps/strip-url ()
  "Strip URL of unnecessary elements."
  (interactive)
  (unless (ps/get-url-at-point)
    (error "No URL at point."))
  (let* ((url-original (ps/get-url-at-point))
         (url-stripped (replace-regexp-in-string "\\(?:https?://\\)?\\(?:www.\\)?" "" url-original)))
    (search-backward " ")
    (while (search-forward url-original nil t)
      (replace-match url-stripped nil t))
    (search-backward url-stripped)))

(defun ps/strip-thing-at-point ()
  "Strip thing at point. (To be expanded.)"
  (interactive)
  (cond ((ps/get-url-at-point)
         (ps/strip-url)))
  (just-one-space 0))

(general-define-key
 "C-H-M-b" 'ps/strip-thing-at-point)

(use-feature simple
  :custom
  (kill-ring-max 500)
  (save-interprogram-paste-before-kill t) ; add system clipboard to kill ring

  :config
  (defun ps/yank-pop-forward (arg)
    (interactive "p")
    (yank-pop (- arg)))

  (defun ps/yank-and-pop ()
    "Yank, then pop the last kill off the ring."
    (interactive)
    (yank)
    (when kill-ring
      (setq kill-ring (cdr kill-ring)))
    (when kill-ring-yank-pointer
      (setq kill-ring-yank-pointer kill-ring))
    (message "Last kill popped off kill-ring."))

  :general
  ("H-v" 'yank
   "H-A-v" 'ps/yank-and-pop))

(use-feature simple
  :demand t
  :custom
  (shift-select-mode nil "Shift keys do not activate the mark momentarily.")
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (eval-expression-print-level nil)
  (eval-expression-print-length nil)
  (print-level nil)
  (print-length nil)

  :config
  (column-number-mode)

  ;; spwhitton.name/blog/entry/transient-mark-mode/
  (defun ps/exchange-point-and-mark (arg)
    "Exchange point and mark, but reactivate mark a bit less often.

Specifically, invert the meaning of ARG in the case where
Transient Mark mode is on but the region is inactive."
    (interactive "P")
    (exchange-point-and-mark
     (if (and transient-mark-mode (not mark-active))
         (not arg)
       arg)))

  (defun ps/visible-mode-enhanced ()
    "Toggle `visible-mode' as well as appropriate actions associated
with the current major mode."
    (interactive)
    (if visible-mode
        (progn
          (visible-mode -1)
          (org-display-inline-images)
          (ps/org-hide-properties)
          (ps/org-hide-logbook)
          (org-modern-mode))
      (visible-mode)
      (org-remove-inline-images)
      (ps/org-show-properties)
      (ps/org-show-logbook)
      (org-modern-mode -1)))

  (defun ps/count-words-dwim ()
    "Count the number of words in region, if active, otherwise in
clipboard. Either way, save count to kill ring."
    (interactive)
    (if (region-active-p)
        (let ((count (how-many "\\w+" (region-beginning) (region-end))))
          (message "%s words in region" count)
          (kill-new (number-to-string count))
          (message "region has %s words" count))
      (let ((clipboard-text (current-kill 0)))
        (with-temp-buffer
          (insert clipboard-text)
          (let ((clipboard-count (kill-new (format "%d" (count-words-region (point-min) (point-max))))))
            (message "Clipboard has %d words" (count-words-region (point-min) (point-max))))))))

  :general
  ("A-H-c" 'ps/count-words-dwim
   "A-H-e" 'eval-defun
   "C-A-e" 'eval-expression
   "C-e" 'eval-last-sexp
   "H-M"  'ps/exchange-point-and-mark
   "H-m" 'set-mark-command
   "H-Z" 'undo-redo
   "M-A-i" 'visual-line-mode
   "M-o" 'downcase-dwim
   "M-q" 'save-buffers-kill-terminal
   "M-u" 'capitalize-dwim
   "M-v" 'ps/visible-mode-enhanced
   "M-w" 'count-words-region
   "H-z" 'undo-only))

(use-feature repeat
  :general
  ("M-r" 'repeat
   "A-M-r" 'repeat-complex-command))

(use-feature view
  :general
  ("M-A-v" 'view-mode))

(use-feature emacs
  :custom
  (tab-always-indent 'complete)

  :config
  ;; Adapted from `spacemacs/indent-region-or-buffer'.
  (defun ps/indent-dwim ()
    "Indent a region if selected, otherwise if point is on code block
indent block only, else indent whole buffer."
    (interactive)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (if (when (derived-mode-p 'org-mode)
              (org-in-src-block-p))
            (let ((org-src-tab-acts-natively t))
              (org-narrow-to-block)
              (indent-region (point-min) (point-max) nil)
              (ps/org-widen-and-reveal))
          (indent-region (point-min) (point-max) nil)
          (message "Indented buffer.")))
      (whitespace-cleanup)))

  :general
  ("M-i" 'ps/indent-dwim))

(use-feature sort
  :custom
  (sort-fold-case t)
  :general
  ("C-t" 'sort-lines))

(use-feature outline
  :general
  ((outline-mode-map outline-minor-mode-map)
   "TAB" 'outline-cycle
   "<backtab>" 'outline-cycle-buffer
   "A-C-s-r" 'outline-backward-same-level
   "A-C-s-f" 'outline-forward-same-level))

(use-feature ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-feature fill
  :init
  ;; endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
  (defun ps/fill-or-unfill-paragraph ()
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
           (if (eq last-command 'ps/fill-or-unfill-paragraph)
               (progn (setq this-command nil)
                      (point-max))
             fill-column)))
      (call-interactively #'fill-paragraph)))

  :general
  ("A-M-f" 'ps/fill-or-unfill-paragraph))

(use-feature abbrev
  :custom
  (save-abbrevs 'silently)
  (abbrev-file-name (file-name-concat ps/dir-abbrev "abbrev_defs"))

  :config
  (setq-default abbrev-mode t))

(use-package yasnippet
  :defer 5
  :init
  (setq yas-snippet-dirs `(,ps/dir-yasnippets))

  :custom
  (yas-triggers-in-field t "allow stacked expansions")

  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (add-to-list 'warning-suppress-log-types '(yasnippet backquote-change))

  ;; FIXME: this isn't working properly, unlike the `ps/yasnippet-expand-by-key' command
  (defun ps/yasnippet-expand-by-name (name)
    "Expand the yasnippet named `snippet'."
    (yas-expand-snippet (yas-lookup-snippet name)))

  (defun ps/yasnippet-expand-by-key (key)
    "Expand snippet whose key is KEY."
    (interactive)
    ;; `progn' wrapper needed to suppress elisp output when snippet triggered within another snippet
    ;; TODO: figure why this still fails when the snippet being triggered contains elisp code
    (progn
      (insert key)
      (yas-expand)
      nil))

  (defun ps/yasnippet-expand-code-block-snippet (key)
    "Expand code block snippet and edit block in separate buffer."
    (ps/yasnippet-expand-by-key key)
    (org-previous-block nil)
    (org-edit-src-code))

  (defvar yas-new-snippet-default "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}\n# --\n$0")

  (defvar ps/yas-new-snippet-tlon "# -*- mode: snippet -*-\n# name: tlon-${1:name-of-snippet}\n# key: t${2:shortcut}\n# --\n$0\\`(ps/insert-date)\\` \\$1\n	${3:account1}:\\$5  \\$2.00 \\${3:\\$\\$(yas-choose-value '('USD' 'EUR' 'GBP'))} ; \\$4\n	${4:account2}:")

  (defun ps/yas-new-tlon-snippet (&optional no-template)
    "Pops a new buffer for writing a snippet.

Expands a snippet-writing snippet, unless the optional prefix arg
NO-TEMPLATE is non-nil."
    (interactive "P")
    (let ((guessed-directories (yas--guess-snippet-directories))
          (yas-selected-text (or yas-selected-text
                                 (and (region-active-p)
                                      (buffer-substring-no-properties
                                       (region-beginning) (region-end))))))

      (switch-to-buffer yas-new-snippet-buffer-name)
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (yas-minor-mode 1)
      (set (make-local-variable 'yas--guessed-modes)
           (mapcar (lambda (d) (yas--table-mode (car d)))
                   guessed-directories))
      (set (make-local-variable 'default-directory)
           (car (cdr (car guessed-directories))))
      (if (and (not no-template) ps/yas-new-snippet-tlon)
          (yas-expand-snippet ps/yas-new-snippet-tlon))))

  :general
  ("C-y" 'yas-new-snippet)
  (minibuffer-mode-map
   "TAB" 'yas-maybe-expand))

(use-feature hippie-expand
  :config
  ;; stackoverflow.com/a/8723712/4479455
  (defadvice hippie-expand (around hippie-expand-case-fold)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))
  (ad-activate 'hippie-expand)

  :general
  ("M-<tab>" 'hippie-expand))

(use-package expand-region
  :general
   ("C-H-s-n" 'er/expand-region
   "C-H-s-h" 'er/contract-region))

(use-package evil-nerd-commenter
  :general
  ("M-/" 'evilnc-comment-or-uncomment-lines))

(use-package multiple-cursors
  :general
  ("M-m" 'mc/mark-pop))

(use-package crux
  :config
  ;; Modified version of `crux-smart-open-line' to handle cases when
  ;; command is invoked on an org heading, which results in an
  ;; invisible new line.
  (defun ps/crux-smart-open-line (arg)
    "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
    (interactive "P")
    (ps/visible-mode-enhanced)
    (if arg
        (crux-smart-open-line-above)
      (move-end-of-line nil)
      (when (or (org-at-heading-p) (org-at-drawer-p) (org-at-timestamp-p))
        (org-end-of-meta-data t)
        (when (eq org-cycle-subtree-status 'folded)
          (show-subtree)))
      (newline-and-indent))
    (ps/visible-mode-enhanced))

  :general
  ("M-l" 'ps/crux-smart-open-line
   "M-A-l" (lambda! (ps/crux-smart-open-line t))
   "H-A-l" 'crux-duplicate-current-line-or-region))

(use-feature button
  :general
  ("A-C-M-s-j" 'backward-button
   "A-C-M-s-;" 'forward-button)
  (telega-chat-mode-map
   "M-RET" 'push-button))

(use-package back-button
  ;; :defer 25
  :config
  (back-button-mode 1)
  :general
  ("H-," 'back-button-local-backward
   "H-." 'back-button-local-forward
   "H-<" 'back-button-global-backward
   "H->" 'back-button-global-forward))

(use-package goto-last-change
  :general
  ("C-z" 'goto-last-change))

(use-package titlecase)

(use-feature goto-addr
  :config
  (global-goto-address-mode))

(use-feature register
  :config
  (defhydra hydra-register
  (:hint nil
  :color blue)
    "
_c_opy        |_n_umber      |p_o_int
_i_nsert      |incremen_t_   |_j_ump        |   _f_rame    | _l_ist
------------^^+------------^^+------------^^+  frame_s_et  + _v_iew       ||_q_uit||
_p_repend     |_r_ectangle   |_u_ndo        |   _w_indow   | _h_elm
_a_pend       |_k_macro      |r_e_store                    "
    ;; text
    ("c" copy-to-register)
    ("i" insert-register)
    ("p" prepend-to-register)
    ("a" append-to-register)
    ;; number
    ("n" number-to-register)
    ("t" increment-register)
    ;; rectangle/kmacro
    ("r" copy-rectangle-to-register)
    ("k" kmacro-to-register)
    ;; position
    ("o" point-to-register)
    ("j" jump-to-register)
    ;; undo
    ("u" undo-tree-save-state-to-register)
    ("e" undo-tree-restore-state-from-register)
    ;; windows
    ("w" window-configuration-to-register)
    ("f" frame-configuration-to-register)
    ("s" frameset-to-register)
    ;; view
    ("v" view-register)
    ("l" list-registers)
    ("h" consult-register)
    ("q" nil))
  :general
  ("C-r" 'hydra-register/body))

(use-feature bookmark
  :custom
  (bookmark-default-file ps/file-bookmarks) ; Set location of bookmarks file
  (bookmark-save-flag 1)) ; Save bookmarks after each entry

(use-feature files
  :custom
  (confirm-kill-processes nil "Do not prompt to kill running processes when quitting Emacs")
  (delete-by-moving-to-trash t)
  (trash-directory "~/.Trash" "fallback for `move-file-to-trash'")
  (find-file-visit-truename t)
  (kill-buffer-query-functions nil "emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation")
  (create-lockfiles nil "lockfiles are indexed by `org-roam', which causes problems with `org-agenda'")
  (large-file-warning-threshold (* 200 1000 1000))
  (enable-local-variables :all)
  (insert-directory-program "/opt/homebrew/bin/gls" "use coreutils to avoid 'listing directory failed' error")
  (auto-save-no-message t "don't emit message when auto-saving")
  (backup-by-copying t "don't clobber symlink")
  (kept-new-versions 100 "keep 100 latest versions")
  (kept-old-versions 10 "keep 10 earliest versions")
  (delete-old-versions t "don't ask about deleting old versions")
  (version-control t "number backups")

  :config
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

  ;; christiantietze.de/posts/2021/06/emacs-trash-file-macos/
  (defun system-move-file-to-trash (path)
    "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention.

Requires the command-line utility `trash' (`brew install trash')."
    (shell-command (concat "trash -vF \"" path "\""
                           "| sed -e 's/^/Trashed: /'")
                   nil ;; Name of output buffer
                   "*Trash Error Buffer*"))

  (defun ps/save-and-revert-buffer ()
    "Save buffer, then revert it."
    (interactive)
    (save-buffer)
    (revert-buffer nil t))

  (defun ps/bury-scratch-buffer ()
    "When trying to kill `*scratch' buffer, bury it instead."
    (if (not (equal (buffer-name) "*scratch*"))
        t
      (bury-buffer)
      nil))

  (add-hook 'kill-buffer-query-functions #'ps/bury-scratch-buffer)

  (defun ps/org-id-goto (id &optional arg)
    "Open ID even if narrowed."
    (dotimes (i 2)
      (ps/org-widen-and-reveal)
      (org-id-goto id))
    (ps/org-narrow-to-entry-and-children))

  ;; Adapted from `spacemacs/new-empty-buffer'.
  (defun ps/new-empty-buffer (&optional)
    "Create a new buffer called `untitled<n>'."
    (interactive)
    (let ((newbuf (generate-new-buffer "untitled")))
      ;; Prompt to save on `save-some-buffers' with positive PRED
      (with-current-buffer newbuf
        (setq-local buffer-offer-save t)
        (when ps/new-empty-buffer-major-mode
          (funcall ps/new-empty-buffer-major-mode)))
      (switch-to-buffer newbuf nil 'force-same-window)))


  (defun ps/new-buffer-in-current-mode ()
    "Create a new buffer in the same major mode
as the current buffer."
    (interactive)
    (let ((buffer-name (generate-new-buffer "untitled"))
          (buffer-major-mode major-mode))
      (cond ((eq buffer-major-mode 'shell-mode)
             (shell))
            ((eq buffer-major-mode 'eshell-mode)
             (eshell))
            (t
             ;; Prompt to save on `save-some-buffers' with positive PRED
             (with-current-buffer buffer-name
               (setq-local buffer-offer-save t)
               (funcall buffer-major-mode))
             (switch-to-buffer buffer-name nil 'force-same-window)))))

  (defun ps/new-empty-buffer-other-window ()
    "Create a new buffer called `untitled<n>' in other window."
    (interactive)
    (ps/switch-to-last-window)
    (ps/new-empty-buffer))

  (defun ps/save-all-buffers ()
    "Save all file-visiting buffers."
    (interactive)
    (save-some-buffers
     `(4)))

  (defun ps/visit-file-or-switch-to-buffer (thing)
    "Visit file or switch to corresponding file-visiting buffer."
    (interactive)
    (if (get-buffer thing)
        (switch-to-buffer thing)
      (find-file thing)))

  (defun ps/org-show-subtree-hide-drawers ()
    (outline-hide-subtree)
    (org-show-entry)
    (org-show-children))

  (defun ps/org-narrow-to-entry-and-children ()
    "Narrow org buffer to entry and all its children."
    (interactive)
    (org-narrow-to-subtree)
    (ps/org-show-subtree-hide-drawers))

  (defun ps/org-narrow-to-entry-no-children ()
    "Narrow org buffer to entry excluding all children."
    (interactive)
    (org-narrow-to-subtree)
    (save-excursion
      (org-next-visible-heading 1)
      (narrow-to-region (point-min) (point))))

  (defun ps/org-widen-and-reveal ()
    "Widen and reveal subtree."
    (interactive)
    (widen)
    (org-reveal nil))

  (defun ps/eval-region-or-buffer ()
    "Evaluate a region if selected, otherwise the whole buffer."
    (interactive)
    (if (region-active-p)
        (eval-region (region-beginning) (region-end))
      (eval-buffer)))

  ;; Adapted from alphapapa: reddit.com/r/orgmode/comments/fuvdqv/org_notetaking_workflow_with_orgroam/fmhl3ml/
  (defun ps/org-to-indirect-buffer ()
    "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
    (interactive "P")
    (let* ((new-buffer-p)
           (pos (point))
           (buffer-name (let* ((heading (org-get-heading t t))
                               (level (org-outline-level))
                               (face (intern (concat "outline-" (number-to-string level))))
                               (heading-string (propertize (org-link-display-format heading)
                                                           'face face)))
                          (concat heading-string "::" (buffer-name))))
           (new-buffer (or (get-buffer buffer-name)
                           (prog1 (condition-case nil
                                      (make-indirect-buffer (current-buffer) buffer-name 'clone)
                                    (error (make-indirect-buffer (current-buffer) buffer-name)))
                             (setq new-buffer-p t)))))
      (switch-to-buffer new-buffer)
      (when new-buffer-p
        ;; I don't understand why setting the point again is necessary, but it is.
        (goto-char pos)
        (rename-buffer buffer-name)
        (org-narrow-to-subtree))))

  (defun ps/get-alternate-buffer ()
    "Return name of last buffer active in the current window."
    (let ((current-buffer (window-buffer))
          (buffer-predicate
           (frame-parameter (window-frame) 'buffer-predicate)))
      ;; switch to first buffer previously shown in this window that matches
      ;; frame-parameter `buffer-predicate'
      (or (cl-find-if (lambda (buffer)
                        (and (not (eq buffer current-buffer))
                             (or (null buffer-predicate)
                                 (funcall buffer-predicate buffer))))
                      (mapcar #'car (window-prev-buffers)))
          ;; `other-buffer' honors `buffer-predicate' so no need to filter
          (other-buffer current-buffer t))))

  (defun ps/switch-to-alternate-buffer ()
    "Switch to the last buffer active in the current window."
    (interactive)
    (switch-to-buffer (ps/get-alternate-buffer)))

  (defun ps/switch-to-other-alternate-buffer ()
    "Switch to the last buffer in the other window."
    (interactive)
    (other-window 1)
    (switch-to-buffer (ps/get-alternate-buffer)))

  ;; reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/
  (defun ps/kill-this-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  (defun ps/kill-other-buffer ()
    "Kill the buffer in the other window."
    (interactive)
    (save-window-excursion
      (other-window 1)
      (ps/kill-this-buffer)))

  (defun ps/kill-this-buffer-switch-to-other-window ()
    "Kill the current buffer and switch to the other window."
    (interactive)
    (ps/kill-this-buffer)
    (ps/switch-to-last-window))

  (defun ps/kill-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

  (defun ps/bury-buffer-switch-to-other-window ()
    "Bury the current buffer and switch to the other window."
    (interactive)
    (bury-buffer)
    (ps/switch-to-last-window))

  (defun ps/download-bypass-paywalls-chrome ()
    "Download and install `bypass-paywalls-chrome'."
    (interactive)
    (let ((file (file-name-concat ps/dir-downloads "bypass-paywalls.zip")))
      (url-copy-file "https://github.com/iamadamdev/bypass-paywalls-chrome/archive/master.zip" file)
      (shell-command (format "unzip %s -d %s" file ps/dir-downloads))
      (dired ps/dir-downloads)
      (goto-char (point-min))
      (search-forward "bypass-paywalls-chrome-master")
      (reveal-in-osx-finder)
      (shell-command "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"89243CDA-4876-45C8-9AF2-3666664A0EAA\"'")))

  (defun ps/internet-archive-dwim ()
    "Convert or download ACSM file, depending on whether or not an
ACSM file is present in `ps/dir-downloads'."
    (interactive)
    (if (member "book.acsm" (directory-files ps/dir-downloads))
        (ps/internet-archive-convert-ACSM)
      (ps/internet-archive-download-ACSM)))

  (defun ps/internet-archive-download-ACSM ()
    "Download and open ACSM file from Internet Archive URL in kill
ring.

NB: You need to have previously borrowed the book for the command
to work. The command will work even if the book was borrowed for
one hour only."
    (if (string-search "archive.org" (current-kill 0))
        (let* ((prefix "https://archive.org/services/loans/loan/?action=media_url&identifier=")
               (suffix "&format=pdf&redirect=1")
               (id (replace-regexp-in-string
                    "\\(http.*?details/\\)\\([_[:alnum:]]*\\)\\(.*\\)"
                    "\\2"
                    (current-kill 0)))
               (url (concat prefix id suffix))
               (acsm-file (file-name-concat ps/dir-downloads "book.acsm")))
          ;; Download the Internet Archive cookies to a file so `wget' can authenticate:
          ;; askubuntu.com/questions/161778/how-do-i-use-wget-curl-to-download-from-a-site-i-am-logged-into
          ;; Then replace the path below with the location of the downloaded cookies file.
          (save-window-excursion
            (let ((shell-command-buffer-name-async "*internet-archive-download-ACSM*"))
              (async-shell-command
               (format
                "wget --load-cookies='%s' '%s' -O '%s'; open %s"
                ps/file-cookies url acsm-file acsm-file))))
          (message "ACSM file downloaded successfully."))
      (user-error "You forgot to copy the URL!")))

  (defun ps/internet-archive-convert-ACSM ()
    "Convert ACSM file to PDF."
    (let* ((adobe-file
            ;; stackoverflow.com/a/30887300/4479455
            (car (directory-files (file-name-as-directory ps/dir-ade) 'full "\\.pdf$" #'file-newer-than-file-p)))
           (output (shell-command-to-string (format "calibredb add '%s'" adobe-file)))
           ;; Capture Calibre book id
           (id (replace-regexp-in-string "\\(\\(\\(
\\|.\\)*\\)Added book ids: \\)\\([[:digit:]]\\)" "\\4" output))
           (calibre-file (car (directory-files-recursively ps/dir-calibre "\\.pdf$" t)))
           ;; Should match filename used in `ps/internet-archive-download-ACSM'
           (acsm-file (file-name-concat ps/dir-downloads "book.acsm")))
      (rename-file calibre-file (file-name-as-directory ps/dir-downloads))
      (shell-command (format "calibredb remove %s" id))
      (mapcar #'delete-file `(,adobe-file ,calibre-file))
      (delete-directory ps/dir-calibre t)
      (kill-buffer "*Shell Command Output*")
      (when (find-file acsm-file)
        (delete-file acsm-file)
        (kill-buffer))
      (message "ACSM file converted successfully.")))

  ;; Copied from emacs.stackexchange.com/a/24461/32089
  (defun ps/revert-all-file-buffers ()
    "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be
reverted. They will be reverted though if they were modified
outside emacs. Buffers visiting files which do not exist any more
or are no longer readable will be killed."
    (interactive)
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buf)))
          (if (file-readable-p filename)
              ;; If the file exists and is readable, revert the buffer.
              (with-current-buffer buf
                (revert-buffer :ignore-auto :noconfirm :preserve-modes))
            ;; Otherwise, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buf)
              (message "Killed non-existing/unreadable file buffer: %s" filename))))))
    (message "Finished reverting buffers containing unmodified files."))

  (defun ps/get-title (file)
    "Return title of buffer at point."
    (let (title)
      (when file
        (with-current-buffer
            (get-file-buffer file)
          (pcase (org-collect-keywords '("TITLE"))
            (`(("TITLE" . ,val))
             (setq title (car val)))))
        title)))

  ;; stackoverflow.com/a/44489067/4479455
  (defun ps/show-buffer-file-name ()
    "Show the full path to the current file in the minibuffer."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (progn
            (message file-name)
            (kill-new file-name))
        (error "Buffer not visiting a file"))))

  (defun ps/show-buffer-name ()
    "Show the full path to the current file in the minibuffer."
    (interactive)
    (let ((buffer-name (buffer-name)))
      (if buffer-name
          (progn
            (message buffer-name)
            (kill-new buffer-name))
        (error "Buffer not visiting a file"))))

  ;; On MacOS, `DS_Store' files can interfere with this command.
  ;; Disable their creation with
  ;; `defaults write com.apple.desktopservices DSDontWriteNetworkStores true'
  (defun ps/newest-file (path)
    "Get latest file (including directory) in PATH."
    (car
     (seq-find
      #'(lambda (x) (not (nth 1 x))) ; non-directory
      (sort
       (directory-files-and-attributes path 'full nil t)
       #'(lambda (x y) (time-less-p (nth 5 y) (nth 5 x)))))))

  (defun ps/switch-to-most-recent-buffer-in-mode (mode)
    "Switch to the most recent buffer in major mode MODE."
    (let (found)
      (catch 'done
        (mapc (lambda (x)
                (when (with-current-buffer x (eq major-mode mode))
                  (switch-to-buffer x)
                  (setq found t)
                  (throw 'done nil)))
              (buffer-list))
        (unless found
          (print "not found")))))

  ;; stackoverflow.com/questions/21486934/file-specific-key-binding-in-emacs/21493693#21493693
  (defun ps/buffer-local-set-key (key command)
    (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
    (let ((oldmap (current-local-map))
          (newmap (make-sparse-keymap)))
      (when oldmap
        (set-keymap-parent newmap oldmap))
      (define-key newmap key command)
      (use-local-map newmap)))

  (defun ps/ocr-pdf (&optional parameters)
    "OCR the PDF file at point or visited by the current buffer."
    (interactive)
    ;; TODO: add disjunct to handle file at point in minibuffer.
    (let* ((filename (cond ((equal major-mode 'dired-mode) (dired-get-filename))
                           ((equal major-mode 'pdf-view-mode) (buffer-file-name))))
           (parameters (or parameters
                           (format "--force '%s' '%s'" filename filename)))
           (shell-command-buffer-name-async "*ocr-pdf*"))
      (async-shell-command (concat "ocrmypdf " parameters))))

  ;; we add `*ocr-pdf' buffer to list of buffers not to be displayed,
  ;; so that the process runs in the background`
  (push '("*ocr-pdf*" display-buffer-no-window) display-buffer-alist)

  (defun ps/kill-buffer ()
    "Ugly hack to kill buffer when
`citar-filenotify-rm-local-watches' prevents it."
    (interactive)
    (defun  citar-filenotify-rm-local-watches ())
    (kill-buffer)
    (defun  citar-filenotify-rm-local-watches ()
      "Delete the filenotify watches for the local bib files."
      (mapc #'file-notify-rm-watch citar-filenotify--local-watches)
      (setq citar-filenotify--local-watches 'uninitialized)))

  ;; Disable `dired-hide-details-mode' so that session dates are shown
  ;; when `recover-session' is invoked.
  (advice-add 'recover-session :after #'(lambda () (dired-hide-details-mode -1)))

  (defun ps/get-stem-of-current-buffer ()
    "Return the stem of the current buffer."
    (when-let ((file-name buffer-file-name))
      (file-name-sans-extension (file-name-nondirectory file-name))))

      (defun ps/bollp ()
    "Return t if point is at the beginning of the last line."
    (let ((beginning-of-last-line
           (save-excursion
             (end-of-buffer)
             (beginning-of-line)
             (point))))
      (>= (point) beginning-of-last-line)))

  :general
  ("M--" 'not-modified
   "M-b" 'ps/save-and-revert-buffer
   "M-e" 'ps/eval-region-or-buffer
   "H-q" 'ps/kill-this-buffer
   "A-H-M-s-q" 'ps/kill-this-buffer-switch-to-other-window
   "A-H-q" 'ps/kill-other-buffer
   "H-n" 'ps/new-empty-buffer
   "H-N" 'ps/new-buffer-in-current-mode
   "H-a" 'mark-whole-buffer
   "H-s" 'save-buffer
   "H-S" 'ps/save-all-buffers
   "A-H-M-s-SPC" 'ps/switch-to-alternate-buffer
   "C-b" 'clone-indirect-buffer-other-window
   "H-C-g" 'abort-recursive-edit
   "H-C-S-g" 'top-level
   "H-C-A-g" 'keyboard-escape-quit) ; ESC ESC ESC
  ((messages-buffer-mode-map telega-root-mode-map)
   "q" 'bury-buffer)
  ((ebib-entry-mode-map ebib-index-mode-map)
   "H-q" 'bury-buffer
   "q" 'ps/bury-buffer-switch-to-other-window)
  ((apropos-mode-map calendar-mode-map completion-list-mode-map dired-mode-map Info-mode-map finder-mode-map ledger-reconcile-mode-map pass-mode-map slack-message-buffer-mode-map slack-thread-message-buffer-mode-map special-mode-map telega-msg-button-map tetris-mode-map view-mode-map w3m-mode-map)
   "q" 'ps/kill-this-buffer)
  ((dired-mode-map pdf-view-mode-map)
   "s-o" 'ps/ocr-pdf)
  ;; We typically enter these modes to lookup some information and
  ;; then return to the previous buffer, so we set `q' to switch to
  ;; the other window, and reserve `Q' for the normal behavior
  ((help-mode-map helpful-mode-map osx-dictionary-mode-map)
   "Q" 'ps/kill-this-buffer
   "q" 'ps/kill-this-buffer-switch-to-other-window)
  ((telega-chat-mode-map)
   "s-q" 'ps/kill-this-buffer))

(use-package f)

(use-feature locate
  :custom
  (locate-command "mdfind" "use the OSX Spotlight backend"))

(use-feature autorevert
  :custom
  (auto-revert-use-notify nil "reddit.com/r/emacs/comments/mq2znn/comment/gugo0n4/")
  :config
  (global-auto-revert-mode 1))

(use-feature dired
  :custom
  (dired-listing-switches
   "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-no-confirm t "never ask for confirmation")
  (dired-dwim-target t "if Dired buffer in other window, use that buffer's current directory as target")
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-guess-shell-alist-user '(("" "open")))

  :config
  (setq dired-deletion-confirmer '(lambda (x) t))
  (dired-async-mode)
  (put 'dired-find-alternate-file 'disabled nil) ; do not disable dired-find-alternate-file!

  (defun ps/dired-copy-filename-as-kill-sans-extension ()
    "Copy name of file at point excluding its extension."
    (interactive)
    (kill-new (file-name-sans-extension (dired-copy-filename-as-kill))))

  ;; from emacswiki.org/emacs/DiredOmitMode
  (defun ps/dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (defun ps/dired-mark-screenshots ()
    "Mark all screenshot files."
    (interactive)
    (dired-mark-files-regexp "Screenshot [[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\} at [[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}.png"))

  (defun ps/dired-up-directory-reuse ()
    "Like `dired-up-directory, but reuse current buffer."
    (interactive)
    (find-alternate-file ".."))

  (defun ps/dired-copy-filename-as-kill-absolute ()
    "Copy absolute names of marked (or next ARG) files into the kill
ring."
    (interactive)
    (dired-copy-filename-as-kill '(0)))

  (defun ps/dired-copy-to-remote-docs-directory ()
    "Copy marked files to `stafforini.com/docs'. If no files are
marked, copy file at point instead."
    (interactive)
    (dolist (file (dired-get-marked-files nil nil nil t))
      (shell-command (format "scp '%s' 'ab80508@108.167.182.246:/home2/ab80508/public_html/stafforini.com/docs/'" file)))
    (kill-new (concat "https://stafforini.com/docs/" (dired-copy-filename-as-kill))))

  ;; emacs.stackexchange.com/a/30681/32089
  (define-advice dired-clean-up-after-deletion
      (:around (old-fun &rest r) kill-dired-buffer-quietly)
    (define-advice y-or-n-p (:around (old-fun prompt) just-yes)
      (if (or (string-prefix-p "Kill Dired buffer" prompt)
              (string-prefix-p "Kill buffer of" prompt))
          t
        (funcall old-fun prompt)))
    (unwind-protect (apply old-fun r)
      (advice-remove 'y-or-n-p #'y-or-n-p@just-yes)))

  (defun ps/dired-do-delete-fast (&optional arg)
    "Delete all marked (or next ARG) files, without using the
external `trash' utility. This command let's you delete large
numbers of files quickly, at the expense of losing the 'put back'
option."
    (interactive)
    (cl-letf (((symbol-function 'system-move-file-to-trash) nil))
      (dired-do-delete arg)))

  ;; emacs.stackexchange.com/a/60663/32089
  (defun ps/dired-duplicate-this-file ()
    "Duplicate file at point."
    (interactive)
    (let* ((existing-file (dired-get-filename t))
           (existing-file-stem (file-name-sans-extension existing-file))
           (existing-file-extension (file-name-extension existing-file))
           (counter 1)
           (new-file (format "%s[%d].%s" existing-file-stem counter existing-file-extension)))
      (while (file-exists-p new-file)
        (setq counter (1+ counter)
              new-file (format "%s[%d].%s" existing-file-stem counter existing-file-extension)))
      (copy-file existing-file new-file))
    (revert-buffer))

  :hook
  (dired-mode-hook . dired-hide-details-mode) ; hide details by default

  :general
  (dired-mode-map
   "<tab>" 'ps/dired-subtree-toggle
   "," 'ps/dired-up-directory-reuse
   ";" 'dired-do-rename
   "-" 'dired-hide-details-mode
   "." 'dired-find-alternate-file
   "'" 'ps/dired-copy-filename-as-kill-absolute
   "H-." 'ps/dired-dotfiles-toggle
   "c" 'ps/dired-copy-filename-as-kill-absolute
   "C" 'dired-do-copy
   "C-s" 'dired-isearch-filenames
   "J" 'dired-jump-other-window
   "k" 'dired-previous-line
   "l" 'dired-next-line
   "r" 'dired-toggle-read-only
   "W" 'ps/dired-copy-filename-as-kill-sans-extension
   "z" 'ps/dired-mark-screenshots
   "H-z" 'dired-undo
   "s-d" 'ps/dired-do-delete-fast
   "s-r" 'ps/dired-copy-to-remote-docs-directory
   "A-C-s-," 'dired-prev-dirline
   "A-C-s-." 'dired-next-dirline
   "A-C-s-r" 'dired-prev-marked-file
   "A-C-s-f" 'dired-next-marked-file)
  (pdf-annot-minor-mode-map
   "x" 'dired-jump))

(use-feature image-dired
  :custom
  (image-dired-main-image-directory "~/Pictures/")

  :config
  (defun ps/image-dired-current-directory ()
    "Run `image-dired' in the current directory."
    (interactive)
    (image-dired (dired-current-directory))
    (image-dired-display-thumbnail-original-image))

  :general
  (image-dired-thumbnail-mode-map
   "k" 'image-dired-display-previous-thumbnail-original
   "l" 'image-dired-display-next-thumbnail-original)
  (dired-mode-map
   "I" 'ps/image-dired-current-directory))

(use-package all-the-icons-dired
  :demand t
  :after (all-the-icons dired)
  :custom
  (all-the-icons-dired-monochrome nil)
  :config
  (defun ps/all-the-icons-dired-mode-activate ()
    "Define conditions for activation of `all-the-icons-dired-mode'."
    (if (< (length (directory-files default-directory)) 1000)
        (all-the-icons-dired-mode)
      (all-the-icons-dired-mode -1)))

  :hook
  (dired-mode-hook . ps/all-the-icons-dired-mode-activate))

(use-feature wdired
  :custom
  (wdired-allow-to-change-permissions t)
  :general
  (wdired-mode-map
   "s-c" 'wdired-finish-edit
   "<return>" 'wdired-finish-edit))

(use-feature gnus-dired
  :after dired
  :custom
  (gnus-dired-mail-mode 'mu4e-user-agent)
  :config
  ;; replaces `gnus-dired-mail-buffers' function so it works on
  ;; `message-mode' derived modes, such as `mu4e-compose-mode'
  (defun ps/gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (advice-add 'gnus-dired-mail-buffers :override #'ps/gnus-dired-mail-buffers)

  :hook
  (dired-mode-hook . turn-on-gnus-dired-mode)

  :general
  (dired-mode-map
   "s-a" 'gnus-dired-attach))

(use-feature dired-x
  :after dired
  :demand t
  :config
  ;; (require 'dired-x)
  (setq dired-omit-verbose nil)    ; shut up
  (setq dired-omit-size-limit nil) ; always omit, regardless of directory size
  (setq dired-omit-files
        (concat dired-omit-files "\\|^.localized$\\|^\\.DS_Store$\\|^\\.pdf-view-restore\\|^Icon\\\015"))
  :hook
  (dired-mode-hook . dired-omit-mode)

  :general
  (dired-mode-map
   "–" 'dired-omit-mode))

(use-package dired-subtree
  :after dired
  :config
  (advice-add 'dired-subtree-toggle :after (lambda () (dired-omit-mode) (dired-omit-mode)))
  (advice-add 'dired-subtree-cycle :after (lambda () (dired-omit-mode) (dired-omit-mode)))
  :general
  (dired-mode-map
   "<tab>" 'dired-subtree-toggle
   "<backtab>" 'dired-subtree-cycle))

(use-package dired-quick-sort
  :after dired
  ;; :config
  ;; (dired-quick-sort-setup)
  :general
  (dired-mode-map
   "T" 'hydra-dired-quick-sort/body))

(use-package dired-du
  :config
  (defun ps/dired-du-toggle ()
    "Toggle `dired-du-mode' depending on state of
`dired-hide-details-mode'."
    (if dired-hide-details-mode
        (dired-du-mode -1)
      (dired-du-mode)))
  (advice-add 'dired-hide-details-mode :after 'ps/dired-du-toggle)
  )

(use-feature minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  :config
  ;; superuser.com/a/132454/387888
  (defun ps/switch-to-minibuffer-window ()
    "Switch to minibuffer window (if active)"
    (interactive)
    (when (active-minibuffer-window)
      (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
      (select-window (active-minibuffer-window))))

  :hook
  (minibuffer-setup-hook . yas-minor-mode)

  :general
  ("A-C-H-0" 'ps/switch-to-minibuffer-window
   "C-A-g" 'exit-minibuffer)
  ((minibuffer-mode-map)
   "M-n" nil
   "M-p" nil))

(use-feature ibuffer
  :general
  (ibuffer-mode-map
   "k" 'ibuffer-do-delete))

(use-feature filenotify
  :config
  ;; Copied from lists.gnu.org/archive/html/emacs-devel/2021-10/msg01206.html
  (defun ps/file-notify-rm-all-watches ()
    "Remove all existing file notification watches from Emacs."
    (interactive)
    (maphash
     (lambda (key _value)
       (file-notify-rm-watch key))
     file-notify-descriptors)))

(use-package uniquify-files
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package reveal-in-osx-finder
  :defer 10
  :general
  (dired-mode-map
   "/" 'reveal-in-osx-finder))

(use-feature tramp
  :custom
  ;; Disable version control on tramp buffers to avoid freezes.
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp))

  ;; Don't clean up recentf tramp buffers.
  (recentf-auto-cleanup 'never)

  ;; This is supposedly [[https://www.emacswiki.org/emacs/TrampMode][faster than the default]], `scp'.
  (tramp-default-method "sshx")

  ;; SSH controlmaster settings are set in =~/.ssh/config=.
  (tramp-use-ssh-controlmaster-options nil)

  ;; Store TRAMP auto-save files locally.
  (tramp-auto-save-directory ps/dir-emacs-var)

  ;; A more representative name for this file.
  (tramp-persistency-file-name (file-name-concat tramp-auto-save-directory "tramp-connection-history"))

  ;; Cache SSH passwords during the whole Emacs session.
  (password-cache-expiry nil)

  ;; emacs.stackexchange.com/a/37855/32089
  (remote-file-name-inhibit-cache nil)

  :config
  ;; Reuse SSH connections. Taken from the TRAMP FAQ.
  (customize-set-variable 'tramp-ssh-controlmaster-options
                          (concat
                           "-o ControlPath=/tmp/ssh-tramp-%%r@%%h:%%p "
                           "-o ControlMaster=auto -o ControlPersist=yes"))

  ;; This will put in effect PATH changes in the remote ~/.profile.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory 'no-identification) ad-do-it)))

(use-package pandoc-mode
  :general
  ("A-p" 'pandoc-main-hydra/body))

(use-package curl-to-elisp)

(use-feature window
  :demand t

  :custom
  (split-width-threshold 300)
  (split-height-threshold nil)
  (frame-resize-pixelwise t) ; github.com/d12frosted/homebrew-emacs-plus#no-titlebar
  (scroll-error-top-bottom t "move point to top of buffer if `scroll-down-command' invoked when screen can scroll no further")
  ;; The following prevents Emacs from splitting windows indefinitely when the monitor config changes
  ;; stackoverflow.com/questions/23207958/how-to-prevent-emacs-dired-from-splitting-frame-into-more-than-two-windows

  :config
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'display-buffer-alist `(,shell-command-buffer-name-async display-buffer-no-window))
  (defvar ps/split-emacs-chrome-sideways nil)
  (defun ps/split-emacs-chrome-sideways ()
    "Split Emacs and Chrome frames sideways. A second
  invocation restores original layout.

  Calls Keyboard Maestro macro via shell script."
    (interactive)
    (toggle-frame-maximized)
    (if ps/split-emacs-chrome-sideways
        (progn
          (winner-undo)
          (ps/switch-to-last-window)
          ;; Maximize Chrome
          (eshell-command "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"DBA06CC2-86E4-4F6B-9FA6-C1C12C007BCC\"'"))
      (progn
        (delete-other-windows)
        (set-frame-size nil 950 1080 t)
        ;; Split Chrome left
        (eshell-command "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"CF26FBCF-93C1-4C5D-9038-9AB31F6C6903\"'")))
    (setq ps/split-emacs-chrome-sideways
          (not ps/split-emacs-chrome-sideways)))

  ;; This function is no longer used. Consider deleting.
  (defun ps/set-split-window-margins ()
    "Set window margins for split screen."
    (unless writeroom-mode
      (cond ((eq (winum-get-number) 1)
             (set-window-margins (selected-window) 20 0))
            ((eq (winum-get-number) 2)
             (set-window-margins (selected-window) 0 20)))))

  (defun ps/get-last-window ()
    "Get to previously selected ordinary or minibuffer window."
    (interactive)
    (if (and (active-minibuffer-window) (not (minibufferp)))
        (select-window (active-minibuffer-window))
      (get-mru-window nil nil t)))

  (defun ps/switch-to-last-window ()
    "Switch to previously selected ordinary or minibuffer window."
    (interactive)
    (let ((last-window (ps/get-last-window)))
      (select-frame-set-input-focus (window-frame last-window))
      (select-window last-window)))

  ;; Modified from endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  (defun ps/narrow-or-widen-dwim ()
    "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, ledger
  transaction, or defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
    (interactive)
    (declare (interactive-only))
    (cond ((buffer-narrowed-p) (ps/org-widen-and-reveal))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-narrow-to-block) t))
                 (t (ps/org-narrow-to-entry-and-children))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          ((derived-mode-p 'ledger-mode)
           (ps/ledger-narrow-to-xact))
          (t (narrow-to-defun))))

  (defun ps/count-visible-buffers (&optional frame)
    "Count the number of buffers currently being shown. Defaults to
selected frame."
    (length (mapcar #'window-buffer (window-list frame))))

  (defun ps/window-split-if-unsplit ()
    "Split windows when frame is unsplit. Split in three windows if
`frame-width' is greater than `ps/frame-width-threshold',
otherwise in two windows."
    (interactive)
    (when (= (length (window-list)) 1)
      (split-window-right))
    (when (> (frame-width) ps/frame-width-threshold)
      (when (= (length (window-list)) 2)
        (split-window-right))
      (balance-windows)))

  (run-with-timer 5 nil 'ps/window-split-if-unsplit)

  (defun ps/window--move-or-swap (this-buffer other-buffer &optional target-window)
    "docstring"
    (ps/window-split-if-unsplit)
    (let* ((target-window (if (or (not target-window)
                                  (eq (ps/get-last-window) target-window)
                                  (eq (selected-window) target-window))
                              (ps/get-last-window)
                            target-window))
           (source-window (if (eq (selected-window) target-window)
                              (ps/get-last-window)
                            (selected-window))))
      (set-window-buffer target-window this-buffer)
      (set-window-buffer source-window other-buffer)
      (select-window target-window)))

  (defun ps/window-buffer-swap ()
    "Swap the current buffer and the buffer in the other
window. If there is only one window, create a second one. If frame
is wide enough, create a third."
    (interactive)
    (ps/window--move-or-swap
     (window-buffer)
     (window-buffer (ps/get-last-window))))

  (defun ps/window-buffer-move (&optional target-window)
    "Move the current buffer to the other window. If there is only one
window, create a second one. If frame is wide enough, create a third."
    (interactive)
    (ps/window--move-or-swap
     (window-buffer)
     (ps/get-alternate-buffer)
     target-window))

  (defun ps/window-buffer-move-right ()
    "docstring."
    (interactive)
    (ps/window-buffer-move
     (winum-get-window-by-number
      (1+
       (mod
        (winum-get-number)
        (count-windows))))))

  (defun ps/window-buffer-move-left ()
    "docstring."
    (interactive)
    (ps/window-buffer-move
     (winum-get-window-by-number
      (1+
       (mod
        (count-windows)
        (winum-get-number))))))

  (defun ps/window-buffer-move-dwim ()
    "Based on frame size, create one or two additional windows if
necessary, and move buffer to the other window or to the middle
window depending on the number of present windows."
    (interactive)
    (ps/window-buffer-move (when (> (count-windows) 2) (winum-get-window-by-number 2))))

  :general
  ("C-H-0" 'ps/switch-to-last-window
   "C-w" 'ps/narrow-or-widen-dwim
   "H-w" 'delete-window
   "H-W" 'delete-other-windows
   "M-A-q" 'delete-frame
   "s-A-," 'ps/split-emacs-chrome-sideways
   "M-–" 'ps/window-buffer-move-dwim ; `emacs-mac'
   "M--" 'ps/window-buffer-move-dwim ; `emacs-plus'
   "M-," 'ps/window-buffer-move-left
   "M-." 'ps/window-buffer-move-right
   "A-M--" 'ps/window-buffer-swap ; `emacs-mac'
   "A-M-–" 'ps/window-buffer-swap ; `emacs-plus'
   "A-C-s-y" 'scroll-down-command
   "A-C-s-h" 'scroll-up-command
   "A-C-s-g" 'scroll-other-window
   "A-C-s-t" 'scroll-other-window-down
   "A-C-s-x" (lambda! (scroll-down-line 4))
   "A-C-s-c" (lambda! (scroll-up-line 4))
   "A-C-s-v" (lambda! (scroll-down-line 16))
   "A-C-s-b" (lambda! (scroll-up-line 16)))
  ((elfeed-show-mode-map eww-mode-map helpful-mode-map mu4e-view-mode-map telega-msg-button-map)
   "y" 'scroll-down-command
   "h" 'scroll-up-command)
  (isearch-mode-map
   "C-w" 'ps/narrow-or-widen-dwim))

(use-feature frame
  :config
  (blink-cursor-mode)

  :general
  ("H-S-SPC" 'other-frame))

(use-package winum
  :demand t
  :custom
  (winum-scope 'frame-local)
  :config
  (winum-mode)
  :general
  ("C-," 'winum-select-window-1
   "C-." 'winum-select-window-2
   "C-/" 'winum-select-window-3
   "H-4" 'winum-select-window-4
   "H-5" 'winum-select-window-5
   "H-6" 'winum-select-window-6
   "H-7" 'winum-select-window-7
   "H-8" 'winum-select-window-8
   "H-9" 'winum-select-window-9
   "H-0" 'winum-select-window-10))

(use-feature winner
  :demand t

  :custom
  (winner-mode 1)

  :config
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)

  :general
  ("H-A-w" 'winner-undo
   "H-A-W" 'winner-redo))

(use-feature scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package avy
  :commands ps/avy-dired-find-file

  :custom
  (avy-case-fold-search nil)
  (avy-timeout-seconds 0.2)
  (avy-all-windows nil)
  (avy-keys '(97 115 100 102 106 107 108 13 32 113 119 101 114 117 105 111 112 122 120 99 118 109 44 46 47))

  :config
  ;; Launch dispatcher with `/' rather than `?'
  (defun ps/avy-handler-default (char)
    "The default handler for a bad CHAR."
    (let (dispatch)
      (cond ((setq dispatch (assoc char avy-dispatch-alist))
             (unless (eq avy-style 'words)
               (setq avy-action (cdr dispatch)))
             (throw 'done 'restart))
            ((memq char avy-escape-chars)
             ;; exit silently
             (throw 'done 'abort))
            ((eq char ?/)
             (avy-show-dispatch-help)
             (throw 'done 'restart))
            ((mouse-event-p char)
             (signal 'user-error (list "Mouse event not handled" char)))
            (t
             (message "No such candidate: %s, hit `C-g' to quit."
                      (if (characterp char) (string char) char))))))
  (advice-add 'avy-handler-default :override #'ps/avy-handler-default)

  (defun ps/avy-goto-word-in-line ()
    "Jump to a word start between start and end of visual line."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil
                       (save-excursion (beginning-of-visual-line))
                       (save-excursion (end-of-visual-line) (point)))))

  (defun ps/avy-goto-word-in-line-behind ()
    "Jump to a word start between start of visual line and point."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil
                       (save-excursion (beginning-of-visual-line))
                       (point))))

  (defun ps/avy-goto-word-in-line-ahead ()
    "Jump to a word start between point and end of visual line."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil
                       (point)
                       (save-excursion (end-of-visual-line) (point)))))

  (defun ps/avy-goto-line-then-word-above ()
    "Goto visible line above point, then to word behind point."
    (interactive)
    (avy-goto-line-above)
    (ps/avy-goto-word-in-line))

  (defun ps/avy-goto-line-then-word-below ()
    "Goto visible line below point, then to word ahead of point."
    (interactive)
    (avy-goto-line-below)
    (ps/avy-goto-word-in-line))

  (defun ps/avy-goto-end-of-line-above (&optional offset bottom-up)
    "Goto visible end of line above the cursor."
    (interactive)
    (call-interactively 'avy-goto-line-above)
    (end-of-line))

  (defun ps/avy-goto-end-of-line-below (&optional offset bottom-up)
    "Goto visible end of line below the cursor."
    (interactive)
    (call-interactively 'avy-goto-line-below)
    (end-of-line))

  (defun ps/avy-goto-line-above-then-enter ()
    "Go to visible line above point, then hit `return'."
    (interactive)
    (avy-goto-line-above)
    (execute-kbd-macro (read-kbd-macro "<return>")))

  (defun ps/avy-goto-line-below-then-enter ()
    "Go to visible line below point, then hit `return'."
    (interactive)
    (avy-goto-line-below)
    (execute-kbd-macro (read-kbd-macro "<return>")))

  (defun ps/avy-dired-find-file ()
    "In Dired, visit the file or directory in selected line."
    (interactive)
    (avy-goto-line)
    (dired-find-alternate-file))

    (defun ps/avy-ebib-view-entry ()
    "In Ebib, view the entry in selected line."
    (interactive)
    (avy-goto-line)
    (ebib-edit-entry))

  (defun ps/mu4e-headers-view-message ()
    "In mu4e, view the message in selected line."
    (interactive)
    (avy-goto-line)
    (mu4e-headers-view-message))

  (defun ps/avy-telega-view-message ()
    "In Telega, view the message in selected line."
    (interactive)
    (avy-goto-line)
    (push-button)) ; not sure what the actual command to open a chat is

  (defun ps/avy-elfeed-search-show-entry ()
    "In Elfeed, display the item in selected line."
    (interactive)
    (avy-goto-line)
    (call-interactively 'elfeed-search-show-entry))

  ;; karthinks.com/software/avy-can-do-anything/#mark-the-region-from-point-to-a-candidate
  (defun ps/avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?r avy-dispatch-alist) 'ps/avy-action-mark-to-char)

  :general
  ("C-H-s-u" 'ps/avy-goto-word-in-line-behind
   "C-H-s-p" 'ps/avy-goto-word-in-line-ahead
   "C-H-s-m" 'avy-goto-line-above
   "C-H-s-," 'ps/avy-goto-end-of-line-above
   "C-H-s-." 'avy-goto-line-below
   "C-H-s-/" 'ps/avy-goto-end-of-line-below
   "C-H-s-k" 'avy-goto-word-1-above
   "C-H-s-l" 'avy-goto-word-1-below)
  (dired-mode-map
   "f" 'ps/avy-dired-find-file)
   (ebib-index-mode-map
   "f" 'ps/avy-ebib-view-entry)
  (telega-root-mode-map
   "f" 'ps/avy-telega-view-message)
   (ebib-entry-mode-map
   "f" 'avy-goto-line))

(use-package iy-go-to-char
  :config
  (defun ps/avy-goto-line-then-word-then-char-above ()
    "Go to visible line below point, then to word ahead of point,
then to selected character immediately ahead of point."
    (interactive)
    (ps/avy-goto-line-then-word-above)
    (call-interactively 'iy-go-to-char))

  (defun ps/avy-goto-line-then-word-then-char-below ()
    "Go to visible line below point, then to word ahead of point,
then to selected character immediately ahead of point."
    (interactive)
    (ps/avy-goto-line-then-word-below)
    (call-interactively 'iy-go-to-char))

  :general
  ("C-H-s-j" 'iy-go-to-char-backward
   "C-H-s-;" 'iy-go-to-char
   "C-H-s-i" 'ps/avy-goto-line-then-word-then-char-above
   "C-H-s-o" 'ps/avy-goto-line-then-word-then-char-below))

(use-package writeroom-mode
  :defer 5
  :init
  ;; This neds to be delayed a few seconds because the value returned
  ;; by `window-total-width' changes as Emacs restarts.
  (run-with-timer 10 nil (lambda () (setq writeroom-width (window-total-width))))

  (defun ps/writerrom-global-effects (arg)
    "Enable and disable custom effects when `writeroom-mode' is
activated (`arg' = 1) and deactivated (`arg' = -1). The function
needs to be included as an element in the list defined by
`writeroom-global-effects'."
    (tab-bar-mode (* -1 arg)))

  :custom
  (writeroom-global-effects '(writeroom-set-fullscreen
                              writeroom-set-alpha
                              writeroom-set-menu-bar-lines
                              writeroom-set-tool-bar-lines
                              writeroom-set-vertical-scroll-bars
                              writeroom-set-bottom-divider-width
                              ps/writerrom-global-effects))

  (writeroom-restore-window-config t "upon leaving `writeroom mode', restore pre-existing number of windows")
  (writeroom-major-modes '(org-mode
                           elfeed-search-mode
                           elfeed-show-mode
                           eww-mode
                           eww-buffers-mode) "major modes activated in global-writeroom-mode")
  (writeroom-fullscreen-effect 'maximized "disables annoying fullscreen transition effect on MacOS")
  (writeroom-maximize-window t)

  :general
  ("M-'" 'writeroom-mode
   "M-A-'" 'global-writeroom-mode))

(use-package ace-link
  :defer 5
  :config
  (defun ps/ace-link-w3m-externally ()
    "Open a visible link in a `w3m-mode' buffer externally."
    (interactive)
    (let ((pt (avy-with ace-link-w3m
                (avy-process
                 (mapcar #'cdr (ace-link--w3m-collect))
                 (avy--style-fn avy-style)))))
      (when (numberp pt)
        (goto-char pt)
        ;; not sure why this elaborate disjunction is needed; adapted from
        ;; github.com/alphapapa/pocket-reader.el/blob/master/pocket-reader.el
        (let ((url (or (get-text-property (point) 'w3m-href-anchor)
                       (unless (bolp)l
                         (save-excursion
                           (get-text-property (1- (point)) 'w3m-href-anchor)))
                       (unless (eolp)
                         (save-excursion
                           (get-text-property (1+ (point)) 'w3m-href-anchor)))
                       (thing-at-point-url-at-point))))
          (w3m-view-url-with-browse-url url)))))

  (defun ps/ace-link-org-agenda-clock-in ()
    "Open a visible link in an `org-mode-agenda' buffer
and start clock."
    (interactive)
    (ace-link-org-agenda)
    (org-clock-in))

  (defun ps/ace-link-eww-externally ()
    "Browse URL using `browse-url-secondary-browser-function'"
    (interactive)
    (ace-link-eww '(4)))

  (defun ps/ace-link-eww-new-buffer ()
    "Browse URL in new buffer."
    (interactive)
    (ace-link-eww '(16)))

  (defun ps/ace-link-w3m-in-eww ()
    "In an `w3m-mode' buffer, open a visible link in `eww'."
    (interactive)
    (require 'w3m)
    (let ((pt (avy-with ace-link-w3m
                (avy-process
                 (mapcar #'cdr (ace-link--w3m-collect))
                 (avy--style-fn avy-style)))))
      (goto-char pt)
      (eww (w3m-url-valid (w3m-anchor)))))

  :general
  ((eww-mode-map elfeed-show-mode-map)
   "f" 'ace-link-eww
   "F" 'ps/ace-link-eww-externally
   "s-f" 'ps/ace-link-eww-new-buffer)
  ((help-mode-map helpful-mode-map elisp-refs-mode-map)
   "f" 'ace-link-help)
  ((Info-mode-map)
   "f" 'ace-link-info)
  ((Man-mode-map woman-mode-map)
   "f" 'ace-link-woman)
  (mu4e-view-mode-map
   "f" 'ps/ace-link-w3m-in-eww
   "F" 'ps/ace-link-w3m-externally)
  (org-agenda-mode-map
   "f" 'ace-link-org-agenda
   "s-f" 'ps/ace-link-org-agenda-clock-in)
  ((org-mode-map telega-chat-mode-map)
   "M-f" 'ace-link-org)
  ((slack-message-buffer-mode-map telega-msg-button-map twittering-mode-map)
   "f" 'ace-link-org))

(use-feature calendar
  :custom
  (calendar-week-start-day 1)    ; week starts on Monday
  (calendar-set-date-style 'iso) ; this isn't the default?
  (calendar-time-display-form
   '(24-hours ":" minutes
              (when time-zone
                (concat " (" time-zone ")"))))
  (calendar-mark-holidays-flag nil)
  (calendar-time-zone-style 'numeric)
  (holiday-bahai-holidays nil)

  :config
  (cond ((equal (system-name) ps/computer-hostname-pablo)
         (setq calendar-location-name "Ciudad de Buenos Aires, Argentina")
         (setq calendar-latitude -34.58194921101665)
         (setq calendar-longitude -58.41290172678564))
        ((equal (system-name) ps/computer-hostname-leo)
         (setq calendar-location-name "Barcelona")
         (setq calendar-latitude 41.3874)
         (setq calendar-longitude 2.1686)))
  ;; To copy the current latitude and longitude, go to Google Maps,
  ;; right-click on your location, and select the coordinates.

  ;; Adapted from Prot
  (defcustom ps/date-specifier "%F"
    "Date specifier for `format-time-string'.
Used by `ps/insert-date'."
    :type 'string)

  (defcustom ps/time-specifier "%R %z"
    "Time specifier for `format-time-string'.
Used by `ps/insert-date'."
    :type 'string)

  (defun ps/insert-date (&optional arg)
    "Insert the current date as `ps/date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `ps/time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
    (interactive "P")
    (let* ((date ps/date-specifier)
           (time ps/time-specifier)
           (format (if arg (format "%s %s" date time) date)))
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert (format-time-string format))))

  :general
  ("C-d" 'calendar
   "A-s-=" 'ps/insert-date
   "s-=" "C-u A-s-=")
  (calendar-mode-map
   "H-m" 'calendar-set-mark
   "A-C-s-u" 'calendar-backward-day
   "A-C-s-i" 'calendar-backward-week
   "A-C-s-o" 'calendar-forward-week
   "A-C-s-p" 'calendar-forward-day
   "A-C-s-m" 'calendar-backward-month
   "A-C-s-," 'calendar-backward-year
   "A-C-s-." 'calendar-forward-year
   "A-C-s-/" 'calendar-forward-month
   "C-f" nil
   "C-b" nil
   "C-n" nil
   "C-p" nil
   "=" 'calendar-count-days-region))

(use-feature loaddefs
  :disabled
  :init
  (dolist (holiday '((holiday-float 6 0 3 "Father's Day")
                     (holiday-float 5 0 2 "Mother's Day"))
                   (delete holiday holiday-general-holidays))))

(use-package org-gcal
  :if (equal (system-name) ps/computer-hostname-pablo)
  :straight (org-gcal :type git :host github :repo "kidd/org-gcal.el")
  :after auth-source-pass
  :defer 10

  :custom
  (org-gcal-client-id (auth-source-pass-get "host" "auth-sources/org-gcal"))
  (org-gcal-client-secret (auth-source-pass-get 'secret "auth-sources/org-gcal"))
  (org-gcal-reload-client-id-secret)
  (org-gcal-fetch-file-alist `((,ps/personal-gmail . ,ps/file-calendar)))
  (org-gcal-recurring-events-mode 'top-level)
  (org-gcal-remove-api-cancelled-events nil) ; never remove cancelled events
  (org-gcal-notify-p nil)
  (org-gcal-auto-archive nil)
  (org-gcal-up-days 1)
  (org-gcal-down-days 3)

  :config
  (defun ps/org-gcal--get-time-and-desc ()
    "Get the timestamp and description of the event at point.

  Return a plist with :start, :end, and :desc keys. The value for a key is nil if
  not present."
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
      ;; Prefer to read event time from the SCHEDULE property if present.
      (setq tobj (or (org-element-property :deadline elem) tobj))
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

  (defun ps/org-gcal--update-entry (calendar-id event &optional update-mode)
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
    (let* ((smry  (or (plist-get event :summary)
                      "busy"))
           (desc  (plist-get event :description))
           (loc   (plist-get event :location))
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
      (org-edit-headline smry)
      (org-entry-put (point) org-gcal-etag-property etag)
      (when recurrence (org-entry-put (point) "recurrence" (format "%s" recurrence)))
      (when loc (org-entry-put (point) "LOCATION" loc))
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
      ;; (newline)
      ;; (insert (format ":%s:" org-gcal-drawer-name))
      ;; (newline)
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
        (if (org-element-property :deadline elem)
            (unless (and recurrence old-start) (org-deadline nil timestamp))
          (org-deadline nil timestamp)
          (newline)
          (when desc (newline))))
      ;; Insert event description if present.
      (when desc
        (insert (replace-regexp-in-string "^\*" "✱" desc))
        (insert (if (string= "\n" (org-gcal--safe-substring desc -1)) "" "\n")))
      ;; (insert ":END:")
      (when (org-gcal--event-cancelled-p event)
        (save-excursion
          (org-back-to-heading t)
          (org-gcal--handle-cancelled-entry)))
      (when update-mode
        (cl-dolist (f org-gcal-after-update-entry-functions)
          (save-excursion
            (org-back-to-heading t)
            (funcall f calendar-id event update-mode))))))

  ;; I replace the two native functions with the slightly tweaked
  ;; versions under `:init' so that `org-gcal' uses the `DEADLINE'
  ;; property for all timestamps.
  (advice-add 'org-gcal--get-time-and-desc :override #'ps/org-gcal--get-time-and-desc)
  (advice-add 'org-gcal--update-entry :override #'ps/org-gcal--update-entry)

  (defun ps/org-gcal-open-at-point ()
    "Get entry id of `org-gcal' entry at point and open the
associated Google Calendar event in a browser."
    (interactive)
    (if-let ((id (org-entry-get nil "entry-id")))
        (browse-url
         (concat
          "https://calendar.google.com/calendar/u/0/r/eventedit/"
          (replace-regexp-in-string
           "\n"
           ""
           (base64-encode-string
            (replace-regexp-in-string
             "/"
             " "
             id))))))
    (user-error "No id found."))


  ;; (advice-add 'org-gcal-sync :before (lambda () (setq message-log-max 10000)))
  ;; (advice-add 'org-gcal-sync-buffer :before (lambda () (setq message-log-max 10000)))

  (defhydra hydra-org-gcal (:exit t :hint nil)
    "
_f_etch all       |_s_ync all        |_p_ost at point   |_d_elete at point |_r_equest token    |_t_oggle debug
_F_etch buffer    |_S_ync buffer     |_o_pen at point   |_u_nlock sync     |_c_lear token      |re_l_oad secret  "
    ("f" org-gcal-fetch)
    ("F" org-gcal-fetch-buffer)
    ("s" org-gcal-sync)
    ("S" org-gcal-sync-buffer)
    ("p" org-gcal-post-at-point)
    ("d" org-gcal-delete-at-point)
    ("o" ps/org-gcal-open-at-point)
    ("u" org-gcal--sync-unlock)
    ("r" org-gcal-request-token)
    ("c" org-gcal-sync-tokens-clear)
    ("t" org-gcal-toggle-debug)
    ("l" org-gcal-reload-client-id-secret))

  :general
  (org-mode-map
   "s-g" 'hydra-org-gcal/body))

(use-feature time
  :after tab-bar
  :demand t
  :custom
  (world-clock-list '(("Europe/Barcelona" "Barcelona")
                      ("Europe/London" "London")
                      ("America/Buenos_Aires" "Buenos Aires")
                      ("America/Nassau" "Nassau")
                      ("America/New_York" "New York")
                      ("America/Los_Angeles" "San Francisco")))
  (display-time-format "%a %e %b %T")
  (display-time-interval 1)
  (display-time-default-load-average nil)

  :config
  (display-time-mode)

  (defun ps/time-last-day-of-last-month ()
  "Insert the last day of the most recent month."
  (interactive)
  (let* ((date (calendar-current-date))
         (year (calendar-extract-year date))
         (month (- (calendar-extract-month date) 1))
         (day (calendar-last-day-of-month month year)))
    (insert (format-time-string
             "%Y-%m-%d"
             (encode-time 0 0 0 day month year)))))

  ;; github.com/arunkmv/.config/tree/main/emacs#tab-bar
  (defface ps/display-time
    '((t (:inherit bold)))
    "Face for `display-time-string' in `global-mode-string'")

  :general
  ("M-A-t" 'world-clock))

(use-package tmr
  :defer 10)

(use-package
  :disabled
  :if (equal (system-name) ps/computer-hostname-pablo)
  :straight (hammy
             :host github
             :repo "alphapapa/hammy.el")

  :config
  (hammy-define "Move"
    :documentation "Don't forget to stretch your legs."
    :intervals
    ;; A list of intervals, each defined with the `interval' function.
    (list (interval
           ;; The name of the interval is a string, used when selecting
           ;; hammys and shown in the mode line.
           :name "💺"
           ;; The duration of the interval: a number of seconds, a string
           ;; passed to `timer-duration', or a function which returns such.
           :duration "45 minutes"
           ;; Optionally, a face in which to show the
           ;; interval's name in the mode line.
           :face 'font-lock-type-face
           ;; A list of actions to take before starting the interval
           ;; (really, one or a list of functions to call with the hammy
           ;; as the argument).  The `do' macro expands to a lambda,
           ;; which the interval's `before' slot is set to.  In its
           ;; body, we call two built-in helper functions.
           :before (do (announce "Whew!")
                       (notify "Whew!"))
           ;; We want this interval to not automatically advance to the
           ;; next one; rather, we want the user to call the
           ;; `hammy-next' command to indicate when the standing-up is
           ;; actually happening.  So we provide a list of actions to
           ;; take when it's time to advance to the next interval.  We
           ;; wrap the list in a call to the built-in `remind' function,
           ;; which causes the actions to be repeated every 10 minutes
           ;; until the user manually advances to the next interval.
           :advance (remind "10 minutes"
                            ;; Every 10 minutes, while the hammy is waiting
                            ;; to be advanced to the next interval, remind
                            ;; the user by doing these things:
                            (do (announce "Time to stretch your legs!")
                                (notify "Time to stretch your legs!")
                              (play-sound-file "~/Misc/Sounds/mooove-it.wav"))))
          (interval :name "🤸"
                    :duration "5 minutes"
                    :face 'font-lock-builtin-face
                    :before (do (announce "Mooove it!")
                                (notify "Mooove it!"))
                    ;; Again, the interval should not advance automatically
                    ;; to the next--the user should indicate when he's
                    ;; actually sat down again.  (If we omitted the
                    ;; `:advance' slot, the interval would automatically
                    ;; advance when it reached its duration.)
                    :advance (do (announce "Time for a sit-down...")
                                 (notify "Time for a sit-down...")
                               (play-sound-file org-pomodoro-finished-sound)))))

  (hammy-mode))

(use-package display-wttr
  :defer 10
  :custom
  (display-wttr-interval (* 15 60))

  :config
  (cond ((equal (system-name) ps/computer-hostname-pablo)
         (setq display-wttr-locations '("Buenos Aires")))
        ((equal (system-name) ps/computer-hostname-leo)
         (setq display-wttr-locations '("Barcelona"))))

  (display-wttr-mode))

(use-feature simple
  :general
  ((minibuffer-mode-map mu4e-minibuffer-search-query-map)
   "M-k" 'previous-history-element
   "M-l" 'next-history-element))

(use-feature savehist
  :demand t
  :init
  (savehist-mode)

  :custom
  (history-length t "unlimited history")
  (savehist-additional-variables
   '(citar-history
     command-history
     compile-history
     compilation-command
     eww-history
     extended-command-history
     file-name-history
     Info-history-list
     ido-file-history
     kill-ring
     kmacro-ring
     last-kbd-macro
     log-edit-comment-ring
     magit-read-rev-history
     mark-ring
     read-expression-history
     regexp-search-ring
     register-alist
     search-ring
     shell-command-history
     telega-search-history
     twittering-search-history))
  (savehist-save-minibuffer-history t))

(use-feature saveplace
  :config
  (save-place-mode))

(use-package session
  :demand t
  :custom
  (session-globals-include '((kill-ring 100)
                             (session-file-alist 500 t)
                             (file-name-history 10000)
                             search-ring regexp-search-ring))
  (history-length 1000)
  :hook
  (after-init-hook . session-initialize))

(use-feature recentf
  :defer 10
  :custom
  (recentf-max-saved-items 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode))

(use-package persistent-scratch
  :demand t
  :custom
  (persistent-scratch-autosave-interval 30)
  ;; What follows is copied from umarahmad.xyz/blog/quick-scratch-buffers/
  ;; (persistent-scratch-scratch-buffer-p-function 'ps/persistent-scratch-buffer-identifier)

  :config
  (defun ps/persistent-scratch-buffer-identifier ()
    (string-match "^*scratch:" (buffer-name)))

  (defun ps/persistent-scratch-get-scratches ()
    (let ((scratch-buffers)
          (save-data
           (read
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents persistent-scratch-save-file))
              (buffer-string)))))
      (dolist (saved-buffer save-data)
        (push (substring (aref saved-buffer 0) (length "*scratch:")) scratch-buffers))
      scratch-buffers))

  (defun ps/persistent-scratch-quick-open ()
    (interactive)
    (let* ((scratch-buffers (ps/persistent-scratch-get-scratches))
           (chosen-scratch (concat "*scratch:"
                                   (completing-read
                                    "Choose a scratch: "
                                    scratch-buffers nil nil nil nil
                                    (ps/random-alnum 4))))
           (buffer-exists-p (get-buffer chosen-scratch)))
      (pop-to-buffer chosen-scratch)
      (unless buffer-exists-p
        (persistent-scratch-restore-this))
      (persistent-scratch-mode)))

  (persistent-scratch-setup-default))

(use-feature remember
  :custom
  (remember-data-file (file-name-concat ps/dir-emacs "var/remember"))
  (remember-notes-buffer-name "\*scratch\*"))

(use-feature vc
  :custom
  (vc-follow-symlinks t "don't ask for confirmation when opening symlinked file")
  (vc-make-backup-files nil "do not backup version controlled files"))

(use-package magit
  :defer 15
  :custom
  ;; chris.beams.io/posts/git-commit/
  (git-commit-summary-max-length 50)
  (magit-commit-ask-to-stage 'stage)

  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)

  ;; adapted from Sacha Chua
  (defun ps/magit-stage-commit-and-push (message)
    "Stage, commit and push all changes."
    (interactive
     (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
    (when (or
           (magit-anything-staged-p)
           (magit-anything-unstaged-p))
      (magit-stage-modified '(4))
      (magit-commit-create (list "-m" message)))
    (call-interactively #'magit-push-current-to-pushremote))

  (defun ps/magit-stage-commit-and-push-all-repos ()
    "Update all active depositories."
    (dolist (dir ps/dir-all-repos)
      (ps/magit-midnight-update dir)))

  (defun ps/magit-midnight-update (arg)
    "Update repository daily using `midnight'."
    (let ((default-directory arg))
      (ps/magit-stage-commit-and-push "Midnight update")))

  ;; gist.github.com/dotemacs/9a0433341e75e01461c9
  (defun ps/magit-parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))

  (defun ps/magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (ps/magit-parse-url url))
        (message "opening repo %s" url))))

  :general
  ("A-g" 'magit)
  (magit-mode-map
   "o" 'ps/magit-open-repo)
  (magit-status-mode-map
   "A-C-s-r" 'magit-section-backward
   "A-C-s-f" 'magit-section-forward))

(use-package ghub
  :defer 5)

(use-package forge
  :after magit ghub
  :demand t
  :init
  (unless (version<= emacs-version "29.0")
    (setq forge-database-connector 'sqlite-builtin))

  :custom
  (forge-owned-accounts `((,ps/forge-owned-accounts)))

  :general
  (forge-post-mode-map
   "s-c" 'forge-post-submit))

(use-package code-review
  :custom
  (code-review-fill-column 80)
  (code-review-new-buffer-window-strategy #'switch-to-buffer)
  (code-review-auth-login-marker 'forge)

  :hook
  (code-review-mode-hook . emojify-mode))

(use-package projectile
  :disabled
  :config
  (projectile-mode)
  :general
  ("H-p" 'projectile-command-map))

(use-package git-timemachine
  :straight
  (git-timemachine
   :type git
   :host codeberg
   :repo "pidu/git-timemachine")
  :demand t
  :general
  ("A-H-t" 'git-timemachine))

(use-package git-auto-commit-mode
  :demand t
  :config
  (setq-default gac-automatically-push-p nil)
  (setq-default gac-debounce-interval 30)
  (setq-default gac-silent-message-p t)
  (setq-default gac-automatically-add-new-files-p t))

(use-package git-gutter
  :disabled
   :defer 10
   :config
   (global-git-gutter-mode))

(use-feature isearch
  :custom
  (search-default-mode #'char-fold-to-regexp)
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-yank-on-move t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  (isearch-allow-scroll 'unlimited)
  (search-upper-case t)
  (search-exit-option t) ; `t' is the default, but some alternative value may be more sensible

  :config
  (defun ps/isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))

  (defun ps/isearch-copy-match ()
    "Send the first isearch match to the kill ring."
    (interactive)
    (kill-new (buffer-substring (point) isearch-other-end))
    (isearch-done))

  :general
  (isearch-mode-map
   "C-H-M-s" 'isearch-delete-char
   "C-H-M-d" "C-- C-H-M-s" ; delete forward char
   "C-g" 'isearch-abort ; "quit once"
   "C-H-g" 'isearch-exit ; "quit twice"
   "C-'" 'isearch-toggle-char-fold
   "C-," 'isearch-forward-symbol-at-point
   "C-." 'isearch-forward-thing-at-point
   "C-/" 'isearch-complete
   "C-<return>" 'ps/isearch-exit-other-end
   "H-m" 'isearch-toggle-lax-whitespace
   "C-a" 'isearch-toggle-regexp
   "C-b" 'isearch-beginning-of-buffer
   "C-d" 'isearch-toggle-word
   "C-f" 'isearch-highlight-lines-matching-regexp
   "C-i" 'isearch-toggle-invisible
   "C-l" 'isearch-yank-line
   "C-m" 'isearch-toggle-symbol
   "C-n" 'isearch-end-of-buffer
   "C-o" 'isearch-occur
   "C-p" 'isearch-highlight-regexp
   "C-v" 'isearch-yank-kill
   "C-y" 'isearch-forward-symbol-at-point
   "H-c" 'ps/isearch-copy-match
   "M-k" 'isearch-ring-retreat
   "M-l" 'isearch-ring-advance)
  ((isearch-mode-map minibuffer-mode-map)
   "C-e" 'isearch-query-replace))

(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
(defadvice
    isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  (recenter))
(defadvice
    isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  (recenter))
(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)

(use-feature replace
  :demand t
  :custom
  ;; emacs.stackexchange.com/a/12318/32089
  (query-replace-from-history-variable 'regexp-search-ring)

  :general
  (isearch-mode-map
   "A-C-e" 'query-replace-regexp))

(use-package substitute
  :straight (substitute
             :host sourcehut
             :repo "protesilaos/substitute")
  :demand t
  :general
  (isearch-mode-map
   "C-t" 'substitute-target-in-buffer))

(use-package rg
  :custom
  (rg-executable "rg")
  (rg-command-line-flags '("--multiline"))
  (rg-group-result t))

(use-package visual-regexp)

(use-package visual-regexp-steroids
  :after visual-regexp
  :demand t
  :custom
  (vr/engine 'python)
  (vr/command-python "python3 /Users/pablostafforini/.emacs.d/straight/build/visual-regexp-steroids/regexp.py")

  :general
  (isearch-mode-map
   "A-C-s" 'vr/isearch-forward
   "A-C-r" 'vr/isearch-backward))

(use-feature imenu
  :custom
  (org-imenu-depth 3))

(use-package pcre2el)

(use-package wgrep
  :general
  (wgrep-mode-map
   "s-c" 'wgrep-finish-edit))

(use-package affe
  :disabled)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :demand t
  :init
  (vertico-mode)

  :config
  (vertico-multiform-mode)

  :custom
  (vertico-multiform-commands
         '((consult-line buffer)
           (consult-imenu buffer)
           (consult-grep buffer)))
  ;; Configure the display per completion category.
  ;; Use the grid display for files and a buffer
  ;; for the consult-grep commands.
  (vertico-multiform-categories
   '((file grid)))
  (vertico-cycle t)
  (vertico-count 16)
  (vertico-quick1 avy-keys)
  (vertico-quick2 avy-keys)

  :general
  (vertico-map
   "<C-i>" 'vertico-exit
   "M-f" 'vertico-quick-exit
   "C-H-M-w" 'vertico-directory-up))

(use-package embark
  :demand t
  :config
  (unless (equal (system-name) ps/computer-hostname-leo)
    (embark-define-keymap embark-yasnippet-completion-actions
      "Embark actions for `consult-yasnippet' and derivatives"
      ("d" consult-yasnippet-visit-snippet-file)))

  (add-to-list 'embark-keymap-alist '(yasnippet . embark-yasnippet-completion-actions))

  :general
  ("C-;" 'embark-act
   "A-C-;" 'embark-dwim
   "C-h B" 'embark-bindings))

(use-package embark-consult
  :demand t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package consult
  :demand t
  :custom
  ;; replace `rg' with `rga'
  (consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
  (consult-locate-args "mdfind")
  (consult-narrow-key "<")
  (consult-widen-key ">")

  :config
  (defun ps/consult-locate-current ()
    "Search with `consult-locate' in current directory."
    (interactive)
    (let ((consult-locate-args (concat "mdfind -onlyin " default-directory)))
      (consult-locate)))

  (defun ps/consult-locate-home ()
    "Search with `consult-locate' in home directory."
    (interactive)
    (let ((consult-locate-args (concat "mdfind -onlyin " ps/dir-user)))
      (consult-locate)))

  (defun ps/consult-locate-anywhere ()
    "Search with `consult-locate' anywhere on my hard drive."
    (interactive)
    (let ((consult-locate-args (concat "mdfind ")))
      (consult-locate)))

  (defun ps/consult-locate-file-current ()
    "Search with `consult-locate' in current directory for
matching file names only."
    (interactive)
    (let ((consult-locate-args (concat "mdfind -name -onlyin " default-directory)))
      (consult-locate)))

  (defun ps/consult-locate-file-home ()
    "Search with `consult-locate' in home directory for
matching file names only."
    (interactive)
    (let ((consult-locate-args (concat "mdfind -name -onlyin " ps/dir-user)))
      (consult-locate)))

  (defun ps/consult-locate-file-anywhere ()
    "Search with `consult-locate' anywhere on my hard drive for
matching file names only."
    (interactive)
    (let ((consult-locate-args "mdfind -name "))
      (consult-locate)))

  (defun ps/consult-ripgrep-current ()
    "Search with `rg' for files in the current directory where the
content matches a regexp."
    (interactive)
    (consult-ripgrep default-directory))

  (defun ps/consult-ripgrep-home ()
    "Search with `rg' for files in home directory where the content
matches a regexp."
    (interactive)
    (consult-ripgrep ps/dir-user))

  (defun ps/consult-ripgrep-anywhere ()
    "Search with `rg' for files anywhere in hard drive where the
content matches a regexp."
    (interactive)
    (consult-ripgrep ps/dir-root))

  (defun ps/consult-org-heading (&optional match scope)
    "Jump to an Org heading.

MATCH and SCOPE are as in org-map-entries and determine which
entries are offered.  By default, all entries of the current
buffer are offered."
    (interactive)
    (widen)
    (ps/org-fold-show-all-headings)
    (consult-org-heading)
    (recenter 1))

  (defun ps/consult-org-agenda (&optional match)
    "Jump to an Org agenda heading.

By default, all agenda entries are offered. MATCH is as in
`org-map-entries' and can used to refine this."
    (interactive)
    (widen)
    (ps/org-fold-show-all-headings)
    (consult-org-agenda)
    (recenter 1))

  (defun ps/consult-recent-files-in-dir (dir)
    "Find recent files in directory using `completing-read'."
    (interactive)
    (find-file
     (consult--read
      (or (mapcar #'abbreviate-file-name
                  (consult--recent-files-sort (f-entries dir)))
          (user-error "No recent files"))
      :prompt "Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history)))

  ;; (advice-remove 'consult-org-heading #'widen)
  ;; (advice-remove 'consult-org-heading #'ps/org-cycle-content)
  ;; (advice-add 'consult-org-agenda :after #'ps/org-narrow-to-entry-and-children)

  :general
  ("A-C-l" 'consult-line
   "H-b" 'consult-buffer
   "H-r" 'consult-history
   "H-V" 'consult-yank-pop
   "H-f" 'ps/consult-locate-current
   "H-f" 'ps/consult-locate-home
   "A-H-f" 'ps/consult-locate-anywhere
   "C-p" 'ps/consult-rga
   "H-p" 'ps/consult-ripgrep-current
   "H-P" 'ps/consult-ripgrep-home
   "A-H-p" 'ps/consult-ripgrep-anywhere)
  (org-mode-map
   "s-j" 'ps/consult-org-heading)
  ((elfeed-show-mode-map eww-mode-map prog-mode-map)
   "s-j" 'consult-imenu))

(use-package consult-dir
  :after consult
  :defer 15

  :custom
  (consult-dir-default-command 'consult-dir-dired)
  ;; Should start using `projectile' first
  ;; (consult-dir-project-list-function 'consult-dir-projectile-dirs)

  :general
  ("H-B" 'consult-dir))

(use-package consult-notes
:disabled)

(use-package consult-yasnippet
  :after (consult yasnippet)
  :general
  ("A-C-y" 'consult-yasnippet))

(use-package consult-spotify
  :disabled
  :demand t
  :after (consult espotify))

(use-package consult-flyspell
  :demand t
  :after (consult flyspell))

(use-package marginalia
  :demand t
  :init
  (marginalia-mode))

(use-package orderless
  :defer 5
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-regexp))
  (orderless-style-dispatchers '(ps/orderless-flex-dispatcher
                                 ps/orderless-initialism-dispatcher
                                 ps/orderless-prefixes-dispatcher
                                 ps/orderless-exclusion-dispatcher))

  :config
  (defun ps/orderless-flex-dispatcher (pattern _index _total)
    "Flex dispatcher using `~' as suffix."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun ps/orderless-initialism-dispatcher (pattern index _total)
    "Initialism dispatcher using `\,' as suffix."
    (when (string-suffix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

      (defun ps/orderless-prefixes-dispatcher (pattern index _total)
    "Prefix dispatcher using `\;' as suffix."
    (when (string-suffix-p ";" pattern)
      `(orderless-prefixes . ,(substring pattern 0 -1))))

  (defun ps/orderless-exclusion-dispatcher (pattern _index _total)
    "Exclusion dispatcher using `!' as suffix."
    (when (string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

(use-feature ido
  :general
  (dired-mode-map
   "i" 'ido-find-file))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info
                              corfu-echo
                              corfu-history
                              ;; corfu-indexed
                              ;; corfu-popupinfo
                              corfu-quick
                              ))

  :demand t
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-cycle vertico-cycle)
  (corfu-count vertico-count)
  (corfu-info-documentation nil)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-popupinfo-delay 0)

  :config
  ;; Adapted from Prot
  ;; protesilaos.com/emacs/dotemacs#h:675ebef4-d74d-41af-808d-f9579c2a5ec4
  (defun ps/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode)))

      ;; github.com/minad/corfu#transfer-completion-to-the-minibuffer
        (defun ps/corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))


  (global-corfu-mode)

  :hook
  (prog-mode-hook . corfu-popupinfo-mode)
  (prog-mode-hook . corfu-echo-mode)
  (corfu-mode-hook . corfu-history-mode)
  (minibuffer-setup-hook . ps/corfu-enable-always-in-minibuffer)

  :general
  (corfu-map
   "M-f" 'corfu-quick-complete
   "M-m" 'ps/corfu-move-to-minibuffer
   "<return>" 'corfu-complete
   "RET" 'corfu-complete))

(use-package corfu-terminal
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after corfu
  :demand t
  :config
  (corfu-terminal-mode))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal
             :host codeberg
                   :repo "akib/emacs-corfu-doc-terminal")
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after corfu-terminal
  :demand t
  :config
  (corfu-doc-terminal-mode))

(use-package kind-icon
  :after corfu
  :demand t

  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :after corfu
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package cape-yasnippet
  :straight (cape-yasnippet
             :host github
             :repo "elken/cape-yasnippet"))

(use-package org-block-capf
  :straight (org-block-capf
             :host github
             :repo "xenodium/org-block-capf")
  :demand t
  :hook
  (org-mode-hook . org-block-capf-add-to-completion-at-point-functions))

(use-package company
  :defer 5
  :hook
  (telega-chat-mode-hook . company-mode))

(use-feature help
  :custom
  (help-window-select t)

  :config
  (defun ps/describe-keymap-of-current-major-mode ()
    (interactive)
    (describe-keymap (current-local-map)))

  :general
  ;; "<C-m>" 'describe mode
  ("C-A-k" 'describe-keymap
   "C-A-m" 'ps/describe-keymap-of-current-major-mode)
  (help-mode-map
   "o" 'ps/help-copy-symbol-name) ; currently not working
  (input-decode-map
   [?\C-m] [C-m]
   [?\C-i] [C-i]))

(use-feature help-at-pt
  :custom
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0)       ; show help immediately when enabled
  :config
  (help-at-pt-set-timer))                ; set timer, thus enabling local help

(use-feature help-fns
  :config
  (defun ps/describe-face ()
    "Like `describe-face', but with `hl-line-mode' disabled, and
always use face at point."
    (interactive)
    (let ((global-hl-line-mode-enabled global-hl-line-mode))
      (global-hl-line-mode -1)
      (describe-face (face-at-point t))
      (when global-hl-line-mode-enabled
        (global-hl-line-mode))))

  :general
  ("C-h C-f" 'ps/describe-face))

(use-package helpful
  :defer 20
  :config
  (defun ps/helpful-copy-as-kill ()
    "Get the name of the symbol whose docstring the current helpful
buffer displays and push it to the kill ring."
    (interactive)
    (kill-new (replace-regexp-in-string "\\(\\*helpful .*: \\)\\(.*\\)\\(\\*\\)" "\\2" (buffer-name)))
    (ps/kill-this-buffer-switch-to-other-window))

  :general
  ("H-U" 'helpful-at-point
   "C-k" 'helpful-key
   "C-h k" 'helpful-key
   "C-h f" 'helpful-function
   "C-A-f" 'helpful-function
   "C-h o" 'helpful-symbol
   "C-A-o" 'helpful-symbol
   "C-h v" 'helpful-variable
   "C-A-v" 'helpful-variable
   "C-h c" 'helpful-command)
  (helpful-mode-map
   "w" 'ps/helpful-copy-as-kill)
  (embark-symbol-map
   "h" 'helpful-symbol))

(use-feature Info
  :general
  (Info-mode-map
  "j" 'Info-prev
  ";" 'Info-next))

(use-feature man)

(use-feature shortdoc
  :general
  ("C-h u" 'shortdoc-display-group))

(use-package elisp-demos
  :init
  (advice-add 'helpful-update :after 'elisp-demos-advice-helpful-update))

(use-package which-key
  :defer 10
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0))

(use-feature kmacro
  :defer 20
  :config
  (kmacro-set-counter 1)
  (defun ps/kmacro-counter-toggle-alpha-number ()
    "Toggle between a numeric and an alphabetical keyboard macro counter."
    (interactive)
    (if (string= kmacro-counter-format "%d")
        (progn
          (kmacro-set-format "%c")
          (kmacro-set-counter 97)
          (message "Set to alphabetical"))
      (progn
        (kmacro-set-format "%d")
        (kmacro-set-counter 1)
        (message "Set to numeric"))))
  :general
  ("A-H-M-s-h" 'kmacro-end-or-call-macro ; = H-h, to circumvent OSX mapping
   "H-H" 'kmacro-start-macro-or-insert-counter
   "A-C-H-s-h" 'kmacro-set-counter
   "C-A-h" 'ps/kmacro-counter-toggle-alpha-number
   "A-h" 'name-last-kbd-macro
   "M-h" 'kmacro-edit-macro
   "M-A-h" 'kmacro-bind-to-key))

(use-package elmacro)

(use-feature simple
  :general
  ("C-A-s" 'shell-command))

(use-feature simple
  :custom
  (async-shell-command-buffer 'new-buffer)) ; don't ask for confirmation before running command in a new buffer

(use-feature shell
  :config
  (defun ps/shell-update-homebrew ()
    "Update Homebrew."
    (interactive)
    (let ((shell-command-buffer-name-async "*homebrew update*"))
      (async-shell-command "brew update; brew upgrade --greedy; brew cleanup; brew doctor")
      (message "Update process finished.")))

  (defun ps/shell-in-current-directory (&optional shell)
    "Open a shell in the current directory, creating one if
necessary."
    (interactive)
    (let* ((shell (or shell
                      'shell))
           (shell-mode (intern (concat (symbol-name shell) "-mode")))
           (current-directory default-directory)
           (found))
      (dolist (buffer (buffer-list (current-buffer)))
        (with-current-buffer buffer
          (when (and (eq major-mode 'shell-mode)
                     (equal current-directory default-directory))
            (setq found buffer))))
      (if found
          (switch-to-buffer found)
        (funcall shell (generate-new-buffer-name (format "*%s*" shell))))))

  :general
  ("A-s" 'ps/shell-in-current-directory)
  (shell-mode-map
   "M-p" nil
   "M-n" nil
   "M-k" 'comint-previous-input
   "M-l" 'comint-next-input))

(use-feature eshell
  :demand t
  :custom
  (eshell-banner-message "")
  (eshell-save-history-on-exit t)
  (eshell-history-size 100000)

  :config
  (defun ps/eshell-open-cwd-on-dired ()
    "Open the current working directory on Dired."
    (interactive)
    (dired "."))

  (defun ps/eshell-new-session ()
    "Create a new interactive Eshell buffer."
    (interactive)
    (eshell '(4)))

  (defun ps/eshell-in-current-directory ()
    "Open a shell in the current directory, creating one if
necessary."
    (interactive)
    (ps/shell-in-current-directory 'eshell))

  :general
  ("A-e" 'ps/eshell-in-current-directory)
  (eshell-mode-map
   "C-H-M-z" 'eshell-kill-input
   "A-C-s-m" 'eshell-bol
   "M-k" 'eshell-previous-matching-input-from-input
   "M-l" 'eshell-next-matching-input-from-input
   "s-l" 'eshell/clear
   "s-c" 'ps/eshell-open-cwd-on-dired
   "s-d" 'eshell-send-eof-to-process
   "H-n" 'ps/eshell-new-session
   "M-p" nil
   "M-n" nil))

(use-feature em-hist
  :after eshell
  :demand t
  :custom
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t))

(use-package eshell-git-prompt
  :disabled
  :after eshell
  :demand t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-syntax-highlighting
  :after eshell
  :demand t
  :hook
  (eshell-mode-hook . eshell-syntax-highlighting-global-mode))

(use-package pcmpl-args
  :disabled
  :defer 5)

(use-package emacs-native-shell-complete
  :disabled
  :after shell
  :demand t
  :straight (emacs-native-shell-complete
             :host github
             :repo "CeleritasCelery/emacs-native-shell-complete"))

(use-package dwim-shell-command
  :straight (dwim-shell-command
             :host github
                   :repo "xenodium/dwim-shell-command"))

(use-package ispell
  :defer 15
  ;; :demand t
  :custom
  (ispell-silently-savep t)
  (ispell-program-name "/opt/homebrew/bin/aspell")

  :config
  (defvar ps/ispell-languages '("espanol" "english")
    "List of languages to use for ispell.")

  (defun ps/ispell-toggle-language ()
    "Toggle ispell dictionaries between languages defined in
 `ps/ispell-languages'."
    (interactive)
    (let ((one (car ps/ispell-languages))
          (two (cadr ps/ispell-languages)))
      (if (string= ispell-local-dictionary one)
          (ispell-change-dictionary two)
        (ispell-change-dictionary one)))
    (flyspell-buffer))
    ;; (message "Language set to %s" ispell-local-dictionary))

  ;; emacs.stackexchange.com/a/74070/32089
  (defun suppress-messages (old-fun &rest args)
    (cl-flet ((silence (&rest args1) (ignore)))
      (advice-add 'message :around #'silence)
      (unwind-protect
          (apply old-fun args)
        (advice-remove 'message #'silence))))

  (add-to-list 'ispell-extra-args "--sug-mode=ultra") ; github.com/rolandwalker/flyspell-lazy#notes

  (advice-add 'ispell-init-process :around #'suppress-messages)
  (advice-add 'ispell-kill-ispell :around #'suppress-messages)


  :general
  ("M-A-p" 'ps/ispell-toggle-language))

(use-feature flyspell
  :demand t
  :after ispell
  :custom
  (flyspell-issue-message-flag nil) ; auto-save personal dictionary whenever a word is added, avoiding prompts
  
  :config
  (defun ps/flyspell-save-word ()
    "Save word at point to personal dictionary."
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


  (defun ps/flyspell-save-word-and-next ()
    "Save word at point to personal dictionary and go to next error."
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))
    (flyspell-goto-next-error))

;; pragmaticemacs.wordpress.com/2015/08/27/jump-back-to-previous-typo/
(defun ps/flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (/= 0 arg)
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (when (= pos min)
          (progn
            (message "No more missspelled words!")
            (setq arg 0))))))

(advice-add 'flyspell-region :around
            #'telega-chatbuf-input-as-region-advice)

  :general
  ("M-p" 'flyspell-buffer
   "A-M-," 'ps/flyspell-goto-previous-error
   "A-M-." 'flyspell-goto-next-error)
  (flyspell-mode-map
   "C-," nil
   "C-." nil
   "C-;" nil)
  (flyspell-mouse-map ;; this key map becomes active only when point is on a highlighted word
   "s-a" 'flyspell-auto-correct-word
   "s-k" 'ps/flyspell-goto-previous-error
   "s-l" 'flyspell-goto-next-error
   "s-s" 'ps/flyspell-save-word
   "A-s-s" 'ps/flyspell-save-word-and-next
   "s-c" 'flyspell-correct-wrapper)

  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode))

(use-package flyspell-lazy
  :after flyspell
  :demand t
  :config
  (flyspell-lazy-mode))

(use-package flyspell-correct
  :demand t
  :after flyspell)

(use-package keytar
  :if (equal (system-name) ps/computer-hostname-pablo)
  :defer 10)

(use-package grammarly
  :demand t
  :custom
  (grammarly-username ps/personal-gmail)
  (grammarly-password (auth-source-pass-get 'secret (concat "chrome/grammarly.com/" ps/personal-gmail))))

(use-package lsp-grammarly
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after (lsp-mode keytar)
  :demand t

  :custom
  (lsp-grammarly-suggestions-split-infinitive nil)
  (lsp-grammarly-suggestions-preposition-at-the-end-of-sentence nil)
  (lsp-grammarly-suggestions-possibly-biased-language-age-related nil)
  (lsp-grammarly-suggestions-possibly-biased-language-disability-related nil)
  (lsp-grammarly-suggestions-possibly-biased-language-family-related nil)
  (lsp-grammarly-suggestions-possibly-biased-language-gender-related nil)
  (lsp-grammarly-suggestions-possibly-biased-language-human-rights nil)
  (lsp-grammarly-suggestions-possibly-biased-language-human-rights-related nil)
  (lsp-grammarly-suggestions-possibly-biased-language-lgbtqia-related nil)
  (lsp-grammarly-suggestions-possibly-biased-language-race-ethnicity-related nil)
  (lsp-grammarly-suggestions-possibly-politically-incorrect-language nil)

  :hook
  (org-mode-hook . (lambda ()
                     (require 'lsp-grammarly)
                     (lsp))))

(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :defer t
  :hook ((text-mode markdown-mode). (lambda ()
                                      (require 'eglot-grammarly)
                                      (eglot-ensure))))

(use-package aide
  :straight (aide
             :host github
             :repo "junjizhi/aide.el")
  :after request
  :commands aide-openai-complete-region
  :custom
  (openai-api-key (auth-source-pass-get 'secret "auth-sources/openai.com")))

(use-feature text-mode
  :hook
  (text-mode-hook . visual-line-mode))

(use-feature with-editor
  :general
  ("s-c" 'with-editor-finish
   "s-k" 'with-editor-abort
   "C-c C-c" 'with-editor-finish))

(use-feature dictionary
  :custom
  (dictionary-server "dict.org"))

(use-package osx-dictionary
  :general
  ("H-y" 'osx-dictionary-search-input))

(use-package powerthesaurus
  :config
  (defun ps/powerthesaurus-lookup-dwim (&optional action-type query-type)
  "Wrapper function for general lookup commands.

When called interactively, optional argument ACTION-TYPE corresponds to
the prefix argument passed to this command, which is translated to an action
using `powerthesaurus-prefix-to-action'.  When called programmatically,
its value can either be nil or a symbol that can be possibly returned by
`powerthesaurus-prefix-to-action' (e.g., `action-insert' or `action-display').

The argument passed to QUERY-TYPE should be the same as in
`powerthesaurus-lookup' or nil; in the latter case,
the user will be prompt for a valid value."
  (interactive "P")
  (pcase-let ((`(,query-term ,beg ,end)
               ;; selection is active -> look up whatever is selected
               (if (use-region-p)
                   (powerthesaurus--extract-query-region)
                 ;; point is is at a word -> look it up
                 (if (thing-at-point 'word)
                     (powerthesaurus--extract-original-word)
                   ;; nothing appropriate nearby -> ask the user
                   (list nil nil nil)))))
    (setq query-term (or query-term
                         (read-string "Term: " query-term))
          query-type (or query-type
                         (completing-read "Query type: "
                                          powerthesaurus-supported-query-types
                                          nil t))
          action-type (powerthesaurus-prefix-to-action action-type query-type))
    (cond
     ((eq action-type 'action-insert)
      (when (null beg)
        (setq beg (point) end (point))))
     ((eq action-type 'action-display)
      (when (or beg end)
        (setq beg nil end nil))))
    (funcall 'powerthesaurus-lookup query-term query-type beg end)))

  (advice-add 'ps/powerthesaurus-lookup-dwim  :override #'ps/ps/powerthesaurus-lookup-dwim)

  :general
  ("H-Y" 'powerthesaurus-lookup-dwim))

(defun ps/goldendict-search-input (arg)
  "If there is a word at point or an active selection, look it up
in GoldenDict, else prompt user for input. If invoked with prefix
argument, always force prompt."
  (interactive "P")
  (let ((string (ps/goldendict-region-or-word)))
    (if (and string (not arg)) (kill-new string)
      (kill-new (read-string "Expression: ")))
    (shell-command "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"D4404D73-FF1D-4DF6-8107-7AB050C28C9F\"'")))

(defun ps/goldendict-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string. Otherwise return word
around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(general-define-key
 "A-y" 'ps/goldendict-search-input)

(use-package google-translate
  :disabled
  :defer 20
  :functions (my-google-translate-at-point google-translate--search-tkk)

  :custom
  (google-translate-default-target-language "en")
  (google-translate-default-source-language "es")

  :config
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  (setq google-translate-backend-method 'curl)

  (defun ps/google-translate-dwim (&optional reverse-p)
    "Translate region if active, word if at point, else prompt for
text. If invoked with a prefix argument, perform a reverse
translation."
    (interactive "P")
    (let ((google-translate-default-source-language ps/ispell-language)
          (google-translate-default-target-language
           (if (string= ps/ispell-language "en")
               "es"
             "en")))
      (if reverse-p
          (google-translate-at-point-reverse)
        (google-translate-at-point))))

  ;; modify original function so that it prompts for text to translate
  ;; if region is inactive and no word is at point.
  (defun ps/%google-translate-at-point (override-p reverse-p)
    (let* ((langs (google-translate-read-args override-p reverse-p))
           (source-language (car langs))
           (target-language (cadr langs))
           (bounds nil))
      (google-translate-translate
       source-language target-language
       (cond ((string-equal major-mode "pdf-view-mode") (car (pdf-view-active-region-text)))
             ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
             (t (or (and (setq bounds (bounds-of-thing-at-point 'word))
                         (buffer-substring-no-properties (car bounds) (cdr bounds)))
                    (google-translate-query-translate)))))))

  (advice-add '%google-translate-at-point :override #'ps/%google-translate-at-point)

  :general
  ("H-A-y" 'ps/google-translate-dwim))

(use-package txl
  :straight (txl
             :host github
             :repo "tmalsburg/txl.el")
  :custom
  (txl-languages '(ES . EN-US))
  (txl-deepl-api-key (auth-source-pass-get "api" (concat "chrome/deepl.com/" ps/personal-email)))

  :general
  ("H-A-y" 'txl-translate-region-or-paragraph)
  (txl-edit-translation-mode-map
   "RET" 'txl-accept-translation
   "q" 'txl-dismiss-translation))

(use-package atomic-chrome
  :defer 10

  :custom
  (atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-url-major-mode-alist
   '(("github\\.com" . gfm-mode)
     ("wikipedia\\.org" . mediawiki-mode)
     ("timelines\\.issarice\\.com" . mediawiki-mode)))

  :config
  (atomic-chrome-start-server)

  :general
  (atomic-chrome-edit-mode-map
   "s-c" 'atomic-chrome-close-current-buffer))

(use-package markdown-mode
  :demand t
  :after atomic-chrome
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc --from markdown --to html")
  (markdown-disable-tooltip-prompt t)
  (markdown-italic-underscore 'double)

  :config
  (defun ps/markdown-insert-italic ()
    "Insert markup to make a region or word italic.
If there is an active region, make the region italic.  If the point
is at a non-italic word, make the word italic.  If the point is at an
italic word or phrase, remove the italic markup.  Otherwise, simply
insert italic delimiters and place the point in between them."
    (interactive)
    (let ((delim (cond ((eq markdown-italic-underscore t) "_")
                       ((eq markdown-italic-underscore nil) "*")
                       ((eq markdown-italic-underscore 'double) "__"))))
      (markdown--insert-common delim delim markdown-regex-italic 1 3 'markdown-italic-face t)))

  (advice-add 'markdown-insert-italic :override #'ps/markdown-insert-italic)

  (defun ps/markdown-paste-from-org ()
    "Take the contents of the system clipboard and use `pandoc' to
convert it from org-mode to markdown."
    (interactive)
    (let* ((clipboard (if (eq system-type 'darwin)
                          "pbv public.utf8-plain-text"
                        "xclip -out -selection 'clipboard' -t text/html"))
           (pandoc (concat "pandoc --wrap=none -f org -t markdown"))
           (cmd (concat clipboard " | " pandoc))
           (output (shell-command-to-string cmd))
           ;; Not sure why Pandoc adds these double slashes; we remove them
           (output (replace-regexp-in-string "^\\\\\\\\$" "" output))
           (text (replace-regexp-in-string "= " "= " output)))
      (kill-new text)
      (yank)))

  :general
  ((gfm-mode-map markdown-mode-map)
   "s-b" 'markdown-insert-bold
   "s-e" 'markdown-insert-code
   "s-i" 'markdown-insert-italic
   "s-k" 'markdown-insert-link
   "s-v" 'ps/markdown-paste-from-org)
  (gfm-mode-map
   "s-a" 'markdown-insert-gfm-code-block
   "s-z" 'markdown-edit-code-block))

(use-package mediawiki
  :demand t
  :after atomic-chrome)

(defun ps/gdrive-import-file ()
  "Import Google Doc file with DOC-ID and convert it to org-mode.

To see a list of Google Docs and their respective IDs, run
`gdrive list' in the terminal."
  (interactive)
  (let* ((default-directory ps/dir-downloads)
         (doc-id (read-from-minibuffer "Doc ID: "))
         (doc-info (shell-command-to-string
                    (format "gdrive info '%s'" doc-id)))
         (doc-name (when (string-match "^Name: \\(.*\\)$" doc-info)
                     (match-string 1 doc-info)))
         (input (concat doc-name ".docx"))
         (output (concat doc-name ".org")))
    ;; download Google Doc as docx
    (shell-command
     (format "gdrive export --mime application/vnd.openxmlformats-officedocument.wordprocessingml.document %s" doc-id))
    ;; export docx to org-mode
    (shell-command
     (format "pandoc -s '%s' -o '%s'" input output))))

(use-package edit-indirect)

(use-package ledger-mode
  :defer 30
  :custom
  (ledger-default-date-format ledger-iso-date-format)
  (ledger-reconcile-default-commodity "ARS")
  (ledger-schedule-file ps/file-tlon-ledger-schedule-file)
  (ledger-schedule-look-forward 0)
  (ledger-schedule-look-backward 30)

  :config
  (dolist (report
           '(("net worth"
              "%(binary) -f %(ledger-file) bal --strict")
             ("net worth (USD)"
              "%(binary) -f %(ledger-file) --price-db .pricedb --exchange USD bal ^assets ^liabilities --strict")
                                        ; I need to understand how the `--basis' flag works
             ("cost basis"
              "%(binary) -f %(ledger-file) --basis bal %(account) --strict")))
    (add-to-list 'ledger-reports report))

  (defun ps/ledger-new-entry-below ()
    "Create new entry below one at point."
    (interactive)
    (indent-for-tab-command)
    (ledger-navigate-next-xact-or-directive)
    (crux-smart-open-line-above))

  (defun ps/ledger-align-and-next ()
    "Align transaction at point and move point to next entry."
    (interactive)
    (ledger-post-align-xact (point))
    (ledger-navigate-next-xact-or-directive))

  (defun ps/ledger-report-account ()
    "Runs an 'account' report from `ledger-reports'."
    (interactive)
    (ledger-report "account" nil))

  (defun ps/ledger-report-net-worth ()
    "Runs an 'net worth' report from `ledger-reports'."
    (interactive)
    (ledger-report "net worth" nil))

  (defun ps/ledger-report-net-worth-USD ()
    "Runs an 'net worth (USD)' report from `ledger-reports'."
    (interactive)
    (ledger-report "net worth (USD)" nil))

  (defun ps/ledger-report-payee ()
    "Runs an 'payee' report from `ledger-reports'."
    (interactive)
    (ledger-report "payee" nil))

  (defun ps/ledger-update-commodities ()
    "Update `commodities.py'."
    (interactive)
    (shell-command
     (format "python3 %s"
             (file-name-concat ps/dir-ledger "commodities.py"))))

  (defun ps/ledger-update-coin-prices ()
    "Update `coinprices.py'."
    (interactive)
    (shell-command
     (format "python3 %s >> %s"
             (file-name-concat ps/dir-ledger "coinprices/coinprices.py")
             ps/file-ledger-db)))

  (defun ps/ledger-sort-region-reversed (beg end)
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

  (defun ps/ledger-sort-buffer-reversed ()
    "Sort the entire buffer in reverse chronological order."
    (interactive)
    (let (sort-start
          sort-end)
      (save-excursion
        (goto-char (point-min))
        (setq sort-start (ledger-sort-find-start)
              sort-end (ledger-sort-find-end)))
      (ps/ledger-sort-region-reversed (or sort-start (point-min))
                                      (or sort-end (point-max)))))

  (defun ps/ledger-sort-region-or-buffer ()
    "Sort a region if selected, otherwise the whole buffer."
    (interactive)
    (if (region-active-p)
        (ledger-sort-region)
      (ledger-sort-buffer)))

  (defun ps/ledger-sort-region-or-buffer-reversed ()
    "Sort in reverse chronological order a region if selected,
otherwise the whole buffer."
    (interactive)
    (if (region-active-p)
        (ps/ledger-sort-region-reversed)
      (ps/ledger-sort-buffer-reversed)))

  (defun ps/ledger-toggle-current-transaction-and-next ()
    "Toggle current transaction and move to the next transaction"
    (interactive)
    (ledger-toggle-current-transaction)
    (ledger-navigate-next-xact-or-directive))

  (defun ps/ledger-copy-transaction-at-point ()
    (interactive)
    "Save transaction at point to the kill ring."
    (save-excursion
      (ledger-navigate-next-xact-or-directive)
      (let ((end (point)))
        (ledger-navigate-prev-xact-or-directive)
        (copy-region-as-kill (point) end))
      (message "Transaction copied.")))

  (defun ps/ledger-narrow-to-xact ()
    "Narrow to the current transaction."
    (interactive)
    (let ((xact-begins (ledger-navigate-beginning-of-xact))
          (xact-ends (ledger-navigate-end-of-xact)))
      (narrow-to-region xact-begins xact-ends)))

  (defun ps/ledger--increase-date-of-transaction-at-point (days)
  (interactive)
  "Increase date of transaction at point by DAYS."
  (let* ((xact-begins (ledger-navigate-beginning-of-xact))
         (xact-ends (ledger-navigate-end-of-xact))
         (xact (buffer-substring xact-begins xact-ends)))
    (delete-region xact-begins xact-ends)
    (insert
     (with-temp-buffer
       (insert xact)
       (let* ((date (ledger-xact-date))
              (timestamp (date-to-time date))
              (date-minus-one-day (format-time-string "%Y-%m-%d" (time-add timestamp (days-to-time days)))))
         (beginning-of-buffer)
         (replace-regexp ledger-iso-date-regexp "")
         (insert date-minus-one-day)
         (buffer-string))))))

  (defun ps/ledger-increase-date-of-transaction-at-point-by-one-day ()
    (interactive)
    "Increase date of transaction at point by one day."
    (ps/ledger--increase-date-of-transaction-at-point 1))

  (defun ps/ledger-decrease-date-of-transaction-at-point-by-one-day ()
    (interactive)
    "Decrease date of transaction at point by one day."
    (ps/ledger--increase-date-of-transaction-at-point -1))

  ;; :hook
  ;; (ledger-mode-hook . (lambda () (setq-local ledger-complete-in-steps t)))

  :general
  (ledger-mode-map
   "s-SPC" 'ps/ledger-new-entry-below
   "s-=" 'ledger-reconcile
   "s-a" 'ledger-add-transaction
   "A-s-a" 'ps/ledger-report-account
   "s-b" 'ledger-post-edit-amount
   "A-s-b" 'ps/ledger-decrease-date-of-transaction-at-point-by-one-day
   "s-c" 'ps/ledger-align-and-next
   "A-s-c" 'ps/ledger-copy-transaction-at-point
   "s-d" 'ledger-delete-current-transaction
   "s-e" 'ps/ledger-toggle-current-transaction-and-next
   "A-s-e" 'ledger-toggle-current-transaction
   "s-f" 'ledger-occur
   "A-s-f" 'ps/ledger-increase-date-of-transaction-at-point-by-one-day
   "s-g" 'ledger-report-goto
   "s-i" 'ledger-insert-effective-date
   "s-y" 'ledger-copy-transaction-at-point
   "s-k" 'ledger-report-quit
   "s-l" 'ledger-display-ledger-stats
   "s-o" 'ledger-report-edit-report
   "s-p" 'ledger-display-balance-at-point
   "A-s-p" 'ps/ledger-report-payee
   "s-q" 'ledger-post-align-dwim
   "s-r" 'ledger-report
   "s-s" 'ledger-report-save
   "s-t" 'ps/ledger-sort-region-or-buffer
   "A-s-t" 'ps/ledger-sort-region-or-buffer-reversed
   "s-u" 'ledger-schedule-upcoming
   "A-s-u" 'ps/ledger-report-net-worth-USD
   "s-v" 'ledger-copy-transaction-at-point
   "A-s-w" 'ps/ledger-report-net-worth
   "s-x" 'ledger-fully-complete-xact
   "s-z" 'ledger-report-redo
   "A-C-s-r" 'ledger-navigate-prev-xact-or-directive
   "A-C-s-f" 'ledger-navigate-next-xact-or-directive)
  (ledger-reconcile-mode-map
   "q" 'ledger-reconcile-quit))

(use-package parse-csv)

(use-package pdf-tools
  :if (or (equal (system-name) ps/computer-hostname-pablo)
          (equal (system-name) ps/computer-hostname-leo))
  :defer 10
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-view-resize-factor 1.1)
  (pdf-annot-default-annotation-properties
   '((t
      (label . ps/personal-name))
     (text
      (color . "#ff0000")
      (icon . "Note"))
     (highlight
      (color . "LightBlue2"))
     (underline
      (color . "blue"))
     (squiggly
      (color . "orange"))
     (strike-out
      (color . "red"))))

  :config
  (unless (or noninteractive (eq this-command 'pdf-tools-install))
    (pdf-tools-install))

  (defun ps/pdf-tools-apply-theme ()
    "Activate `pdf-tools' midnight mode iff dark theme is active."
    (if (eq (modus-themes--current-theme) ps/theme-loaddefs-dark)
        (pdf-view-midnight-minor-mode 1)
      (pdf-view-midnight-minor-mode -1)))

  ;; gist.github.com/politza/3f46785742e6e12ba0d1a849f853d0b9#file-scroll-other-window-el
  (defun ps/pdf-tools-toggle-writeroom ()
    "Toggle `writeroom-mode' on/off."
    (interactive)
    (let ((writeroom-width 120))
      (writeroom-mode 'toggle)
      (pdf-view-fit-height-to-window)))

  (load-file (file-name-concat ps/dir-emacs-local "scroll-other-window.el"))

  (defun ps/pdf-tools-open-externally ()
    "Open current PDF in external application. If `opentopage
 script is available, open to current page."
    (interactive)
    (let ((file (pdf-view-buffer-file-name))
          (script "opentopage")) ; apple.stackexchange.com/a/233987
      (if (file-exists-p (file-name-concat "~/bin" script))
          (shell-command (format "sh %s '%s' %d" script file (pdf-view-current-page)))
        (shell-command (format "open '%s'" file)))))

  (defvar ps/pdf-tools-selected-pages '())

  (defun ps/pdf-tools-add-or-remove-page ()
    "Add current page number to list of selected pages. If page
number is already listed, remove it from list."
    (interactive)
    (if (member (pdf-view-current-page) ps/pdf-tools-selected-pages)
        (progn
          (setq ps/pdf-tools-selected-pages (delete (pdf-view-current-page) ps/pdf-tools-selected-pages)
                ps/pdf-tools-selected-pages (sort ps/pdf-tools-selected-pages #'<))
          (message "Page removed. Current selection: %s." ps/pdf-tools-selected-pages))
      (add-to-list 'ps/pdf-tools-selected-pages (pdf-view-current-page) t)
      (setq ps/pdf-tools-selected-pages (sort ps/pdf-tools-selected-pages #'<))
      (message "Page added. Current selection: %s." ps/pdf-tools-selected-pages))
    (when (< (pdf-view-current-page) (pdf-cache-number-of-pages))
      (pdf-view-next-page))
    (setq ps/pdf-tools-selected-pages (sort ps/pdf-tools-selected-pages #'<)))

  (defun ps/pdf-tools-clear-page-selection ()
    "Clear the list of pages selected in `ps/pdf-tools-selected-pages'."
    (interactive)
    (setq ps/pdf-tools-selected-pages '())
    (message "Page selection cleared."))

  (defun ps/pdf-tools-extract-pages (file)
    "Save pages selected in `ps/pdf-tools-selected-pages' to
FILE."
    (interactive "FSave as: ")
    (let ((output (if (string= (expand-file-name file) (buffer-file-name))
                      "--replace-input"
                    (expand-file-name file))))
      (shell-command (format "qpdf '%s' --pages . %s -- '%s'"
                             (buffer-file-name)
                             (mapconcat #'number-to-string
                                        ps/pdf-tools-selected-pages
                                        ",")
                             output)))
    (ps/pdf-tools-clear-page-selection))

  (defun ps/pdf-count-words ()
    "Count words in current PDF."
    (interactive)
    (kill-new
     (string-trim
      (shell-command-to-string
       (format "pdftotext '%s' - | wc -w" (buffer-file-name)))))
    (message (format "This PDF has %s words." (current-kill 0))))

  (defun ps/pdf-tools-copy-all-text ()
    "Copy all text in current PDF to kill ring."
    (interactive)
    (kill-new
     (string-trim
      (shell-command-to-string
       (format "pdftotext '%s' -" (buffer-file-name)))))
    (message "Copied all text in PDF to kill ring."))

  :hook
  (pdf-tools-enabled-hook . ps/pdf-tools-apply-theme)
  (pdf-tools-enabled-hook . pdf-view-fit-page-to-window)
  (pdf-tools-enabled-hook . sow-mode)

  :general
  ((pdf-view-mode-map pdf-annot-minor-mode-map pdf-history-minor-mode-map)
   "a" 'ps/pdf-tools-copy-all-text
   "c" 'ps/pdf-count-words
   "C" 'ps/pdf-tools-clear-page-selection
   "e" 'pdf-annot-add-highlight-markup-annotation
   "h" 'pdf-annot-add-highlight-markup-annotation
   "j" 'pdf-view-goto-page
   "k" 'pdf-view-previous-line-or-previous-page
   "l" 'pdf-view-next-line-or-next-page
   "t" 'ps/pdf-tools-toggle-writeroom
   "x" 'ps/pdf-tools-open-externally
   "S" 'ps/pdf-tools-add-or-remove-page
   "X" 'ps/pdf-tools-extract-pages
   "H-c" 'pdf-view-kill-ring-save
   "A-d" 'pdf-view-midnight-minor-mode)
  (sow-mode-map
   "A-C-s-t" 'sow-scroll-other-window-down
   "A-C-s-g" 'sow-scroll-other-window))

(use-package pdf-view-restore
  :after pdf-tools
  :demand t
  :hook
  (pdf-view-mode-hook . pdf-view-restore-mode))

(use-package org-pdftools
  :after org pdf-tools
  :demand t
  :hook
  (org-mode-hook . org-pdftools-setup-link))

(use-feature prog-mode
  :config
  (global-prettify-symbols-mode)
  :hook
  (prog-mode-hook . outline-minor-mode)
  (prog-mode-hook . hs-minor-mode)
  :general
  ("A-H-v" 'set-variable
   "M-d" 'toggle-debug-on-error
   "A-M-d" 'toggle-debug-on-quit)
  (prog-mode-map
   "A-H-C-i" 'mark-defun
   "M-." 'xref-find-definitions)
  (emacs-lisp-mode-map shell-mode-map
      "s-c" 'exit-recursive-edit))

(use-package lsp-mode
  :disabled
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "A-C-l")

  :custom
  (lsp-ui-doc-show-with-cursor t) ; move the cursor over a symbol to show its documentation
  (lsp-warn-no-matched-clients nil)
  (lsp-headerline-breadcrumb-enable nil)

  :config

  (defun ps/lsp-toggle ()
    "Connect/disconnect to lsp server."
    (interactive)
    (if (or (not lsp-mode)
            (equal lsp-mode '(lsp-enable-which-key-integration)))
        (lsp)
      (lsp-disconnect)))

  :hook
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)

  :general
  ("A-l" 'ps/lsp-toggle))

(use-package lsp-ui
  :after lsp-mode
  :demand t
  :commands lsp-ui-mode)

;; optionally if you want to use debugger
(use-package dap-mode
  :after lsp-mode
  :demand t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package dumb-jump)

(use-feature elisp-mode
  :general
  (emacs-lisp-mode-map
   "s-d" 'eval-defun
   "A-s-d" 'edebug-defun))

(use-feature debug)

(use-feature backtrace
  :disabled
  :custom
  (backtrace-line-length nil))

(use-package macrostep)

(use-package ess
:disabled)

(use-package clojure-mode)

(use-package cider)

(use-feature python
  :demand t
  :custom
  (python-shell-interpreter "python3")
  (org-babel-python-command "python3")

  :config
  (setq flycheck-python-pycompile-executable "python3")
  (remove-hook 'python-mode-hook #'yasnippet-snippets--fixed-indent) ; some package (`elpy'?) is adding this
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)))

  :general
  (python-mode-map
   "s-l" 'python-shell-send-file
   "s-d" 'python-shell-send-defun
   "s-c" 'python-shell-send-buffer
   "s-s" 'python-shell-send-string
   "s-r" 'python-shell-send-region
   "s-e" 'python-shell-send-statement))

(use-package applescript-mode)

(use-package puni
  :disabled
  :demand t
  :general
  (puni-mode-map
   "C-M-a" 'puni-beginning-of-sexp
   "C-M-b" 'puni-backward-sexp
   "C-M-e" 'puni-end-of-sexp
   "C-M-f" 'puni-forward-sexp
   "C-S-k" 'puni-backward-kill-line
   "C-c DEL" 'puni-force-delete
   "C-d" 'puni-forward-delete-char
   "C-k" nil
   "C-w" nil
   "DEL" 'puni-backward-delete-char
   "M-(" 'puni-syntactic-backward-punct
   "M-)" 'puni-syntactic-forward-punct
   "M-DEL"    'puni-backward-kill-word)

  :hook
  ((prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook) . puni-mode))

(use-package copilot
  :if (equal (system-name) ps/computer-hostname-pablo)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))

  :custom
  (copilot-node-executable "/opt/homebrew/opt/node@16/bin/node")

  :hook
  (prog-mode-hook . copilot-mode)
  (emacs-lisp-mode-hook . copilot-mode)

  :general
  (copilot-mode-map
   "M-<tab>" 'copilot-accept-completion
   "A-M-<tab>" 'copilot-accept-completion-by-line))

(use-package gpt
  :commands gpt-dwim
  :config
  (setq gpt-openai-key (auth-source-pass-get 'secret "auth-sources/openai.com"))

  :general
  ("A-C-g" 'gpt-dwim))

(use-package elpy
  :custom
  (elpy-rpc-python-command "python3")
  (elpy-rpc-virtualenv-path 'current)
  :config
  (elpy-enable))

(use-package epc)

(use-feature eldoc
  :demand t
  :config
  ;; emacs.stackexchange.com/a/55914/32089
  (define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
    "If SYM is a function, append its docstring."
    (concat
     (apply orig-fun sym r)
     (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
            (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
       (and oneline
            (stringp oneline)
            (not (string= "" oneline))
            (concat "  |  " (propertize oneline 'face 'italic))))))

  (global-eldoc-mode))

(use-package bicycle
  :after outline)

(use-package org
  :custom
  (org-directory ps/dir-org) ; set org directory
  (org-todo-keywords
   '((sequence "TODO(t)"
               "DOING(g)"
               "SOMEDAY(s)"
               "MAYBE(m)"
               "WAITING(w)"
               "PROJECT(p)"
               "|"
               "DELEGATED(l)"
               "CANCELLED(c)"
               "DONE(d)")))
  (org-priority-highest 1)
  (org-priority-default 7)
  (org-priority-lowest 9 "set priorities")
  ;; (org-extend-today-until 4 "youtu.be/31gwvApo8zg?t=3342")
    (org-deadline-warning-days 0)              ; show due tasks only on the day the tasks are due
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t "indent every heading and hide all but the last leading star")
  (org-return-follows-link t)
  (org-startup-folded t)
  (org-startup-indented t)
  (org-log-into-drawer "STATES")
  (org-clock-into-drawer "LOGBOOK" "file task state changes in STATES drawer")
  (org-log-done 'time "add timestamp when task is marked as DONE")
  (org-log-repeat nil "do not log TODO status changes for repeating tasks")
  (org-M-RET-may-split-line nil "irreal.org/blog/?p=6297")
  (org-loop-over-headlines-in-active-region t "Allow simultaneous modification of multiple task statuses.")
  (org-ctrl-k-protect-subtree t)
  (org-catch-invisible-edits 'smart)
  (org-special-ctrl-a/e t "`org-beginning-of-line' goes to beginning of first word")
  (org-link-search-must-match-exact-headline nil)
  (org-mark-ring-length 4)
  (org-pretty-entities nil)
  (org-image-actual-width '(800))
  (org-ellipsis " ")
  (org-link-elisp-confirm-function nil)
  (org-file-apps '((auto-mode . emacs)
                   (directory . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'" . emacs)))
  (org-use-tag-inheritance t)

  (org-structure-template-alist '(("a" . "export ascii")
                                  ("c" . "center")
                                  ("C" . "comment")
                                  ("e" . "example")
                                  ("E" . "export")
                                  ("h" . "export html")
                                  ("l" . "export latex")
                                  ("q" . "quote")
                                  ("s" . "src")
                                  ("se" . "src emacs-lisp")
                                  ("sj" . "src clojure")
                                  ("sp" . "src python")
                                  ("ss" . "src shell")
                                  ("v" . "verse")
                                  ("w" . "WP")))

  :config
  (dolist (module '(org-habit org-tempo))
    (add-to-list 'org-modules module))

  (defun ps/org-set-todo-properties ()
    "Set priority and effort."
    (interactive)
    (org-priority)
    (org-set-effort))

  (defun ps/org-url-dwim ()
    "docstring"
    (interactive)
    (cond
     ((url-get-url-at-point)
      (kill-new (url-get-url-at-point)))
     ((ps/org-link-get-url-at-point)
      (kill-new (ps/org-link-get-thing-at-point 1)))))

  ;; Adapted from lists.gnu.org/archive/html/emacs-orgmode/2011-06/msg00716.html
  (defun ps/org-link-get-thing-at-point (arg)
    "When point is on org link, extract object, as defined by ARG."
    (when (org-in-regexp org-link-bracket-re 1)
      (kill-new (org-link-unescape (match-string-no-properties arg)))))

  (defun ps/org-link-get-link-at-point ()
    "When point is on org link, extract link (both url and
description)."
    (interactive)
    (ps/org-link-get-thing-at-point 0))

  (defun ps/org-link-get-url-at-point ()
    "When point is on org link, extract url."
    (interactive)
    (ps/org-link-get-thing-at-point 1))

  (defun ps/org-link-get-description-at-point ()
    "When point is on org link, extract description."
    (interactive)
    (ps/org-link-get-thing-at-point 2))

  (defun ps/org-isearch-visible-org-heading ()
    "Jump to first visible org heading that matches string."
    (interactive)
    (widen)
    (let ((search-invisible nil)
          (isearchp-regexp-quote-yank-flag nil))
      (isearch-forward-regexp nil 1) ; isearch+
      (ps/isearch-yank-unquoted-string "^\*+ ")))

  ;; from http://emacs.stackexchange.com/a/10714/32089
  (defun ps/org-remove-link ()
    "Replace an org link by its description or, if empty, its
address."
    (interactive)
    (if (org-in-regexp org-link-bracket-re 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description
                 (if (match-end 2)
                     (org-match-string-no-properties 2)
                   (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))

  (defun ps/org-insert-todo-subheading-after-body ()
    (interactive)
    (save-restriction
      (org-narrow-to-subtree)
      (outline-hide-subtree)
      (outline-show-entry)
      (goto-char (point-max))
      (org-beginning-of-line)
      (org-insert-todo-heading nil t)
      (org-do-demote)))

  ;; Adapted from hungyi.net/posts/org-mode-subtree-contents
  (defun ps/org-get-heading-contents ()
    "Get the content text of the heading at point and add it to the `kill-ring'.
Excludes the heading itself and any child subtrees."
    (if (org-before-first-heading-p)
        (message "Not in or on an org heading")
      (save-excursion
        ;; If inside heading contents, move the point back to the heading
        ;; otherwise `org-agenda-get-some-entry-text' won't work.
        (unless (org-at-heading-p) (org-previous-visible-heading 1))
        (let ((contents (substring-no-properties
                         (org-agenda-get-some-entry-text
                          (point-marker)
                          most-positive-fixnum))))
          contents))))

  (defun ps/org-copy-heading-contents ()
    (interactive)
    (let ((contents (ps/org-get-heading-contents)))
      (if (string= contents "")
          (message "Heading is empty.")
        (message "Copied: %s" contents)
        (kill-new contents))))


  (defun ps/org-copy-heading-name ()
    (interactive)
    "Copy name of heading at point."
    (kill-new (org-entry-get nil "ITEM")))

  ;; reddit.com/r/emacs/comments/e4jnlj/how_to_create_a_word_counter_that_counts_words_in/f9e3796
  (defun ps/org-count-words ()
    "If region is active, count words in it; otherwise count words
in current subtree."
    (interactive)
    (if (use-region-p)
        (funcall-interactively #'count-words-region (region-beginning) (region-end))
      (org-with-wide-buffer
       (cl-loop for (lines words characters)
                in (org-map-entries
                    (lambda ()
                      (unpackaged/org-forward-to-entry-content 'unsafe)
                      (let ((end (org-entry-end-position)))
                        (list (count-lines (point) end)
                              (count-words (point) end)
                              (- end (point)))))
                    nil 'tree)
                sum lines into total-lines
                sum words into total-words
                sum characters into total-characters
                finally return (let ((message (format "Subtree \"%s\" has %s lines, %s words, and %s characters."
                                                      (org-get-heading t t) total-lines total-words total-characters)))
                                 (kill-new (number-to-string total-words))
                                 (message message)
                                 message)))))

  ;; Note that there exists `org-back-to-heading', possibly making the below redundant or inferior
  (defun ps/org-jump-to-beginning-of-heading ()
    "Move to the beginning of heading at point."
    (interactive)
    (if visual-line-mode
        (progn
          (visual-line-mode -1)
          (setq visual-line-mode-toggle t)))
    (when (org-at-heading-p)
      (next-line))
    (org-previous-visible-heading 1)
    (if visual-line-mode-toggle
        (visual-line-mode)))

  (defun ps/org-move-subtree ()
    "Move subtree at point to separate file. If parent heading is
'Parent heading', file will be named `parent-heading.org'."
    (interactive)
    (ps/org-jump-to-beginning-of-heading)
    (let* ((name (ps/org-copy-heading-name))
           ;; TODO: expand regular expression so that it reflects the transformation used by the EA Wiki
           (filename (expand-file-name
                      (concat (ps/org-wiki-slug name) ".org")
                      ps/dir-people)))
      (org-cut-subtree)
      (find-file-other-window filename)
      (let ((subtree (replace-regexp-in-string "^\\*+" "*" (current-kill 0))))
        (insert (concat "#+title: " name "\n\n" subtree)))
      (ps/org-jump-to-beginning-of-heading)
      (ps/switch-to-last-window)))

  (defun ps/org-jump-to-first-heading ()
    "Move point to the beginning of the first org heading in the
current buffer."
    (interactive)
    (widen)
    (goto-char (point-min))
    (org-next-visible-heading 1))

  ;; This tweaked command calls `ps/org-cycle-global' instead.
  ;; [2022-06-19 Sun] No longer using `ps/org-cycle-global' so this
  ;; command isn't used either.
  (defun ps/org-shifttab (&optional arg)
    "Global visibility cycling or move to previous table field.
Call `org-table-previous-field' within a table.
When ARG is nil, cycle globally through visibility states.
When ARG is a numeric prefix, show contents of this level."
    (interactive "P")
    (cond
     ((org-at-table-p) (call-interactively 'org-table-previous-field))
     ((integerp arg)
      (let ((arg2 (if org-odd-levels-only (1- (* 2 arg)) arg)))
        (message "Content view to level: %d" arg)
        (org-cycle-content (prefix-numeric-value arg2))
        (org-cycle-show-empty-lines t)
        (setq org-cycle-global-status 'overview)
        (run-hook-with-args 'org-cycle-hook 'overview)))
     (t (call-interactively 'ps/org-cycle-global))))

  (defun ps/org-super-return (&optional indent arg interactive)
    "When `org-return-follows-link' is non-nil and point is on a
link, call `org-open-at-point' and set
`browse-url-browser-function' to `eww-browse-url'"
    (interactive "P")
    (let ((browse-url-browser-function 'eww-browse-url)
          (browse-url-handlers nil))
      (org-open-at-point)))

  (defun ps/backward-org-transpose-element ()
    "Transpose current and previous elements, keeping blank lines between.
Point is moved after both elements."
    (interactive)
    (org-skip-whitespace)
    (let ((end (org-element-property :end (org-element-at-point))))
      (org-drag-element-forward)
      (goto-char end)))

  (defun ps/org-clear-heading-contents (&optional include-children include-properties)
    "Remove contents in org heading at point."
    (interactive)
    (save-restriction
      (if include-children
          (ps/org-narrow-to-entry-and-children)
        (ps/org-narrow-to-entry-no-children))
      (org-back-to-heading)
      (if include-properties
          (forward-line)
        (org-end-of-meta-data t))
      (delete-region (point) (point-max))))

  (defun ps/org-paste-html ()
    "Take the contents of the system clipboard and use `pandoc' to
convert it to `org-mode' format."
    (interactive)
    (let* ((clipboard (if (eq system-type 'darwin)
                          "pbv public.html"
                        "xclip -out -selection 'clipboard' -t text/html"))
           (pandoc (concat "pandoc --wrap=none -f html -t org"))
           (cmd (concat clipboard " | " pandoc))
           (output (shell-command-to-string cmd))
           ;; Not sure why Pandoc adds these double slashes; we remove them
           (output (replace-regexp-in-string "^\\\\\\\\$" "" output))
           (text (replace-regexp-in-string "= " "= " output)))
      (kill-new text)
      (yank)))

  (defun ps/org-paste-image ()
    "Take the contents of the system clipboard and paste it as an
image."
    (interactive)
    (if (executable-find "pngpaste")
        (let* ((counter 1)
               (image-file (concat
                            ps/dir-org-images
                            (org-id-get nil 'create)
                            (format "-%d.png" counter))))
          (while (file-exists-p image-file)
            (setq counter (1+ counter))
            (setq image-file (concat
                              ps/dir-org-images
                              (org-id-get nil 'create)
                              (format "-%d.png" counter))))
          (call-process-shell-command (format "pngpaste '%s'" image-file))
          (let ((caption (read-string "Caption: ")))
            (unless (string= caption "")
              (insert (format "#+CAPTION: %s \n" caption))))
          (insert (format "[[file:%s]]" image-file))
          (org-display-inline-images)
          (message "You can toggle inline images with C-c C-x C-v"))
      (user-error "Requires pngpaste in PATH")))

  :general
  (org-mode-map
   "<S-left>" nil
   "<S-right>" nil
   "<S-up>" nil
   "<S-down>" nil
   "<M-left>" nil
   "<M-right>" nil
   "<M-S-left>" nil
   "<M-S-right>" nil
   "<M-up>" nil
   "<M-down>" nil
   "C-j" nil
   "<backtab>" 'org-shifttab
   "s-<return>" 'ps/org-super-return
   "C-k" nil
   "C-," nil
   "A-C-s-i" 'org-backward-sentence
   "A-C-s-o" 'org-forward-sentence
   "A-C-s-," 'org-backward-paragraph
   "A-C-s-." 'org-forward-paragraph ; org element?
   "A-C-s-m" 'org-beginning-of-line
   "A-C-s-z" 'org-end-of-line ; karabiner maps `/' to `z'; otherwise I can't trigger the command while holding `shift'
   "A-C-s-r" 'org-previous-visible-heading
   "A-C-s-f" 'org-next-visible-heading
   "A-C-s-n" 'ps/org-jump-to-beginning-of-heading ; move to beginning of heading
   "A-C-s-M-m" 'org-previous-block
   "A-C-s-M-/" 'org-next-block
   "A-H-M-t" 'org-transpose-element
   "H-s-o" 'org-open-at-point
   "A-C-s-n" 'ps/org-jump-to-first-heading
   "s-A-b" 'ps/org-set-todo-properties
   "s-d" 'org-deadline
   "s-e" 'org-set-effort
   "s-f" 'org-insert-todo-subheading
   "s-A-f" 'ps/org-insert-todo-subheading-after-body
   "s-h" 'ps/org-copy-heading-name
   "s-A-h" 'ps/org-copy-heading-contents
   "s-p" 'org-time-stamp-inactive
   "s-A-p" 'org-time-stamp
   "s-q" 'org-set-tags-command
   "s-A-s" 'org-schedule
   ;; "s-A-s" 'ps/org-isearch-visible-org-heading
   "s-t" 'org-todo
   "s-A-t" 'org-sort
   "s-v" 'ps/org-paste-html
   "s-A-v" 'ps/org-paste-image
   "s-y" 'org-evaluate-time-range
   "s-A-y" 'ps/org-open-at-point-with-eww
   "s-z" 'org-edit-special
   "s-A-z" 'ps/org-export-to-ea-wiki
   "s-," 'org-priority
   "A-<return>" "C-u M-<return>"
   "A-M-<return>" 'org-insert-todo-heading
   ;; bindings with matching commands in Fundamental mode
   "H-v" 'org-yank
   "M-w" 'ps/org-count-words)
  (org-agenda-mode-map
   "s-s" 'org-save-all-org-buffers)
  (telega-chat-mode-map
   "s-b" (lambda! (org-emphasize ?*))
   "s-i" (lambda! (org-emphasize ?/))
   "s-e" (lambda! (org-emphasize ?~))))

(use-feature org-agenda
  :if (equal (system-name) ps/computer-hostname-pablo)
  :demand t
  :init
  (defun ps/org-agenda-switch-to-agenda-current-day ()
    "Open agenda in left window, creating it if necessary."
    (interactive)
    (ps/window-split-if-unsplit)
    (winum-select-window-1)
    (let ((agenda "*Org Agenda(a)*"))
      (if (get-buffer agenda)
          (switch-to-buffer "*Org Agenda(a)*")
        (org-agenda nil "a"))))

  (setq org-agenda-hide-tags-regexp "project")

  (defun ps/org-agenda-goto-and-start-clock ()
    "In org-agenda, go to entry at point and clock in."
    (interactive)
    (org-agenda-goto)
    (org-clock-in))

  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-use-time-grid nil)
  (org-agenda-ignore-properties '(effort appt category))
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-sticky t)
  (org-agenda-todo-ignore-with-date t)       ; exclude tasks with a date.
  (org-agenda-todo-ignore-scheduled 'future) ; exclude scheduled tasks.
  (org-agenda-restore-windows-after-quit t)  ; don't destroy window splits
  (org-agenda-span 1)                        ; show daily view by default
  (org-agenda-clock-consistency-checks       ; highlight gaps of five or more minutes in agenda log mode
   '(:max-duration "5:00" :min-duration "0:01" :max-gap 5 :gap-ok-around ("2:00")))
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-log-mode-items '(clock))
  (quote (:link t :maxlevel 5 :fileskip0 t :narrow 70 :formula % :indent t :formatter ps/org-clocktable-sorter))
  (org-agenda-custom-commands
   '(
     ("E" "TODOs without effort"
      ((org-ql-block '(and (todo)
                           (not (property "effort")))
                     ((org-ql-block-header "TODOs without effort")))))
     ("w" "Weekly review"
      agenda ""
      ((org-agenda-clockreport-mode t)
       (org-agenda-archives-mode t)
       (org-agenda-start-day "-7d")
       (org-agenda-span 7)
       (org-agenda-start-on-weekday 0)))
     ("p" "Appointments" agenda* "Today's appointments"
      ((org-agenda-span 1)
       (org-agenda-max-entries 3)))
     ("r"
      "Reading list"
      tags
      "PRIORITY=\"1\"|PRIORITY=\"2\"|PRIORITY=\"3\"|PRIORITY=\"4\"|PRIORITY=\"5\"|PRIORITY=\"6\"|PRIORITY=\"7\"|PRIORITY=\"8\"|PRIORITY=\"9\""
      ((org-agenda-files (list ps/dir-bibliographic-notes))))
     ("g" "All TODOs"
      todo "TODO")
     ("," "All tasks with no priority"
      tags-todo "-PRIORITY=\"1\"-PRIORITY=\"2\"-PRIORITY=\"3\"-PRIORITY=\"4\"-PRIORITY=\"5\"-PRIORITY=\"6\"-PRIORITY=\"7\"-PRIORITY=\"8\"-PRIORITY=\"9\"")))
  (org-agenda-clockreport-parameter-plist
   )
  :config
  (cond ((equal (system-name) ps/computer-hostname-pablo)
         (setq org-agenda-files nil))
        ((equal (system-name) ps/computer-hostname-leo)
         (setq org-agenda-files (list
                                 ps/file-tlon-tareas-leo))))

  (when (equal (system-name) ps/computer-hostname-pablo)
    (setq ps/org-agenda-files-excluded
      (list
       ;; I have to exclude these files because otherwise extraneous
       ;; information shows up in my agenda, such as TODOs and time
       ;; logs. These files lack the `property' tag but the may still
       ;; otherwise be included if they have been modified recently
       ;; (see the function `vulpea-agenda-files-update')
       ps/file-tlon-tareas-leo
       ps/file-tlon-tareas-fede)))

  (run-with-idle-timer 602 nil 'ps/org-agenda-switch-to-agenda-current-day)
  (run-with-idle-timer 602 t 'org-agenda-redo)

  (defun ps/org-clocktable-sorter (ipos tables params)
    (setq tables (cl-sort tables (lambda (table1 table2) (> (nth 1 table1) (nth 1 table2)))))
    (funcall (or org-clock-clocktable-formatter 'org-clocktable-write-default) ipos tables params))

  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (ps/org-narrow-to-entry-and-children)))

  (setq org-agenda-archives-mode 'trees) ; this variable is non-customizable, so won't work in `:custom'

  (defun ps/org-agenda-done-and-next ()
    "Temporary command to address bug when setting status via
`org-agenda-todo'."
    (interactive)
    (org-agenda-goto)
    (org-todo "DONE")
    (ps/org-agenda-switch-to-agenda-current-day)
    (org-agenda-next-line))

  (defun ps/org-agenda-postpone-and-next ()
    "Postpone task at point by one day and move to next task."
    (interactive)
    (org-agenda-date-later 1)
    (org-agenda-next-line))

  (defun ps/org-unhighlight ()
    "Interactive version of `org-unhighlight'."
    (interactive)
    (org-unhighlight))

  ;; Replace native function with variant that doesn't ask the user
  ;; multiple times to remove non-existent agenda file
  (defun ps/org-check-agenda-file (file)
    "Make sure FILE exists.  If not, ask user what to do."
    (unless (file-exists-p file)
      (org-remove-file file)
      (throw 'nextfile t)))

  (advice-add 'org-check-agenda-file :override #'ps/org-check-agenda-file)

  ;; We add these two advices because `org-modern-mode' interferes
  ;; with the display of the habits progress bar
  (advice-add 'org-habit-toggle-display-in-agenda :before
              (lambda (arg)
                (if org-habit-show-habits
                    (global-org-modern-mode)
                  (global-org-modern-mode -1))))
  (advice-add 'org-habit-toggle-display-in-agenda :after
              (lambda (arg)
                (org-agenda-redo)
                (global-org-modern-mode)))

  :hook
  (org-agenda-mode-hook . (lambda ()
                            (visual-line-mode -1)
                            (toggle-truncate-lines 1)))
  :general
  ("C-<escape>" 'org-agenda)
  (org-agenda-mode-map
   "'" 'ps/org-agenda-done-and-next
   ";" 'org-agenda-later
   "\"" 'ps/org-agenda-postpone-and-next
   "C-b" 'org-agenda-tree-to-indirect-buffer
   "C-k" nil
   "d" 'org-agenda-deadline
   "h" 'org-habit-toggle-display-in-agenda
   "M-t" nil
   "H-n" nil
   "s-k" nil
   "i" 'org-agenda-clock-in
   "I" 'org-agenda-diary-entry
   "j" 'org-agenda-earlier
   "J" 'org-agenda-goto-date
   "k" 'org-agenda-previous-line
   "l" 'org-agenda-next-line
   "n" 'org-agenda-date-later
   "o" 'org-agenda-open-link
   "p" 'org-agenda-date-earlier
   "q" 'org-agenda-kill-all-agenda-buffers
   "s" 'org-agenda-schedule
   "SPC" 'ps/org-agenda-goto-and-start-clock
   "W" 'org-agenda-refile
   "X" 'org-agenda-exit
   "x" 'org-agenda-log-mode
   "y" 'org-agenda-day-view
   "Z" 'org-agenda-add-note
   "z" 'org-agenda-undo))

(use-feature org-capture
  ;; :defer 10
  :demand t
  :custom
  (org-default-notes-file ps/file-inbox-desktop)
  (org-capture-templates
   `(("a" "Entry: add" entry
      (id "6E725763-8B53-43EC-9197-C0FE7468328D")
      "*** %(ps/org-web-tools-insert-title-for-clipboard-url-ea-forum)\nSCHEDULED: %(org-insert-time-stamp nil nil nil nil nil \" .+1d\")\n[[%c][online entry]]\n\nwber%?" :empty-lines 1 :prepend t)
     ("b" "Library" entry
      (id "ABE4DDD1-8107-487A-B09F-2FE7466D23DA")
      "* %?\n" :prepend t)
     ("c" "calendar" entry
      (file ps/file-calendar)
      "* TODO %^ \nDEADLINE: %^T" :empty-lines 1 :immediate-finish t)
     ("d" "Entry: discuss" entry
      (id "AE09BAB8-5EA0-4CB0-AC5A-FF876CB9ABC5")
      "**** TODO Discuss %(ps/org-web-tools-insert-link-for-clipboard-url-ea-forum)\nSCHEDULED: %(org-insert-time-stamp nil nil nil nil nil \" .+1d\")" :empty-lines 1 :prepend t)
     ("e" "email" entry
      (id "4388B4D0-3830-48E0-A118-C3195B62F0D1")
      "** TODO Follow up with %:fromname on %a\nSCHEDULED: %t\n\n%i" :immediate-finish t :empty-lines 1 :prepend t)
     ("f" "Fede")
     ("ff" "Fede: generic task" entry
      (file+headline ps/file-tlon-tareas-fede "Tareas Fede")
      "** TODO [#6] %? :fede:\n" :empty-lines 1 :prepend t)
     ("fp" "Fede: Pending for next meeting" plain
      (id "AAB63566-B9AD-4BA3-96E9-0F3F0A26E2B1")
      "" :empty-lines 1 :empty-lines-after 3)
      ("fr" "Fede: RAE suggestion" entry
      (file+headline ps/file-tlon-tareas-fede "sugerencias de Pablo")
      "** TODO [#6] %? :fede:\n" :empty-lines 1 :prepend t)
     ("g" "Ledger" plain (file ps/file-ledger)
      "" :empty-lines 1)
     ("l" "Leo")
     ("lb" "Leo: Add to ea.news" entry
      (id "8b9c313a-3630-4b77-b924-a8f7f9e52d8d")
      "** TODO [#6] Agregar a ea.news :leo:\n%c" :empty-lines 1 :prepend t)
     ("ll" "Leo: Generic task" entry
      (file+headline ps/file-tlon-tareas-leo "Tareas Leo")
      "** TODO [#6] %? :leo:\n" :empty-lines 1 :prepend t)
     ("lm" "Leo: Meetings" entry
      (id "51610BEB-7583-4C84-8FC2-A3B28CA79FAB")
      "** %(ps/org-time-stamp-inactive-current-time)\n%?")
     ("ln" "Leo: Add to Future Matters: news" entry
      (id "5d94a97f-701f-4d0d-94ad-ff1b88bf0e82")
      "** TODO [#4] Future Matters: news :leo:\n%c\n[[https://docs.google.com/document/d/1Mq7f0sn6Ps1IIA71dTu0MCgz8cdn81zQ9_zHyZUn7aQ/edit][Checklist]]" :empty-lines 1 :prepend t)
     ("lp" "Leo: Pending for next meeting" plain
      (id "8B2F18B4-A309-4F29-A5E6-CD40E010970D")
      "" :empty-lines 1 :empty-lines-after 3)
     ("lr" "Leo: Add to Future Matters: research" entry
      (id "5d94a97f-701f-4d0d-94ad-ff1b88bf0e82")
      "** TODO [#4] Future Matters: research :leo:\n%c\n[[https://docs.google.com/document/d/1Mq7f0sn6Ps1IIA71dTu0MCgz8cdn81zQ9_zHyZUn7aQ/edit][Checklist]]" :empty-lines 1 :prepend t)
     ("lt" "Leo: Add to translations Aritable" entry
      (id "49adbb3e-b542-4752-a67b-16bc2eb44624")
      "** TODO [#4] Add to translations Airtable :leo:\n%c\nPrioridad: %(completing-read \"Priority\" '(\"highest\" \"high\" \"medium\" \"low\" \"lowest\"))" :empty-lines 1 :prepend t)
     ("lg" "Leo: Telegram" entry
      (file+headline ps/file-tlon-tareas-leo "Tareas Leo")
      "** TODO [#6] [via Telegram] %? \n%a\n%c'" :empty-lines 1 :prepend t)
     ("m" "Leo: Messaging (to send later)" entry
      (id "4388B4D0-3830-48E0-A118-C3195B62F0D1")
      "** TODO Send message\n%?\n")
     ("n" "telegram" entry
     (id "4388B4D0-3830-48E0-A118-C3195B62F0D1")
      "** TODO Follow up with %a\nSCHEDULED: %t\n\n%i" :immediate-finish t :empty-lines 1 :prepend t)
     ;; ("m" "To discuss in meeting with Leo" plain
     ;; (id "3AAD2510-0522-4598-9182-50E97504EAF6")
     ;; "- [ ] %?" :empty-lines 1)
     ("r" "bibliography reference" plain
      (file ,ps/file-orb-noter-template)
      :if-new
      (file ,ps/file-orb-capture-template)
      :unnarrowed t :immediate-finish t)
     ("s" "Slack" entry
      (id "4388B4D0-3830-48E0-A118-C3195B62F0D1")
      "** TODO Follow up %a\nSCHEDULED: %t\n\n%i" :immediate-finish t :empty-lines 1 :prepend t)
     ("t" "Todo" entry
      (id "4388B4D0-3830-48E0-A118-C3195B62F0D1")
      "** TODO %?\n" :empty-lines 1)
     ;; ("n" "Day reflection" plain (function org-journal-find-location)
     ;; "** %(format-time-string org-journal-time-format)Day reflection\n%i%?")
     ;; ("j" "Pomodoro" plain (function org-journal-find-location)
     ;; "** %(format-time-string org-journal-time-format)Pomodoro\n%i%?")
     ;; ("v" "Entry: revise" entry
     ;; (function
     ;; (lambda ()
     ;; (let ((filename (file-name-concat ps/dir-wiki-entries (current-kill 0))))
     ;; (set-buffer (find-file-noselect filename))
     ;; (pop kill-ring)
     ;; (goto-char (point-max)))))
     ;; "* TODO Revise entry \nSCHEDULED: %(org-insert-time-stamp nil nil nil nil nil \" .+1d\")")
     ("y" "YouTube playlist" entry
      (id "319B1611-A5A6-42C8-923F-884A354333F9")
      "* %(ps/org-web-tools--youtube-dl (current-kill 0))\n[[%c][YouTube link]]" :empty-lines 1 :prepend t :immediate-finish t)
     ;; github.com/alphapapa/org-protocol-capture-html#org-capture-template
     ("w" "Web site" entry
      (file ps/file-downloadsk)
      "* %a :website:\n\n%U %?\n\n%:initial")))
  ;; ("w" "Film watchlist" entry
  ;; (id "E821F19E-C619-4895-A084-54D0A2772BAE")
  ;; "** %?\n" :empty-lines 1 :prepend t)))
  ;; ("W" "Weekly review" plain (function org-journal-find-location)
  ;; "** %(format-time-string org-journal-time-format)Weekly review\n*** Mistakes made\n%?\n*** Lessons learned\n")
  ;; ("z" "mailnote" entry
  ;; (id "0D266C71-41B8-4E14-836E-AABE2654E942")
  ;; "** From: %:from Subject: %:subject\n %a" :kill-buffer t)
  ;; ("p" "Pomodoro entry" plain (function org-journal-find-location)
  ;; "** %(format-time-string org-journal-time-format)Pomodoro\n%i%?")

  :config
  (defun ps/org-capture-hydra-notes-hook ()
    (when (string= (org-capture-get :key t) "p")
      (hydra-org-notes-only-clock/body)))

  (defun ps/org-capture-prepare-finalize-hook-behavior ()
    "docstring"
    (cond
     ((string= "fm" (plist-get org-capture-plist :key))
      (save-window-excursion
        (org-id-goto "22E9B7E2-A48E-41EA-8320-64578AB6C9A1")
        (org-narrow-to-subtree)
        (ps/org-copy-heading-contents))
      (insert (current-kill 0)))))

  (defun ps/org-capture-before-finalize-hook-behavior ()
    "Define behavior of `org-capture-before-finalize-hook'
conditional on active capture template."
    (cond
     ((string= "g" (plist-get org-capture-plist :key))
      (ledger-post-align-xact (point)))
     ((string= "l" (plist-get org-capture-plist :key))
      (org-align-all-tags)
      (ispell-change-dictionary "english"))
     ((string= "la" (plist-get org-capture-plist :key))
      (save-window-excursion
        (ps/switch-to-alternate-buffer)
        (ps/org-jump-to-first-heading)
        (widen)
        (org-narrow-to-subtree)
        (let ((org-use-tag-inheritance))
          (org-roam-tag-remove '("unprocessed" "empty" "leo" "unpublished"))
          (org-roam-tag-add '("leo")))
        (ps/show-buffer-name))
      (goto-char 0)
      (search-forward "Procesar ")
      (insert (format "~%s~" (current-kill 0))))
     ((string= "le" (plist-get org-capture-plist :key))
      (save-window-excursion
        (ps/switch-to-alternate-buffer)
        (ps/show-buffer-name))
      (goto-char 0)
      (search-forward "Renombrar ")
      (insert (concat "~" (current-kill 0) "~ ")))
     ;; Add link to open Slack message externally.
     ((string= "s" (plist-get org-capture-plist :key))
      (org-narrow-to-subtree)
      (let ((url (s-replace-regexp
                  "emacs-slack:[_[:digit:][:alnum:]]\\{11\\}&\\([_[:digit:][:alnum:]]\\{11\\}\\)&ts:\\([[:digit:]]\\{10\\}\\)\\.\\([[:digit:]]\\{6\\}\\)"
                  "https://samotsvety.slack.com/archives/\\1/p\\2\\3"
                  (plist-get org-store-link-plist :link))))
        (goto-char (point-max))
        (insert (format "[[%s][external link]]" url))))
     ((string= "v" (plist-get org-capture-plist :key))
      (org-do-demote))
     ((string= "y" (plist-get org-capture-plist :key))
      (youtube-dl (current-kill 0) :directory ps/dir-downloads :destination (org-hugo-slug (ps/org-web-tools--org-title-for-url))))))


  :general
  ("H-t" 'org-capture
   "H-T" 'org-capture-goto-last-stored)
  (org-capture-mode-map
   "s-c" 'org-capture-finalize
   "s-w" 'org-capture-refile)

  :hook
  (org-capture-mode-hook . ps/org-capture-hydra-notes-hook)
  (org-capture-before-finalize-hook . ps/org-capture-before-finalize-hook-behavior)
  (org-capture-before-prepare-hook . ps/org-capture-prepare-finalize-hook-behavior))

(use-feature org-clock
  :defer 20
  :custom
  (org-clock-out-when-done t)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-in-resume t)
  (org-clock-report-include-clocking-task t)
  (org-clock-ask-before-exiting nil)
  :config
  (org-clock-persistence-insinuate)

  ;; FIXME: suggested by copilot; not working: the date entities are not being
  ;; converted properly
  (defun ps/org-clock-create-clock-entry ()
    "Create a new clock entry for the org heading at point,
 prompting the user for start and end times."
    (interactive)
    (let* ((start-time (org-read-date nil t nil "Start time: "))
           (end-time (org-read-date nil t nil "End time: "))
           (start-time (date-to-time start-time))
           (end-time (org-time-string-to-time end-time))
           (duration (org-time-subtract end-time start-time))
           (duration (org-duration-from-minutes (org-duration-to-minutes duration))))
      (org-clock-in '(16))
      (org-clock-modify-effort-estimate duration)))

  (defun ps/org-new-clock-entry-today (begin end)
    "Insert a new clock entry with today's date, prompting for times."
    (interactive "sTime begins: \nsTime ends: ")
    (ps/org-jump-to-latest-clock-entry)
    (crux-smart-open-line-above)
    (let ((today (format-time-string "%Y-%m-%d %a" (current-time))))
      (insert "CLOCK: [%s %s]--[%s %s]" today begin today end))
    (org-evaluate-time-range))

  (defun ps/org-time-stamp-active-current-time ()
    "Insert an active timestamp with the current date and time."
    (interactive)
    (org-time-stamp '(16)))

  (defun ps/org-time-stamp-inactive-current-time ()
    "Insert an inactive timestamp with the current date and time."
    (interactive)
    (org-time-stamp '(16) t))

  (defun ps/org-time-stamp-active-current-date ()
    "Insert an active timestamp with the current date."
    (interactive)
    (org-insert-time-stamp (current-time) nil))

  (defun ps/org-time-stamp-inactive-current-date ()
    "Insert an inactive timestamp with the current date."
    (interactive)
    (org-insert-time-stamp (current-time) nil t))

  (defun ps/org-clone-clock-entry ()
    "Duplicate clock entry at point and set mark for editing via multiple cursors."
    (interactive)
    (crux-duplicate-current-line-or-region 1)
    (org-beginning-of-line)
    (forward-line -1)
    (let ((query "\\[\\([[:digit:]]\\)\\{4\\}-\\([[:digit:]]\\)\\{2\\}-\\([[:digit:]]\\)\\{2\\} \\w\\w\\w "))
      (search-forward-regexp query)
      (set-mark-command nil)
      (set-mark-command nil)
      (next-line)
      (search-forward-regexp query)))

  (defun ps/org-jump-to-latest-clock-entry ()
    "Jump to most recent clock entry for org heading at point."
    (interactive)
    (org-back-to-heading)
    (isearch-forward nil t)
    (isearch-yank-string "CLOCK: ")
    (isearch-exit)
    (org-beginning-of-line))

  (defun ps/org-clock-report (start-date end-date)
    "Generate an org clock report for the period between START-DATE and END-DATE."
    (interactive
     (list (org-read-date nil nil nil "Start date: ")
           (org-read-date nil nil nil "End date: ")))
    (insert (format "#+BEGIN: clocktable :scope subtree :maxlevel 4 :narrow 50 :tstart \"%s\" :tend \"%s\"\n#+END:" start-date end-date))
    (previous-line)
    (org-ctrl-c-ctrl-c))

  :general
  ("A-H-j" 'org-clock-goto
   "A-H-x" 'org-clock-cancel)
  (org-mode-map
   "s-i" 'org-clock-in
   "s-o" 'org-clock-out
   "s-u" 'ps/org-clone-clock-entry
   "s-A-n" 'ps/org-new-clock-entry-today
   "s--" 'ps/org-time-stamp-active-current-time
   "s-A--" 'ps/org-time-stamp-active-current-date
   "s-=" 'ps/org-time-stamp-inactive-current-time
   "s-A-=" 'ps/org-time-stamp-inactive-current-date))

(use-feature org-cycle
  :custom
  (org-cycle-emulate-tab nil "TAB always cycles, even if point not on a heading")

  :config
  (defun ps/org-cycle-global (&optional arg)
    "Cycle the global visibility, hiding archived subtrees."
    (interactive)
    (org-cycle-global arg)
    (org-cycle-hide-archived-subtrees 'all)))

(use-feature org-archive
  :custom
  (org-archive-default-command 'org-archive-to-archive-sibling)
  (org-archive-location (expand-file-name "%s_archive.org::" ps/dir-archive))

  :config
  ;; Based on stackoverflow.com/a/27043756/4479455
  (defun ps/org-archive-done-tasks-in-file ()
    "Archive all DONE tasks in file."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-to-archive-sibling)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file))

  (defun ps/org-mark-as-done-for-good-and-archive ()
    "Mark task as DONE, removing any schedules and deadlines, if
present, then archive it."
    (interactive)
    (org-schedule '(4) nil)
    (org-deadline '(4) nil)
    (org-todo "DONE")
    (org-archive-to-archive-sibling))

  :general
  ("A-s-d" 'ps/org-mark-as-done-for-good-and-archive)
  (org-mode-map
   "s-a" 'org-archive-to-archive-sibling
   "s-A-a" 'org-archive-subtree))

(use-package org-archive-hierarchically
  :disabled
  :straight (org-archive-hierarchically
             :host gitlab
             :repo "andersjohansson/org-archive-hierarchically")
  :demand t)

(use-feature org-fold
  :custom
  (org-fold-catch-invisible-edits 'smart)

  :config
  (defun ps/org-fold-show-all-headings ()
    "Show contents of all headings in buffer, except archives."
    (interactive)
    (org-fold-show-all '(headings))
    (org-cycle-hide-archived-subtrees 'all))

  ;; github.com/org-roam/org-roam/wiki/User-contributed-Tricks#hiding-the-properties-drawer
  (defun ps/org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be
slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun ps/org-hide-logbook ()
    "Hide all org-mode headline logbook drawers in buffer. Could be
slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:logbook:\n\\(^clock:.*?\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-logbook-drawer t))))
    (put 'org-toggle-logbook-hide-state 'state 'hidden))

  (defun ps/org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun ps/org-show-logbook ()
    "Show all org-mode logbook drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-logbook-drawer t)
    (put 'org-toggle-logbook-hide-state 'state 'shown))

  (defun ps/org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (ps/org-show-properties)
      (ps/org-hide-properties)))

  (defun ps/org-toggle-logbook ()
    "Toggle visibility of logbook drawers."
    (interactive)
    (if (eq (get 'org-toggle-logbook-hide-state 'state) 'hidden)
        (ps/org-show-logbook)
      (ps/org-hide-logbook)))

  :hook
  (org-mode-hook . ps/org-hide-properties)
  (org-mode-hook . ps/org-hide-logbook))

(use-feature org-id
  :demand t
  :custom
  (org-id-link-to-org-use-id t)

  :init
  ;; stackoverflow.com/a/16247032/4479455
  (defvar ps/org-id-excluded-directories
    (list ps/dir-dropbox-tlon-fede
          ps/dir-dropbox-tlon-leo)
    "Directories containing files to exclude from `ps/org-id-add-ids-to-headings-in-file'")

  (defvar ps/org-id-excluded-files
    (list ps/file-orb-noter-template)
    "Files to exclude from `ps/org-id-add-ids-to-headings-in-file'")

  (defun ps/org-id-add-ids-to-headings-in-file ()
    "Add ID properties to all headings in the current file which do
not already have one."
    (when (and (equal (system-name) ps/computer-hostname-pablo)
               (eq major-mode 'org-mode)
               (string-match ps/dir-org (buffer-file-name))
               (eq buffer-read-only nil))
      (unless
          (or
	   ;; exclude directories
           (member (file-name-directory (buffer-file-name))
                   ps/org-id-excluded-directories)
	   ;; exclude files
           (member (buffer-file-name)
		   ps/org-id-excluded-files)
           (member (org-get-heading)
                   '("Local variables"
                     "COMMENT Local variables"
                     "TODO Local variables")))
        (org-map-entries 'org-id-get-create))))

  ;; (setq org-id-extra-files
  ;; (directory-files-recursively ps/dir-org "\\.org"))

  :config
  ;; copied from emacs.stackexchange.com/a/58834/32089
  ;; TODO: add support for scanning file at point
  (defun ps/org-id-update-id-locations (&optional files silent)
    "Scan relevant files for IDs.
Store the relation between files and corresponding IDs.
This will scan all agenda files, all associated archives, and all
files currently mentioned in `org-id-locations'.
When FILES is given, scan these files instead."
    (interactive)
    (if (not org-id-track-globally)
        (error "Please turn on `org-id-track-globally' if you want to track IDs")
      (let* ((org-id-search-archives
              (or org-id-search-archives
                  (and (symbolp org-id-extra-files)
                       (symbol-value org-id-extra-files)
                       (member 'agenda-archives org-id-extra-files))))
             (files
              (or files
                  (append
                   ;; Agenda files and all associated archives
                   (org-agenda-files t org-id-search-archives)
                   ;; Explicit extra files
                   (if (symbolp org-id-extra-files)
                       (symbol-value org-id-extra-files)
                     org-id-extra-files)
                   ;; Files associated with live Org buffers
                   (delq nil
                         (mapcar (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'org-mode) (buffer-file-name))))
                                 (buffer-list)))
                   ;; All files known to have IDs
                   org-id-files)))
             org-agenda-new-buffers
             file nfiles tfile ids reg found id seen (ndup 0))
        (when (member 'agenda-archives files)
          (setq files (delq 'agenda-archives (copy-sequence files))))
        (setq nfiles (length files))
        (while (setq file (pop files))
          (unless silent
            (message "Finding ID locations (%d/%d files): %s"
                     (- nfiles (length files)) nfiles file))
          (setq tfile (file-truename file))
          (when (and (file-exists-p file) (not (member tfile seen)))
            (push tfile seen)
            (setq ids nil)
            (with-current-buffer (org-get-agenda-file-buffer file)
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"
                                            nil t)
                    (setq id (match-string-no-properties 1))
                    (if (member id found)
                        (progn
                                        ;added logic
                          (if org-clone-delete-id
                              (org-entry-delete nil "ID")
                            (org-id-get-create t))
                                        ;end of added logic
                          (message "Duplicate ID \"%s\", also in file %s"
                                   id (or (car (delq
                                                nil
                                                (mapcar
                                                 (lambda (x)
                                                   (if (member id (cdr x))
                                                       (car x)))
                                                 reg)))
                                          (buffer-file-name)))
                          (when (= ndup 0)
                            (ding)
                            (sit-for 2))
                          (setq ndup (1+ ndup)))
                      (push id found)
                      (push id ids)))
                  (push (cons (abbreviate-file-name file) ids) reg))))))
        (org-release-buffers org-agenda-new-buffers)
        (setq org-agenda-new-buffers nil)
        (setq org-id-locations reg)
        (setq org-id-files (mapcar 'car org-id-locations))
        (org-id-locations-save) ;; this function can also handle the alist form
        ;; now convert to a hash
        (setq org-id-locations (org-id-alist-to-hash org-id-locations))
        (if (> ndup 0)
            (message "WARNING: %d duplicate IDs found, check *Messages* buffer" ndup)
          (message "%d unique files scanned for IDs" (length org-id-files)))
        org-id-locations)))

  :hook
  (before-save-hook . ps/org-id-add-ids-to-headings-in-file)

  :general
  (org-mode-map
   "s-A-i" 'org-id-copy
   "s-A-u" 'ps/org-id-update-id-locations))

(use-feature org-list
  :custom
  (org-plain-list-ordered-item-terminator ?.)
  (org-list-indent-offset 2)

  :config
  (defun ps/org-mark-checkbox-complete-and-move-to-next-item ()
    "Mark checkbox as completed and move to the next item."
    (interactive)
    (org-ctrl-c-ctrl-c nil)
    (let ((debug-on-error nil))
      (org-next-item)))

  ;; I should instead advice the original org function
  (defun ps/org-reset-checkbox-state-subtree ()
    "Reset all checkboxes in an entry subtree, without showing heading properties."
    (interactive)
    (org-reset-checkbox-state-subtree)
    (org-cycle)
    (org-cycle))

  :general
  (org-mode-map
   "s-A-c" 'ps/org-mark-checkbox-complete-and-move-to-next-item
   "s-A-o" 'ps/org-reset-checkbox-state-subtree))

(use-feature org-refile
  :defer 10
  ;; :demand t
  :custom
  (org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with helm/ivy
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-reverse-note-order t) ; refile to the beginning of header
  ;; Build cache at startup
  (org-refile-use-cache t)
  (org-refile-use-outline-path t)
  (org-refile-targets '((org-agenda-files :maxlevel . 9)
                        (ps/open-buffer-files :maxlevel . 9)))
  :config
  (defun ps/open-buffer-files ()
    "Return the list of files currently open in emacs"
    (delq nil
          (mapcar (lambda (x)
                    (if (and (buffer-file-name x)
                             (string-match "\\.org$"
                                           (buffer-file-name x)))
                        (buffer-file-name x)))
                  (buffer-list))))

  ;; Regenerate cache every half hour minutes
  (run-with-idle-timer (* 60 30) t (lambda ()
                                     (org-refile-cache-clear)
                                     (org-refile-get-targets)))
  ;; [2022-06-21] Replaced by `consult-org-heading'; consider
  ;; deleting.
  (defun ps/org-refile-jump (&optional arg)
    "Jump to heading in current buffer. With prefix argument,
refresh cache."
    (interactive "P")
    (widen)
    (let ((org-refile-targets '((nil :maxlevel . 9))))
      (when arg (org-refile 0))
      (org-refile '(4)))
    (ps/org-narrow-to-entry-and-children))

  (defun ps/org-refile-latest ()
    "Jump to the most recently refiled item."
    (interactive)
    (widen)
    (org-refile '(16)))

  (defun ps/org-refile-and-archive ()
    "Refile and archive the current subtree."
    (interactive)
    (org-refile)
    (save-excursion
      (org-refile '(16))
      (org-archive-to-archive-sibling))
      (org-mark-ring-goto))

  ;; Inspired by emacs.stackexchange.com/q/8045/32089
  (defun ps/org-refile-to (file heading)
    "Refile current heading to specified location."
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer heading))))
      (org-refile nil nil (list heading file nil pos))))

  (defun ps/org-refile-video-to-watched ()
    "Refile current heading to 'Watched' section."
    (interactive)
    (save-window-excursion (ps/org-refile-to "videos.org" "Watched")))

  ;; to make this command work, I need to find a way to extend the scope of the
  ;; `file' and `heading' temporary variables beyond the bounds of the excursion.
  ;; I can just define an ordinary variable with `setq' but that seems incorrect.
  (defun ps/org-refile-to-set-target ()
    "Refile current heading to target specified by the properties
`REFILE_TARGET_FILE' and `REFILE_TARGET_HEADING'."
    (interactive)
    ;; check that property exists; otherwise abort
    (save-excursion
      (org-narrow-to-subtree)
      (beginning-of-buffer)
      (widen)
      (org-next-visible-heading -1)
      (let ((file (org-entry-get nil "REFILE_TARGET_FILE"))
            (heading (org-entry-get nil "REFILE_TARGET_HEADING")))))
    (save-window-excursion (ps/org-refile-to file heading)))

  :general
  (org-mode-map
   "s-w" 'org-refile
   "s-A-w" 'ps/org-refile-latest))

(use-feature org-keys
  :custom
  (org-use-speed-commands t)
  (org-speed-commands
   '(("Outline navigation")
     ("k" . (org-speed-move-safe 'org-previous-visible-heading))
     ("." . (org-speed-move-safe 'org-forward-heading-same-level))
     ("," . (org-speed-move-safe 'org-backward-heading-same-level))
     ("l" . (org-speed-move-safe 'org-next-visible-heading))
     ("j" . (org-speed-move-safe 'outline-up-heading))
     ("m" . (org-previous-block nil))
     ("/" . (org-next-block nil))
     ("Outline structure editing")
     ("A" . (org-metaleft))
     ("D" . (org-metadown))
     ("S" . (org-metaup))
     ("F" . (org-metaright))
     ("Q" . (org-shiftmetaleft))
     ("E" . (org-shiftmetadown))
     ("W" . (org-shiftmetaup))
     ("R" . (org-shiftmetaright))
     ("Outline visibility")
     ("'" . (org-force-cycle-archived))
     ("w" . (ps/narrow-or-widen-dwim))
     ("Meta data editing")
     ("t" . (org-todo))
     ("Clock")
     ("h" . (ps/org-jump-to-latest-clock-entry))
     ("H" . (lambda () (ps/org-jump-to-latest-clock-entry) (ps/org-clone-clock-entry)))
     ("i" . (org-clock-in))
     ("o" . (org-clock-out))
     ("Regular editing")
     ("z" . (undo-tree-undo))
     ("X" . (org-cut-subtree)) ; capital 'X' to prevent accidents
     ("c" . (org-copy-subtree))
     ("v" . (org-yank))
     ("Other")
     ("f" . (ace-link-org))
     ("a" . (org-open-at-point nil))
     ("I" . (org-id-copy))
     ("p" . (org-priority))
     ("u" . (org-speed-command-help))
     ("g" . (org-agenda)))))

(use-feature ol
  :general
  ("H-L" 'org-store-link)
  (org-mode-map
   "A-C-M-s-j" 'org-previous-link
   "A-C-M-s-;" 'org-next-link)
  ((org-mode-map org-msg-edit-mode-map)
   "s-A-l" 'ps/org-url-dwim)
  ((org-mode-map org-msg-edit-mode-map telega-chat-mode-map)
   "s-k" 'org-insert-link))

(use-feature org-protocol
  :defer 15)

(use-package org-protocol-capture-html)

(use-feature ox
  :defer 30
  :custom
  (org-export-exclude-tags '("noexport" "ARCHIVE"))
  (org-export-backends '(ascii html icalendar latex md odt)) ; set export backends
  (org-export-with-broken-links 'mark) ; allow export with broken links
  (org-export-with-section-numbers nil) ; do not add numbers to section headings
  (org-export-with-toc nil) ; do not include table of contents
  (org-export-with-title nil) ; do not include title
  (org-export-headline-levels 4) ; include up to level 4 headlines
  (org-export-preserve-breaks t) ; respect single breaks when exporting
  ;; (org-export-with-author nil "do not include author")
  ;; (org-export-with-date nil "do not include export date")
  ;; (org-html-validation-link nil "do not include validation link")
  (org-html-postamble nil "the three lines above unnecessary when this set to nil")
  (org-latex-logfiles-extensions (quote
                                  ("lof" "lot" "tex" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "pygtex" "pygstyle")) "get rid of temporary LaTeX files upon export")
  (org-preview-latex-default-process 'dvisvgm)
  (org-export-show-temporary-export-buffer nil "bury temporary export buffers generated by `org-msg'")

  :general
  (org-mode-map
   "s-A-e" 'org-export-dispatch))

(use-package ox-hugo
  :defer 100)

(use-feature ob
  :custom
  (org-confirm-babel-evaluate 'ps/org-confirm-babel-evaluate)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  (defun ps/org-confirm-babel-evaluate (lang body)
    (not (member lang '("python" "emacs-lisp"))))

  (defvar version 'normal)
  (defun ps/org-babel-tangle-config-file (&optional arg)
    "Tangle all code blocks defined as part of a normal configuration.
If invoked with a prefix argument, tangle only code blocks
defined as part of a minimal configuration instead."
    (interactive "P")
    (widen)
    (save-buffer)
    (let ((version (if arg 'minimal 'normal)))
      (org-babel-tangle)))

  (add-to-list 'org-babel-key-bindings (cons "j" 'org-babel-next-src-block))
  (add-to-list 'org-babel-key-bindings (cons "k" 'org-babel-previous-src-block))
  (add-to-list 'org-babel-key-bindings (cons "n" 'org-babel-insert-header-arg))
  (add-to-list 'org-babel-key-bindings (cons "p" 'org-babel-remove-result-one-or-many))
  :general
  (org-mode-map
   "s-b" 'ps/org-babel-tangle-config-file))

(use-feature org-src
  :defer 5
  :custom
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation nil)
  (org-src-window-setup 'current-window)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively nil "When set to `nil', newlines will be properly indented")

  :config
  (defun ps/org-src--construct-edit-buffer-name (org-buffer-name lang)
    "Construct the buffer name for a source editing buffer. This
tweaked function names such buffers more cleanly than the
original."
    (concat org-buffer-name " (org src)"))

  (advice-add 'org-src--construct-edit-buffer-name :override #'ps/org-src--construct-edit-buffer-name)

  :general
  (org-src-mode-map
  "s-z" 'org-edit-src-exit))

(use-feature org-tempo
  :disabled)

(use-feature org-table
  :config
  (defun ps/org-table-copy-cell ()
    (interactive)
    (when (org-at-table-p)
      (kill-new
       (string-trim
        (substring-no-properties (org-table-get-field))))))

  :general
  ("H-s-c" 'ps/org-table-copy-cell)
  (org-table-fedit-map
   "s-c" 'org-table-fedit-finish))

(use-package orgtbl-edit
  :straight (orgtbl-edit
             :host github
             :repo "shankar2k/orgtbl-edit"))

(use-feature org-crypt
:demand t
  :custom
  (org-tags-exclude-from-inheritance '("crypt"))
  (org-crypt-key ps/personal-gmail)

  :config
  (org-crypt-use-before-save-magic))

(use-feature org-lint)

(use-feature org-habit
  :custom
  (org-habit-today-glyph #x1f4c5)
  (org-habit-completed-glyph #x2713)
  (org-habit-preceding-days 29)
  (org-habit-following-days 1)
  (org-habit-graph-column 3)
  (org-habit-show-habits nil)
  (org-habit-show-habits-only-for-today nil)

  :config
  ;; copied from github.com/progfolio/.emacs.d/blob/master/init.org#org-habit
  (defun ps/org-habit-graph-on-own-line (graph)
    "Place org habit consitency graph below the habit."
    (let* ((count 0)
           icon)
      (save-excursion
        (beginning-of-line)
        (while (and (eq (char-after) ? ) (not (eolp)))
          (when (get-text-property (point) 'display) (setq icon t))
          (setq count (1+ count))
          (forward-char)))
      (add-text-properties (+ (line-beginning-position) count) (line-end-position)
                           `(display ,(concat (unless icon "  ")
                                              (string-trim-left (thing-at-point 'line))
                                              (make-string (or org-habit-graph-column 0) ? )
                                              (string-trim-right
                                               (propertize graph 'mouse-face 'inherit)))))))

  (defun ps/org-habit-insert-consistency-graphs (&optional line)
    "Insert consistency graph for any habitual tasks."
    (let ((inhibit-read-only t)
          (buffer-invisibility-spec '(org-link))
          (moment (org-time-subtract nil
                                     (* 3600 org-extend-today-until))))
      (save-excursion
        (goto-char (if line (point-at-bol) (point-min)))
        (while (not (eobp))
          (let ((habit (get-text-property (point) 'org-habit-p)))
            (when habit
              (let ((graph (org-habit-build-graph
                            habit
                            (time-subtract moment (days-to-time org-habit-preceding-days))
                            moment
                            (time-add moment (days-to-time org-habit-following-days)))))
                (ps/org-habit-graph-on-own-line graph))))
          (forward-line)))))

  (advice-add #'org-habit-insert-consistency-graphs
              :override #'ps/org-habit-insert-consistency-graphs))

(use-package org-checklist
  :defer 30
  :straight org-contrib)

(use-package org-extra
  :disabled
  :defer 30
  :straight org-contrib
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package org-ql
  :disabled
  :custom
  (org-ql-search-directories-files-recursive t)
  :config
  (defun ps/org-ql-find-agenda ()
    "docstring"
    (interactive)
    (org-ql-find org-agenda-files :query-prefix "!tags:ARCHIVE")))

(use-package org-make-toc
  :after org
  :defer 10)

(defun ps/org-paste-clipboard-image ()
  ;; TODO: write docstring
  (interactive)
  (if (executable-find "pngpaste")
      (let* ((counter 1)
             (image-file (concat
                          ps/dir-org-images
                          (org-id-get nil 'create)
                          (format "-%d.png" counter))))
        (while (file-exists-p image-file)
          (setq counter (1+ counter))
          (setq image-file (concat
                            ps/dir-org-images
                            (org-id-get nil 'create)
                            (format "-%d.png" counter))))
        (call-process-shell-command (format "pngpaste '%s'" image-file))
        (let ((caption (read-string "Caption: ")))
          (unless (string= caption "")
            (insert (format "#+CAPTION: %s \n" caption))))
        (insert (format "[[file:%s]]" image-file))
        (org-display-inline-images)
        (message "You can toggle inline images with `C-c C-x C-v'"))
    (user-error "Requires pngpaste in PATH")))

(use-package org2blog
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after auth-source-pass
  :custom
  (org2blog/wp-blog-alist
   `(("Pablo's website"
      :url "https://www.stafforini.com/xmlrpc.php"
      :username ,(auth-source-pass-get "user" "chrome/stafforini.com/wp-admin/admin")
      :password ,(auth-source-pass-get 'secret "chrome/stafforini.com/wp-admin/admin"))
     ("Pablo's miscellany"
      :url "https://www.stafforini.com/blog/xmlrpc.php"
      :username ,(auth-source-pass-get "user" "chrome/stafforini.com/blog/wp-admin/admin")
      :password ,(auth-source-pass-get 'secret "chrome/stafforini.com/blog/wp-admin/admin"))
     ("notatu dignum"
      :url "https://www.stafforini.com/quotes/xmlrpc.php"
      :username ,(auth-source-pass-get "user" "chrome/stafforini.com/quotes/wp-admin/admin")
      :password ,(auth-source-pass-get 'secret "chrome/stafforini.com/quotes/wp-admin/admin"))
     ("Puro compás"
      :url "https://www.stafforini.com/tango/xmlrpc.php"
      :username ,(auth-source-pass-get "user" "chrome/stafforini.com/tango/wp-admin/admin")
      :password ,(auth-source-pass-get 'secret "chrome/stafforini.com/tango/wp-admin/admin"))
      ("EA Quotes"
      :url "https://eaquotes.net/xmlrpc.php"
      :username ,(auth-source-pass-get "user" "tlon/EAQUOTES/eaquotes.net/wp_admin@eaquotes.net")
      :password ,(auth-source-pass-get 'secret "tlon/EAQUOTES/eaquotes.net/wp_admin@eaquotes.net"))))

  (org2blog/wp-show-post-in-browser 'show)
  (org2blog/wp-track-posts (list ps/file-org2blog "Posts"))

  :config
  ;; (load-file "/Users/pablostafforini/.emacs.d/straight/repos/org2blog/org2blog.el")
  (defun ps/org2blog-move-tags-to-drawer ()
    "Convert org-mode tags to values of the property `POST_TAGS' in
an org drawer."
    (interactive)
    (while (re-search-forward "^\\* .*?:\\(.*\\):
")
    (let ((tags (string-join
                 (split-string
                  (substring-no-properties
                   (match-string 1))
                  ":")
                 ", ")))
      (org-set-property "POSG_TAGS" tags))))

  :general
  ("A-o" 'org2blog-user-interface))

(use-package org-journal
  :custom
  ;; (org-enable-org-journal-support t)
  (org-journal-dir ps/dir-journal)
  (org-journal-date-format "%Y-%m-%d")
  (org-journal-file-format "%Y.org")
  (org-journal-file-type 'yearly) ; set org-journal to use the same files for same year entries

  :config
  (defun ps/org-journal-goto-today ()
    "docstring"
    (interactive)
    (widen)
    (goto-char (point-min))
    (re-search-forward
     (concat "\* " (format-time-string org-journal-date-format) "\n"))
    (ps/org-narrow-to-entry-and-children)
    (goto-char (point-max)))

  :general
  ("A-j" 'org-journal-new-entry))

(use-package org-autosort
  :straight (org-autosort :type git :host github :repo "yantar92/org-autosort"))

(use-package ox-clip
  :demand t
  :custom
  ;; the default value works but will set the font to a particular
  ;; size, so when pasting to e.g. Google Docs it won't inherit the
  ;; font size of the rest of the document. this new value fixes this.
  (ox-clip-osx-cmd "hexdump -ve '1/1 \"%.2x\"' | xargs printf \"set the clipboard to {text:\\\" \\\", «class HTML»:«data HTML%s»}\" | osascript -")

  :general
  ((org-mode-map)
   "s-c" 'ox-clip-formatted-copy))

(use-package ox-pandoc)

(use-package ox-reveal
  ;; :defer 300
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package orgmdb
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after auth-source-pass
  :defer 60
  :straight (orgmdb
             :host github
             :repo "isamert/orgmdb.el")
  :config
  (setq orgmdb-omdb-apikey (auth-source-pass-get 'secret "auth-sources/omdb"))
  (defun ps/orgdmb-see-movie-in-imdb (&optional arg)
    "Visit the IMDb page of the movie at point.

With optional prefix argument, open with eww."
    (interactive "P")
    (let ((url (concat "https://www.imdb.com/title/" (org-entry-get nil "IMDB-ID"))))
      (if arg
          (eww url)
        (browse-url url))))
  (defhydra hydra-orgmdb
    (:exit t :idle 0.5)
    "orgmdb"
    ("f" (orgmdb-fill-movie-properties nil) "Fill properties")
    ("r" (orgmdb-fill-movie-properties 4) "Fill properties and replace title")
    ("w" (ps/orgdmb-see-movie-in-imdb 4) "See in IMDb (eww)")
    ("x" (ps/orgdmb-see-movie-in-imdb) "See in IMDb (external)"))
  :general
  (org-mode-map
   ;; "s-A-g" 'hydra-orgmdb/body
   ))

(use-package org-mime
  :general
  (org-mode-map
   "s-m" 'org-mime-htmlize
   "s-A-m" 'org-mime-org-subtree-htmlize))

(use-package elgantt
  :straight (elgantt
             :host github
             :repo "legalnonsense/elgantt")
  :defer 20
  :custom
  (elgantt-agenda-files (file-name-concat ps/dir-emacs "straight/repos/elgantt/test.org")))

(use-package org-pomodoro
  :defer 30
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length (- 30 org-pomodoro-length))
  (org-pomodoro-long-break-length org-pomodoro-short-break-length)
  (org-pomodoro-finished-sound "/System/Library/Sounds/Blow.aiff")
  (org-pomodoro-long-break-sound org-pomodoro-finished-sound)
  (org-pomodoro-short-break-sound org-pomodoro-finished-sound)

  :config
  (defun ps/org-pomodoro-notify (title message)
    "Send a persistent notification with TITLE and MESSAGE using
`alert'."
    (alert message :title title :category 'org-pomodoro))

  (advice-add 'org-pomodoro-notify :override #'ps/org-pomodoro-notify)

  :general
  ("H-I" 'org-pomodoro
   "A-H-i" 'org-pomodoro-extend-last-clock)
  (org-agenda-mode-map
   "I" 'org-pomodoro))

(use-package emacsql-sqlite3
  :if (version< emacs-version "29.0")
  :demand t)

(use-package emacsql-sqlite-builtin
  :if (not (version< emacs-version "29.0"))
  :demand t)

(use-package org-roam
  :after (:any emacsql-sqlite3 emacsql-sqlite-builtin)
  :demand t
  :init
  ;; `sqlite3' is deprecated, but I cannot get org-roam to work with
  ;; either `sqlite' or `sqlite-builtin'. So using this for the time
  ;; being, until I succeed in diagnosing the problem. See the
  ;; `org-roam-database-connector' docstring and the 'How to cache'
  ;; section of the manual: orgroam.com/manual.html#How-to-cache

  ;; [2023-01-24 Tue] I should migrate to `sqlite-builtin' when Emacs
  ;; 29 is released. See `org-roam-database-connector' docstring.
  (if (version< emacs-version "29.0")
      (setq org-roam-database-connector 'sqlite3)
    (setq org-roam-database-connector 'sqlite-builtin))

  (defun ps/org-roam-recent (days &optional limit)
    "Return list of files modified in the last DAYS. Optionally,
return such list if its length is less than LIMIT."
    (let* ((mins (* 60 24 days))
           (file-list (split-string
                       (shell-command-to-string
                        (format
                         "find %s -name '*.org'  -mmin -%s"
                         (directory-file-name org-roam-directory) mins)))))
      ;; Remove excluded files
      (setq file-list (cl-delete-if (lambda (k)
                                      (string-match-p org-roam-file-exclude-regexp k))
                                    file-list))
      (when (and limit
                 (< (length file-list) limit))
        file-list)))

  :custom
  (org-roam-directory ps/dir-org-roam)
  ;; (org-roam-complete-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))

  (org-roam-capture-templates
   `(("r" "bibliography reference" plain
      (file ,ps/file-orb-noter-template)
      :if-new
      (file ,ps/file-orb-capture-template)
      :unnarrowed t :immediate-finish t :jump-to-captured t)))

  ;; (org-roam-completion-everywhere t)

  :config
  ;; adapted from
  ;; github.com/org-roam/org-roam/wiki/User-contributed-Tricks#showing-node-hierarchy
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0)
         (concat
          (propertize (org-roam-node-file-title node) 'face 'org-level-1)
          " > "))
       ;; This is a hacky propertization because it doesn't color the
       ;; intermediate headings differently, but doing that slowed
       ;; down the function too much.
       (when (> level 1)
         (concat
          (propertize (string-join (org-roam-node-olp node) " > ") 'face 'org-level-2)
          " > "))
       (propertize (org-roam-node-title node) 'face 'org-level-3))))

  (setq org-roam-node-display-template
        (concat "${hierarchy:160} "
                (propertize "${tags:20}" 'face 'org-tag)))


  (defvar ps/org-roam-excluded-dirs nil)
  (when (equal (system-name) ps/computer-hostname-pablo)
    (dolist (dir (list ps/dir-anki
                       ps/dir-inactive
                       ps/dir-bibliographic-notes ; excluded since discoverable via `org-cite-insert'
                       ps/dir-archive))
      (push (file-relative-name dir ps/dir-org-roam) ps/org-roam-excluded-dirs)))

  (defvar ps/org-roam-excluded-files nil)
  (when (equal (system-name) ps/computer-hostname-pablo)
    (dolist (file '("orb-noter-template.org"
                    "calendar.org"
                    "notatu-dignum.org"
                    "quotes-old.org"
                    ".org2blog.org"
                    "wiki-notes.org" ; removing temporarily
                    "feeds.org"
                    "conflicted-copy"))
      (push file ps/org-roam-excluded-files)))

  ;; exclude selected headings based on parent files or directories
  (setq org-roam-file-exclude-regexp
        (regexp-opt (delete-dups
                     (append
                      ps/org-roam-excluded-dirs
                      ps/org-roam-excluded-files))))

  ;; exclude selected headings based on other criteria
  (setq org-roam-db-node-include-function
        (lambda ()
          (if (or
               ;; exclude based on tags
               (member "noid" (org-get-tags))
               (member "ARCHIVE" (org-get-tags))
               ;; exclude based on heading names
               (member (org-get-heading) '("Local variables"
                                           "COMMENT Local variables"
                                           "TODO Local variables"
                                           "Evaluation"
                                           "History"
                                           "Further reading"
                                           "External links"
                                           "Related entries"
                                           "Archive :ARCHIVE:"))
               ;; exclude buffers when in list of special dirs and org
               ;; heading at point is of level higher than 1 (i.e.
               ;; don't create unnecessary IDs for article
               ;; subsections)
               (and
                ;; dir condition
                (member
                 (file-name-directory (buffer-file-name))
                 (mapcar #'file-name-as-directory
                         ;; List of special dirs
                         (list
                          ps/dir-journal)))
                ;; heading condition
                (> (org-current-level) 1))
               )
              nil
            t)))

  (defun ps/org-roam-db-query (sql &rest args)
    "Run SQL query on Org-roam database with ARGS.
        SQL can be either the emacsql vector representation, or a string."
    (sleep-for 0 1)
    (apply #'emacsql (org-roam-db) sql args))

  (advice-add 'org-roam-db-query :override #'ps/org-roam-db-query)
  (advice-add 'org-roam-node-find :before #'widen)
  (advice-add 'org-roam-node-find :after #'ps/org-narrow-to-entry-and-children)

  ;; org-roam.discourse.group/t/org-roam-v2-org-id-id-link-resolution-problem/1491/7
  ;; github.com/org-roam/org-roam/issues/1702#issuecomment-888998330
  (defun ps/org-roam-update-id-locations ()
    "Update org id locations indexed by org roam."
    (interactive)
    (org-id-update-id-locations
     (directory-files-recursively org-roam-directory ".org$\\|.org.gpg$")))

  (defun ps/org-roam-remove-file-level-properties ()
    "Remove `ROAM_REFS' and `ID' properties from file-level drawer."
    (when (string= "r" (plist-get org-capture-plist :key))
      (goto-char (point-min))
      (unless (org-get-heading)
        ;; Take action with file-level properties only.
        (org-delete-property "ID")
        (org-delete-property "ROAM_REFS")
        (ps/org-jump-to-first-heading)
        (org-id-get-create))))

  (defun ps/org-roam-new-note (note-type)
    "Create a new `org-roam' note."
    (interactive
     (list
      (completing-read
       "Select note type: "
       '("generic" "person"))))
    (let ((tags)
          (directory))
      (cond ((string= note-type "generic")
             (setq tags "note")
             (setq directory ps/dir-notes))
            ((string= note-type "person")
             (setq tags "person")
             (setq directory ps/dir-people)))
      (let* ((name (read-from-minibuffer "Entry name: "))
             (slug (org-hugo-slug name))
             (filename (concat slug ".org")))
        (when (file-exists-p filename)
          (user-error (format "File `%s' already exists." filename)))
        (find-file (file-name-concat directory filename))
        (insert "#+title: " name "\n\n")
        (org-insert-heading)
        (insert name)
        (org-set-tags tags)
        (org-id-get-create)
        (ps/org-narrow-to-entry-and-children)
        (goto-char (point-max)))))

  (add-to-list 'completion-at-point-functions #'org-roam-complete-link-at-point)
  ;; (add-to-list 'completion-at-point-functions #'org-roam-complete-everywhere)

  ;; include transcluded links in `org-roam' backlinks
  (delete '(keyword "transclude") org-roam-db-extra-links-exclude-keys)

  (org-roam-db-autosync-mode -1)

  :hook
  (org-roam-capture-new-node-hook . orb--insert-captured-ref-h)
  (org-roam-capture-new-node-hook . org-roam-capture--insert-captured-ref-h)
  (org-capture-prepare-finalize-hook . ps/org-roam-remove-file-level-properties)

  :general
  ("A-H-n" 'ps/org-roam-new-note
   "H-j" 'org-roam-node-find
   "H-s-i" 'org-roam-node-insert)
  (org-mode-map
   "s-r" 'org-roam-buffer-toggle))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save nil)
  (org-roam-ui-open-on-start nil))

(use-package org-roam-timestamps
  :disabled
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after org-roam
  :demand t
  :custom
  (org-roam-timestamps-remember-timestamps nil)
  (org-roam-timestamps-timestamp-parent-file t)

  :config
  (org-roam-timestamps-mode))

(use-package org-transclusion
  :after org
  :defer 5

  :config
  (dolist (element '(headline drawer property-drawer))
    (push element org-transclusion-exclude-elements))

  (face-spec-set 'org-transclusion-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec)
  (face-spec-set 'org-transclusion-source-fringe
                 '((((background light))
                    :foreground "black")
                   (t
                    :foreground "white"))
                 'face-override-spec)
  :general
  ("s-l" 'org-transclusion-add-all))

(use-package vulpea
  :if (equal (system-name) ps/computer-hostname-pablo)
  :commands (vulpea-buffer-p vulpea-agenda-files-update vulpea-buffer-prop-get-list vulpea-project-update-tag)
  :config
  ;; adapted from
  (defvar ps/vulpea-excluded-directories nil
    "Directories containing files to exclude from list of projects")

  (defvar ps/vulpea-excluded-files nil
    "files to exclude from list of projects")

  (defun ps/vulpea-project-p ()
    "Return non-nil if current buffer has a TODO, a schedule or a deadline.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (when (and (eq major-mode 'org-mode)

               ;; exclude dirs
               (not (member (file-name-directory (buffer-file-name))
                            ps/vulpea-excluded-directories))

               ;; exclude files
               (not (member (buffer-file-name) ps/vulpea-excluded-files)))

      (org-element-map
          (org-element-parse-buffer 'headline)
          'headline
        (lambda (headline)
          (or
           (eq (org-element-property :todo-type headline)
               'todo)
           (org-element-property :scheduled headline)
           (org-element-property :deadline headline)))
        nil
        'first-match)))

  (defun ps/vulpea-anniversary-p ()
    "Return non-nil if current buffer has an anniversary."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (search-forward "%%(org-anniversary" nil t))))

  (defun ps/vulpea-priority-p ()
    "Return non-nil if current buffer has a heading with a priority.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (when (eq major-mode 'org-mode)
      (org-element-map
          (org-element-parse-buffer 'headline)
          'headline
        (lambda (headline)
          (org-element-property :priority headline))
        nil
        'first-match)))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (or (ps/vulpea-project-p) (ps/vulpea-anniversary-p))
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files
          (seq-difference
           (delete-dups (append
                         (org-agenda-files)
                         (vulpea-project-files)
                         ;; include files modified in past three days,
                         ;; provided number of such files less than 1000
                         (ps/org-roam-recent 1 1000)))
           ps/org-agenda-files-excluded)))

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  :hook
  (find-file-hook . vulpea-project-update-tag)
  (before-save-hook . vulpea-project-update-tag))

(use-package org-noter
  :custom
  (org-noter-notes-search-path `(,ps/dir-bibliographic-notes))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame nil)
  ;; (org-noter-property-doc-file "file") ;; change to this once everything is working
  (org-noter-separate-notes-from-heading t)
  :general
  (pdf-annot-minor-mode-map
   "s-s" 'org-noter-create-skeleton))

(defvar ps/library-genesis
  '("Library Genesis"
    "https://libgen.lc/index.php?req="
    "&phrase=1&view=simple&column=def&sort=extension&sortmode=DESC"))

(defvar ps/amazon
  '("Amazon"
    "https://smile.amazon.com/s?k="
    "&i=stripbooks"))

(defvar ps/worldcat
  '("Worldcat"
    "https://www.worldcat.org/search?q="
    "&itemType=book&limit=50&offset=1"))

(defvar ps/internet-archive
  '("Internet Archive"
    "https://archive.org/search.php?query="
    ""))

(defvar ps/university-of-toronto
  '("University of Toronto"
    "https://librarysearch.library.utoronto.ca/discovery/search?query=any,contains,"
    "&tab=Everything&search_scope=UTL_AND_CI&vid=01UTORONTO_INST:UTORONTO&offset=0"))

(defvar ps/hathitrust
  '("HathiTrust"
    "https://babel.hathitrust.org/cgi/ls?q1="
    "&field1=ocr;a=srchls;lmt=ft;sz=100"))

(defvar ps/connected-papers
  '("Connected Papers"
    "https://www.connectedpapers.com/search?q="
    ""))

(defvar ps/google-scholar
  '("Google Scholar"
    "https://scholar.google.com/scholar?q="
    ""))

(defvar ps/wikipedia
  '("Google Scholar"
    "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&profile=default&search="
    "&fulltext=Search"))

(defvar ps/goodreads
  '("Goodreads"
    "https://www.goodreads.com/search?q="
    ""))

(defvar ps/ebib-search-book
  '(ps/ebib-search-goodreads
    ps/ebib-search-hathitrust
    ps/ebib-search-internet-archive
    ps/ebib-search-university-of-toronto
    ps/ebib-search-library-genesis
    ps/ebib-search-amazon))

(defvar ps/ebib-download-book
  '(ps/ebib-search-hathitrust
    ps/ebib-search-internet-archive
    ps/ebib-search-university-of-toronto
    ps/ebib-search-library-genesis))

(defvar ps/ebib-search-article
  '(ps/ebib-search-connected-papers
    ps/ebib-search-google-scholar))

(use-feature oc
  :demand t
  :custom
  (org-cite-global-bibliography `(,ps/file-bibliography-new
                                  ,ps/file-bibliography-old))
  (org-cite-csl-styles-dir ps/dir-csl-styles)
  (org-cite-csl-locales-dir ps/dir-csl-locales)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar) ; `org-open-at-point` integration
  (org-cite-activate-processor 'citar) ;
  (org-cite-export-processors
   '(
     (md csl "effective-altruism-wiki-markdown.csl" "effective-altruism-wiki-markdown.csl")      ; Footnote reliant
     (latex csl "effective-altruism-wiki-markdown.csl" "effective-altruism-wiki-markdown.csl")   ; For philosophy
     (odt csl "effective-altruism-wiki-markdown.csl" "effective-altruism-wiki-markdown.csl")     ; Footnote reliant
     (docx csl "effective-altruism-wiki-markdown.csl" "effective-altruism-wiki-markdown.csl")    ; Footnote reliant
     (t csl "american-medical-association-brackets.csl")       ; Fallback
     ))

  :config
  ;; Comment out `org-cite--allowed-p' condition to allow invocation
  ;; in any mode. Even if inserting a citation is not allowed, one may
  ;; want to invoke the command to trigger contextual actions via
  ;; `embark'.
  (defun ps/org-cite-insert (arg)
    "Insert a citation at point.
Insertion is done according to the processor set in `org-cite-insert-processor'.
ARG is the prefix argument received when calling interactively the function."
    (interactive "P")
    (unless org-cite-insert-processor
      (user-error "No processor set to insert citations"))
    (org-cite-try-load-processor org-cite-insert-processor)
    (let ((name org-cite-insert-processor))
      (cond
       ((not (org-cite-get-processor name))
        (user-error "Unknown processor %S" name))
       ((not (org-cite-processor-has-capability-p name 'insert))
        (user-error "Processor %S cannot insert citations" name))
       (t
        (let ((context (org-element-context))
              (insert (org-cite-processor-insert (org-cite-get-processor name))))
          (cond
           ((memq (org-element-type context) '(citation citation-reference))
            (funcall insert context arg))
           (t
            (org-cite--allowed-p context)
            (funcall insert nil arg))))))))

  (advice-add 'org-cite-insert :override #'ps/org-cite-insert)

  :general
  ("H-/" 'org-cite-insert))

(use-feature oc-basic
  :after oc
  :demand t)

(use-feature oc-bibtex
  :after oc
  :demand t)

(use-feature oc-biblatex
  :after oc
  :demand t)

(use-feature oc-csl
  :after oc
  :demand t)

(use-package citeproc
  :demand t)

(use-feature bibtex
  :demand t
  :custom
  ;; This corresponds (roughly?) to `auth+year+shorttitle(3,3)' on Better BibTeX
  ;; retorque.re/zotero-better-bibtex/citing/
  (bibtex-autokey-names 1)
  (bibtex-autokey-name-case-convert 'capitalize)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-titlewords 3)
  (bibtex-autokey-titlewords-stretch 0)
  (bibtex-autokey-titleword-ignore '("A" "a" "An" "an" "On" "on" "The" "the" "Eine?" "Der" "Die" "Das" "El" "La" "Lo" "Los" "Las" "Un" "Una" "Unos" "Unas" "el" "la" "lo" "los" "las" "un" "una" "unos" "unas" "y" "o" "Le" "La" "L'" "Les" "Un" "Une" "Des" "Du" "De la" "De l'" "Des" "le" "la" "l'" "les" "un" "une" "des" "du" "de la" "de l'" "des" "Lo" "Il" "La" "L'" "Gli" "I" "Le" "Uno" "lo" "il" "la" "l'" "gli" "i" "le" "uno"))
  (bibtex-autokey-title-terminators "[.!?;]\\|--")
  (bibtex-autokey-titleword-case-convert 'capitalize)
  (bibtex-autokey-titleword-length nil)
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-preserve-case t)
  ;; Remove accents
  (bibtex-autokey-before-presentation-function 'ps/bibtex-asciify-string)
  (bibtex-entry-format '(opts-or-alts numerical-fields realign))

  :config
  ;; Copied from xahlee.info/emacs/emacs/emacs_zap_gremlins.html
  (defun ps/bibtex-asciify-text (&optional Begin End)
    "Remove accents in some letters. e.g. café → cafe.
Change European language characters into equivalent ASCII ones.
When called interactively, work on current line or text selection.

URL `http://xahlee.info/emacs/emacs/emacs_zap_gremlins.html'
Version 2018-11-12 2021-09-17"
    (interactive)
    (let (($charMap
           [
            ["ß" "ss"]
            ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
            ["æ" "ae"]
            ["ç\\|č\\|ć" "c"]
            ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
            ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
            ["ñ\\|ň\\|ń" "n"]
            ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
            ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
            ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
            ["þ" "th"]
            ["ď\\|ð\\|đ" "d"]
            ["ĩ" "i"]
            ["ľ\\|ĺ\\|ł" "l"]
            ["ř\\|ŕ" "r"]
            ["š\\|ś" "s"]
            ["ť" "t"]
            ["ž\\|ź\\|ż" "z"]
            [" " " "]       ; thin space etc
            ["–" "-"]       ; dash
            ["—\\|一" "--"] ; em dash etc
            ])
          ($p1 (if Begin Begin
                 (if (region-active-p)
                     (region-beginning)
                   (line-beginning-position))))
          ($p2 (if End End
                 (if (region-active-p)
                     (region-end)
                   (line-end-position)))))
      (let ((case-fold-search t))
        (save-restriction
          (narrow-to-region $p1 $p2)
          (mapc
           (lambda ($pair)
             (goto-char (point-min))
             (while (re-search-forward (elt $pair 0) (point-max) t)
               (replace-match (elt $pair 1))))
           $charMap)))))

  (defun ps/bibtex-asciify-string (String)
    "Returns a new string. e.g. café → cafe.
See `xah-asciify-text'
Version 2015-06-08"
    (with-temp-buffer
      (insert String)
      (ps/bibtex-asciify-text (point-min) (point-max))
      (buffer-string)))

  ;; tweak function so that `bibtex-autokey-get-field' looks up `urldate' field
  (defun ps/bibtex-autokey-get-year ()
    "Return year field contents as a string obeying `bibtex-autokey-year-length'."
    (let* ((str (bibtex-autokey-get-field '("date" "year" "urldate"))) ; possibly ""
           (year (or (and (iso8601-valid-p str)
                          (let ((year (decoded-time-year (iso8601-parse str))))
                            (and year (number-to-string year))))
                     ;; BibTeX permits a year field "(about 1984)", where only
                     ;; the last four nonpunctuation characters must be numerals.
                     (and (string-match "\\([0-9][0-9][0-9][0-9]\\)[^[:alnum:]]*\\'" str)
                          (match-string 1 str))
                     (user-error "Year or date field `%s' invalid" str))))
      (substring year (max 0 (- (length year) bibtex-autokey-year-length)))))

  (defun ps/bibtex-get-key ()
    "Return the key of the current BibTeX entry."
    (save-excursion
      (save-restriction
        (bibtex-narrow-to-entry)
        (goto-char (point-min))
        (if (re-search-forward "@\\w+{\\([^,]+\\),")
            (match-string-no-properties 1)
          (user-error "Not on a BibTeX entry")))))

  (advice-add 'bibtex-autokey-get-year :override #'ps/bibtex-autokey-get-year)

  ;; Add custom 'video' field
  (push '("Video" "Video file"
          (("author" nil nil 0)
           ("title")
           ("date" nil nil 1)
           ("year" nil nil -1)
           ("url" nil nil 2))
          nil
          (("subtitle")
           ("titleaddon")
           ("language")
           ("version")
           ("note")
           ("organization")
           ("month")
           ("addendum")
           ("pubstate")
           ("eprintclass" nil nil 4)
           ("primaryclass" nil nil -4)
           ("eprinttype" nil nil 5)
           ("archiveprefix" nil nil -5)
           ("urldate"))) bibtex-biblatex-entry-alist)

  (push '("Movie" "Film"
          (("author" nil nil 0)
           ("title")
           ("date" nil nil 1)
           ("year" nil nil -1)
           ("url" nil nil 2))
          nil
          (("abstract")
          ("keywords")
           ("titleaddon")
           ("language")
           ("version")
           ("note")
           ("organization")
           ("month")
           ("addendum")
           ("pubstate")
           ("eprintclass" nil nil 4)
           ("primaryclass" nil nil -4)
           ("eprinttype" nil nil 5)
           ("archiveprefix" nil nil -5)
           ("urldate"))) bibtex-biblatex-entry-alist)

  :general
  (bibtex-mode-map
   "A-C-H-x" 'bibtex-copy-entry-as-kill
   "A-C-H-c" 'bibtex-kill-entry
   "A-C-H-a" 'bibtex-copy-field-as-kill
   "A-C-H-f" 'bibtex-kill-field
   "A-C-s-r" 'bibtex-previous-entry
   "A-C-s-f" 'bibtex-next-entry))

(use-package bibtex-completion
  ;; :after citar
  :demand t
  :custom
  (bibtex-completion-pdf-open-function 'find-file)
  (bibtex-completion-bibliography org-cite-global-bibliography)
  (bibtex-completion-notes-path ps/dir-bibliographic-notes)
  (bibtex-completion-pdf-field "file")
  (bibtex-dialect 'biblatex))

(use-package org-roam-bibtex
  :demand t
  ;; :after bibtex-completion
  :custom
  (orb-roam-ref-format 'org-cite)
  (orb-insert-interface 'citar-open-notes)
  (orb-note-actions-interface 'default)
  (orb-preformat-keywords
   '("citekey" "title" "url" "author" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf"))

  :config
  (add-to-list 'orb-preformat-keywords "year")
  (org-roam-bibtex-mode))

(use-package citar-org-roam
  :straight (citar-org-roam
             :host github
                   :repo "emacs-citar/citar-org-roam")
  :demand t
  :no-require t
  :after  org-roam)

(use-package citar
  :straight (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
  ;; :defer 10
  ;; :after oc vertico embark marginalia
  :demand t
  :custom
  (citar-open-note-functions '(orb-citar-edit-note))
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths `(,ps/dir-bibliographic-notes))
  (citar-at-point-function 'embark-act)
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  (citar-format-reference-function 'citar-citeproc-format-reference)

  :config
  (require 'citar-org-roam)
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))

  (setq citar-notes-source 'orb-citar-source)

  (defun ps/citar--get-title (entry)
    "Return title of ENTRY."
    (let* ((field (citar--field-with-value '(title) entry)))
      (when field
        (citar--get-value field entry))))

  (defun ps/citar-search-library-genesis (key-entry)
    "Search title of KEY-ENTRY on Library Genesis.

With prefix, rebuild the cache before offering candidates."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg)))
    (let ((title (ps/citar--get-title (cdr key-entry))))
      (if title
          (browse-url (concat
                       ps/library-genesis-prefix
                       title
                       ps/library-genesis-suffix))
        (message "No link found for %s" (car key-entry)))))

  (defun ps/citar-search-worldcat (key-entry)
    "Search title of KEY-ENTRY on Worldcat.

With prefix, rebuild the cache before offering candidates."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg)))
    (let ((title (ps/citar--get-title (cdr key-entry))))
      (if title
          (browse-url (concat
                       ps/worldcat-prefix
                       title
                       ps/worldcat-suffix))
        (message "No link found for %s" (car key-entry)))))

  (defun ps/citar-search-internet-archive (key-entry)
    "Search title of KEY-ENTRY on the Internet Archive.

With prefix, rebuild the cache before offering candidates."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg)))
    (let ((title (ps/citar--get-title (cdr key-entry))))
      (if title
          (browse-url (concat
                       ps/internet-archive-prefix
                       title
                       ps/internet-archive-suffix))
        (message "No link found for %s" (car key-entry)))))

  (defun ps/citar-search-amazon (key-entry)
    "Search title of KEY-ENTRY on Amazon.

With prefix, rebuild the cache before offering candidates."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg)))
    (let ((title (ps/citar--get-title (cdr key-entry))))
      (if title
          (browse-url (concat
                       ps/amazon-prefix
                       title
                       ps/amazon-suffix))
        (message "No link found for %s" (car key-entry)))))

  (defun ps/citar-open-in-ebib (citekey)
    "Open bibliographic entry associated with the CITEKEY in Ebib."
    (interactive (list (citar-select-ref)))
    (unless (get-buffer "*ebib*")
      (ebib))
    (ps/ebib-open-key citekey))

  (defun ps/citar-open-file-at-point ()
    "Launch citar with citekey associated with file at point."
    (interactive)
      (citar-open `(,(ps/get-stem-of-current-buffer))))

  :general
  ("A-H-M-s-?" 'ps/citar-open-file-at-point)
  ((citar-map citar-citation-map)
   "c" 'embark-copy-as-kill
   "i" 'ps/citar-open-in-ebib
   "u" 'citar-open-links))

(use-feature citar-citeproc
  :after citar
  :demand t
  :custom
  (citar-citeproc-csl-styles-dir org-cite-csl-styles-dir)
  (citar-citeproc-csl-locales-dir org-cite-csl-locales-dir))

(use-package citar-embark
  :after citar embark
  :demand t
  :config
  (citar-embark-mode))

(use-package org-ref
  :defer 20
  :custom
  (org-ref-insert-cite-function 'org-cite-insert)
  (org-ref-bibtex-pdf-download-dir ps/dir-downloads)
  (bibtex-completion-library-path ps/dir-library-pdf)

  :config
  ;; Tweak function to remove redundant and conflicting `Year' field
  (defun isbn-to-bibtex (isbn bibfile)
    "Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from www.ebook.de."
    (interactive
     (list
      (read-string
       "ISBN: "
       ;; now set initial input
       (cond
        ;; If region is active and it starts with a number, we use it
        ((and  (region-active-p)
               (s-match "^[0-9]" (buffer-substring (region-beginning) (region-end))))
         (buffer-substring (region-beginning) (region-end)))
        ;; if first entry in kill ring starts with a number assume it is an isbn
        ;; and use it as the guess
        ((stringp (car kill-ring))
         (when (s-match "^[0-9]" (car kill-ring))
           (car kill-ring)))
        ;; type or paste it in
        (t
         nil)))
      (completing-read "Bibfile: " (org-ref-possible-bibfiles))))

    (let* ((url (format "https://www.ebook.de/de/tools/isbn2bibtex?isbn=%s" isbn))
           (entry))
      (with-current-buffer (url-retrieve-synchronously url t t)
        (goto-char (point-min))
        (when (re-search-forward "@[a-zA-Z]+{.+\\(\n\s+[^\n]+\\)+}$" nil t)
          (setq entry (match-string 0))
          ;; FIXME: This isn't working
          (s-replace-regexp "^  Year =.*" "" entry)))

      (if (not entry)
          (message "Nothing found.")
        (find-file bibfile)
        (goto-char (point-max))
        (insert (with-temp-buffer
                  (insert (concat entry "\n}"))
                  (goto-char (point-min))
                  ;; [2020-06-06 Sat] I got a report that ottobib returns entries
                  ;; with ,, in the first line. here if we find one, I eliminate
                  ;; one of them.
                  (when (re-search-forward ",," nil t)
                    (delete-char -1))
                  (org-ref-isbn-clean-bibtex-entry)
                  ;; TODO: Uncomment the line below when you fix the FIXME issue above
                  ;; (org-ref-clean-bibtex-entry)
                  (bibtex-fill-entry)
                  (s-trim (buffer-string))))
        (save-buffer))))

  (delete 'orcb-check-journal org-ref-clean-bibtex-entry-hook)
  (delete 'orcb-download-pdf org-ref-clean-bibtex-entry-hook))

(use-feature doi-utils
  :after org-ref
  :config
  (defun ps/doi-utils-doi-p (string)
    "Return `t' if string is a valid DOI."
    (string-match "^10.[[:digit:]]\\{4,9\\}/[().-;A-Z_-]+$" string)))

(use-package ebib
  :defer 5
  :init

  :custom
  (ebib-filename-separator ";")
  (ebib-file-associations nil "do not open any file types externally")
  (ebib-layout 'index-only)
  (ebib-bibtex-dialect 'biblatex)
  (ebib-use-timestamp t)
  (ebib-preload-bib-files org-cite-global-bibliography)
  (ebib-index-columns '(("Entry Key" 30 t)
                        ("Author/Editor" 25 t)
                        ("Year" 4 t)
                        ("Title" 50 t)))
  (ebib-timestamp-format "%Y-%m-%d %T (%Z)")
  (ebib-save-xrefs-first nil)
  (ebib-default-entry-type "online")
  (ebib-hidden-fields '("addendum" "afterword" "annotator" "archiveprefix" "bookauthor" "booksubtitle" "booktitleaddon" "chapter" "commentator" "edition" "editora" "editorb" "editorc" "eid" "eprint" "eprintclass" "eprinttype" "eventdate" "eventtitle" "foreword" "holder" "howpublished" "introduction" "isrn" "issn" "issue" "issuesubtitle" "issuetitle" "issuetitleaddon" "journaltitleadddon" "journalsubtitle" "language" "location" "mainsubtitle" "maintitle" "maintitleaddon" "month" "origlanguage" "pagetotal" "part" "primaryclass" "remark" "subtitle" "urldate" "venue" "version" "volumes" "year"))

  :config
  (defun ps/ebib-open-or-switch ()
    "Open ebib in the right window or switch to it if already open."
    (interactive)
    (ps/window-split-if-unsplit)
    (if (> (frame-width) ps/frame-width-threshold)
        (winum-select-window-3)
      (winum-select-window-2))
    (ebib))

  (defun ps/ebib-reload-current-database-no-confirm ()
    "Reload the current database from disk, without asking for
confirmation."
    (interactive)
    (ebib--execute-when
      (entries
       (let ((buf (current-buffer)))
         (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
         (ebib--reload-database ebib--cur-db)
         (ebib--set-modified nil ebib--cur-db)
         ;; This is the line that causes point to disappear, but
         ;; disabling it seems harmless.
         ;; (ebib--update-buffers)
         (message "Database reloaded")))
      (default
       (beep))))

  ;; these two functions, together with the associated timers, detect
  ;; when a database has changes and save its contents to the corresponding bib
  ;; file
  (defun ps/ebib-auto-save-new-db ()
    "docstring"
    (when (ebib-db-modified-p (car ebib--databases))
      (ebib-save-current-database '(16)))
    (run-with-timer 10 nil #'ps/ebib-auto-save-new-db))

  (run-with-timer 10 nil #'ps/ebib-auto-save-new-db)

  (defun ps/ebib-auto-save-old-db ()
    "docstring"
    (when (ebib-db-modified-p (cadr ebib--databases))
      (ebib-save-current-database '(16))
      (cancel-function-timers #'ps/ebib-auto-save-old-db)
      (run-with-idle-timer 60 nil #'ps/ebib-auto-save-old-db)))

  (run-with-idle-timer 60 nil #'ps/ebib-auto-save-old-db)

  ;; these two functions, in turn, detect when a bib file has changed
  ;; and reload the corresponding database
  (file-notify-add-watch
   ps/file-bibliography-new
   '(change attribute-change)
   (lambda (event)
     (ps/ebib-reload-current-database-no-confirm)))

  (file-notify-add-watch
   ps/file-bibliography-old
   '(change attribute-change)
   (lambda (event)
     (ps/ebib-reload-current-database-no-confirm)))

  ;; Tweak original function to prevent unnecessary vertical window splits
  (defun ps/ebib--setup-windows ()
    "Create Ebib's window configuration.
If the index buffer is already visible in some frame, select its
window and make the frame active,"
    (let ((index-window (get-buffer-window (ebib--buffer 'index) t))
          (old-frame (selected-frame)))
      (if index-window
          (progn (select-window index-window t)
                 (unless (eq (window-frame) old-frame)
                   (select-frame-set-input-focus (window-frame))
                   (setq ebib--frame-before old-frame)))
        (setq ebib--saved-window-config (current-window-configuration))
        (setq ebib--frame-before nil)
        (cond
         ((eq ebib-layout 'full)
          (delete-other-windows))
         ((eq ebib-layout 'custom)
          (setq ebib--window-before (selected-window))
          (delete-other-windows)
          (let ((width (cond
                        ((integerp ebib-width)
                         (- (window-total-width) ebib-width))
                        ((floatp ebib-width)
                         (- (window-total-width) (truncate (* (window-total-width) ebib-width)))))))
            (select-window (split-window (selected-window) width t)))))
        (let* ((index-window (selected-window))
               (entry-window (selected-window)))
          (switch-to-buffer (ebib--buffer 'index))
          (unless (eq ebib-layout 'index-only)
            (set-window-buffer entry-window (ebib--buffer 'entry)))
          ;; (set-window-dedicated-p index-window t)
          (if (eq ebib-layout 'custom)
              (set-window-dedicated-p entry-window t)))))
    (if (buffer-local-value 'ebib--dirty-index-buffer (ebib--buffer 'index))
        (setq ebib--needs-update t)))

  (advice-add 'ebib--setup-windows :override #'ps/ebib--setup-windows)

  ;; tweak original function to pass custom arguments to `format-time-string'
  (defun ps/ebib--store-entry (entry-key fields db &optional timestamp if-exists)
    "Store the entry defined by ENTRY-KEY and FIELDS into DB.
Optional argument TIMESTAMP indicates whether a timestamp is to
be added to the entry.  Note that for a timestamp to be added,
`ebib-use-timestamp' must also be set to T. IF-EXISTS is as for
`ebib-db-set-entry'.

If storing the entry was successful, return the key under which
the entry is actually stored (which, if IF-EXISTS is `uniquify',
may differ from ENTRY-KEY); otherwise return nil.  Depending on
the value of IF-EXISTS, storing an entry may also result in an
error."
    (let ((actual-key (ebib-db-set-entry entry-key fields db if-exists)))
      (when (and actual-key timestamp ebib-use-timestamp)
        (ebib-set-field-value "timestamp" (format-time-string ebib-timestamp-format nil "GMT") actual-key db 'overwrite))
      actual-key))

  (advice-add 'ebib--store-entry :override #'ps/ebib--store-entry)

  (defvar ps/ebib-isbn-p
    "\\(ISBN-*\\(1[03]\\)* *\\(: \\)?\\)*\\(\\([0-9Xx][ -]*\\)\\{13\\}\\|\\([0-9Xx][ -]*\\)\\{10\\}\\)")

  (defun ps/ebib-isbn-p (string)
    "Return `t' if string looks like an ISBN."
    (string-match ps/ebib-isbn-p string))

  (defun ps/ebib-get-isbn ()
    "Return ISBN for the current entry, if it exists."
    (when-let ((isbn
                (ebib-get-field-value
                 "isbn"
                 (ebib--get-key-at-point)
                 ebib--cur-db
                 'noerror
                 'unbraced
                 'xref)))
      (car (split-string
            (s-replace "-"
                       ""
                       (substring-no-properties
                        isbn))
            " "))))

  (defun ps/ebib-video-p (string)
    "Return `t' if string looks like a video URL."
    ;; TODO: Add more video sites
    (string-match
     "https?://\\(www\\.\\)?\\(youtube\\.com/watch\\?v=\\|youtu.be/\\)\\([a-zA-Z0-9_-]+\\)"
     string))

  (defun ps/ebib--update-file-field-contents (key file-name)
    "docstring"
    (let* ((field "file")
           (file-field-contents (ebib-get-field-value field key ebib--cur-db t t)))
      (unless (and
               file-field-contents
               (catch 'file-exists
                 (dolist (file (ebib--split-files file-field-contents))
                   (when (string= file file-name)
                     (throw 'file-exists file)))))
        (ebib-set-field-value field file-name key ebib--cur-db ";")
        (ebib--store-multiline-text (current-buffer))
        (ebib--redisplay-field field)
        (ebib--redisplay-index-item field)
        (ebib-save-current-database t))))

  (defun ps/ebib-download-by-identifier (&optional id)
    "docstring"
    (interactive)
    (let ((id (or id
                  (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
                  (ps/ebib-get-isbn)
                  (ebib-get-field-value "url" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
                  (read-string "Enter ISBN or DOI: "))))
      ;; TODO: Add support for arXiv
      (cond ((ps/doi-utils-doi-p id)
             (scihub id))
            ((ps/ebib-isbn-p id)
             (ps/ebib-download-book nil id))
            ((ps/ebib-video-p id)
             (ps/ebib-download-video id))
            (t
             (user-error "Identifier does not appear to be an ISBN or DOI.")))))


  (defun ps/ebib-search-by-identifier (&optional id)
    "docstring"
    (interactive)
    (let ((id (or id
                  (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
                  (ps/ebib-get-isbn)
                  (read-string "Enter ISBN or DOI: "))))
      ;; TODO: Add support for arXiv
      (cond ((ps/doi-utils-doi-p id)
             (scihub id))
            ((ps/ebib-isbn-p id)
             (ps/ebib-search-book nil id))
            (t
             (user-error "Identifier does not appear to be an ISBN or DOI.")))))

  (defun ps/ebib--search-or-download-dwim (action)
    "docstring"
    (if (string= (ebib--current-field) "title")
        (when-let ((type (ebib-get-field-value "=type=" (ebib--get-key-at-point)
                                               ebib--cur-db)))
          (cond
           ((member type
                    '("book" "collection" "mvbook" "inbook" "incollection" "bookinbook" "suppbook" "Book" "Collection" "MVBook" "Inbook" "Incollection" "Bookinbook" "Suppbook"))
            (cond ((eq action 'search)
                   (ps/ebib-search-book-by-title))
                  ((eq action 'download)
                   (ps/ebib-download-book-by-title))))
           ((member type '("article" "Article"))
            (cond ((eq action 'search)
                   (ps/ebib-search-article-by-title))
                  ((eq action 'download)
                   (ps/ebib-download-article-by-title))))
           (t
            (user-error (format "No action defined for entries of type `%s'" type)))))
      (cond ((eq action 'search)
             (ps/ebib-search-by-identifier))
            ((eq action 'download)
             (ps/ebib-download-by-identifier)))))

  (defun ps/ebib-search-dwim ()
    "If field at point is 'title', run a search with its value; otherwise use identifier.

  The list of websites for the search query is defined by the variable `ps/ebib-search-book'"
    (interactive)
    (ps/ebib--search-or-download-dwim 'search))

  (defun ps/ebib-download-dwim ()
    "If field at point is 'title', run a search with its value;
otherwise use identifier.

The list of websites for the search query is defined by the
  variable `ps/ebib-download-book'"
    (interactive)
    (ps/ebib--search-or-download-dwim 'download))

  (defun ps/ebib-get-file (extension)
    "docstring"
    (when-let ((files (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db t t)))
      (catch 'tag
        (mapc
         (lambda (file)
           (when (equal (file-name-extension file) extension)
             (throw 'tag file)))
         (ebib--split-files files))
        nil)))

  (defun ps/ebib-open-file (extension)
    "Open file with EXTENSION in entry at point, if it (uniquely)
exists."
    (interactive)
    (if-let ((file-name (ps/ebib-get-file extension)))
        (find-file file-name)
      (user-error (format "No (unique) `%s' file found" extension))))

  (defun ps/ebib-open-file-externally (extension)
    "Open file with EXTENSION in entry at point, if it (uniquely)
exists."
    (interactive)
    (if-let ((file-name (expand-file-name (ps/ebib-get-file extension))))
        (shell-command (format "open '%s'" file-name))
      (user-error (format "No (unique) `%s' file found" extension))))

  (defun ps/ebib-open-pdf-file ()
    "Open `pdf' file in entry at point, if it (uniquely) exists."
    (interactive)
    (ps/ebib-open-file "pdf"))

  (defun ps/ebib-open-webm-file ()
    "Open `webm' file in entry at point, if it (uniquely) exists."
    (interactive)
    (ps/ebib-open-file-externally "webm"))

  (defun ps/ebib-open-mp3-file ()
    "Open `webm' file in entry at point, if it (uniquely) exists."
    (interactive)
    (ps/ebib-open-file-externally "mp3"))

  (defun ps/ebib-open-pdf-file-externally ()
    "Open `pdf' file in entry at point, if it (uniquely) exists."
    (interactive)
    (ps/ebib-open-file-externally "pdf"))

  (defun ps/ebib-open-html-file ()
    "Open `html' file in entry at point, if it (uniquely) exists."
    (interactive)
    (ps/ebib-open-file "html")
    (let ((html-buffer (buffer-name))
          (browse-url-handlers nil)
          (browse-url-browser-function #'eww-browse-url))
      (browse-url-of-buffer)
      (kill-buffer html-buffer)))

  (defun ps/ebib-open-file-dwim ()
    "Open file in entry at point. If the entry contains more than one
file, use the preference ordering defined in
`ps/ebib-valid-file-extensions'."
    (interactive)
    (if-let ((extension
              (catch 'tag
                (dolist (extension ps/ebib-valid-file-extensions)
                  (when (ps/ebib-get-file extension)
                    (throw 'tag extension))))))
        (call-interactively
         (intern
          (concat "ps/ebib-open-" extension "-file")))
      (user-error "No file found.")))

  (defvar ps/ebib-valid-file-extensions
    '("pdf" "html" "webm" "flac" "mp3")
    "List of valid file extensions for Ebib, in order of preference.")

  (defun ps/ebib-validate-file-extension ()
    "If entry at point has attachments, check that the extension of
each of the attached files is in
`ps/ebib-valid-file-extensions'."
    (when-let ((files (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db t t)))
      (when
          (catch 'tag
            (mapc
             (lambda (file)
               (unless (member (file-name-extension file) ps/ebib-valid-file-extensions)
                 (throw 'tag file)))
             (ebib--split-files files))
            nil)
        (user-error "Invalid file extension."))))

  (defun ps/ebib-validate-file-stem ()
    "If entry at point has attachments, check that the stem of
each of the attached files is the entry's unique key."
    (when-let ((files (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db t t)))
      (when
          (catch 'tag
            (mapc
             (lambda (file)
               (unless (equal (file-name-nondirectory (file-name-sans-extension file))
                              (ebib--get-key-at-point))
                 (throw 'tag file)))
             (ebib--split-files files))
            nil)
        (user-error "Invalid file stem."))))

  (defun ps/ebib-validate-file-name ()
    "If entry at point has attachments, check that their stems match
the entry's unique key and that their extensions are listed in
`ps/ebib-valid-file-extensions'."
    (ps/ebib-validate-file-stem)
    (ps/ebib-valide-file-extension))

  (defun ps/ebib-rename-files ()
    "Rename files in entry at point so that the file stems match the
corresponding entry key."
    (interactive)
    (ebib--execute-when
      (entries
       (let* ((field "file")
              (key (ebib--get-key-at-point))
              (file-list (split-string
                          (ebib-get-field-value field key ebib--cur-db t t)
                          ";")))
         (unless (ps/ebib-valid-key-p key)
           (user-error "Entry has an invalid key; pleasse regenerate it."))
         (when file-list
           (ebib-delete-field-contents field t)
           (dolist (filename file-list)
             (let ((stem (file-name-sans-extension (file-name-nondirectory filename)))
                   (extension (file-name-extension filename)))
               (unless (equal stem key)
                 (let ((new-filename
                        (ps/ebib--rename-and-abbreviate-file
                         (ps/ebib--extension-directories extension)
                         key
                         extension)))
                   (rename-file filename new-filename)
                   (setq filename new-filename)))
               (ebib-set-field-value field filename key ebib--cur-db ";")))
           (ebib--redisplay-field field)
           (ebib--redisplay-index-item field))))
      ;; (ebib-save-current-database nil))))
      (default
       (beep))))

  (defun ps/ebib--rename-and-abbreviate-file (directory key extension)
    "We abbreviate the filename so that it works when invoked by
different users, as long as they have the same folder structure."
    (file-name-concat
     (abbreviate-file-name
      directory)
     (file-name-with-extension key extension)))

  (defun ps/ebib-valid-key-p (&optional key)
    "docstring"
    (let ((key (or key
                   (ebib--get-key-at-point))))
      (string-match
       "^[_[:alnum:]-]\\{2,\\}[[:digit:]]\\{4\\}[_[:alnum:]]\\{2,\\}$"
       key)))

  (defun ps/ebib-switch-old-db ()
    "docstring"
    (interactive)
    (switch-to-buffer-other-window " 1:old.bib"))

  (defun ps/ebib-switch-new-db ()
    "docstring"
    (interactive)
    (switch-to-buffer-other-window " 2:new.bib"))

  (defun ps/ebib-copy-doi ()
    "docstring"
    (interactive)
    (ebib--execute-when
      (entries
       (let ((doi (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)))
         (unless doi
           (user-error "[Ebib] No DOI found in doi field"))
         ;; FIXME: `(return doi)' fails to return DOI; why?
         (kill-new doi)))))

  (defun ps/ebib-download-doi ()
    "docstring"
    (interactive)
    (let* ((key (ebib--get-key-at-point))
           (file (file-name-concat
                  ps/dir-library-pdf
                  (file-name-with-extension key ".pdf"))))
      (ps/ebib-copy-doi)
      (url-copy-file (scihub (current-kill 0)) file)
      (ebib-set-field-value "file" file key ebib--cur-db ";")))

  ;; We tweak the two functions below so that focus doesn't move away
  ;; from the current entry when the database is saved or reloaded.

  (defun ps/ebib--save-database (db &optional force)
    "Save the database DB.
The FORCE argument is used as in `ebib-save-current-database'."
    ;; See if we need to make a backup.
    (when (and (ebib-db-backup-p db)
               (file-exists-p (ebib-db-get-filename db)))
      (ebib--make-backup (ebib-db-get-filename db))
      (ebib-db-set-backup nil db))

    ;; Check if the file has changed on disk.
    (let ((db-modtime (ebib-db-get-modtime db))
          (file-modtime (ebib--get-file-modtime (ebib-db-get-filename db))))
      ;; If the file to be saved has been newly created, both modtimes are nil.
      (when (and db-modtime file-modtime
                 (time-less-p db-modtime file-modtime))
        (unless (or (and (listp force)
                         (eq 16 (car force)))
                    (yes-or-no-p (format "File `%s' changed on disk.  Overwrite? " (ebib-db-get-filename db))))
          (error "[Ebib] File not saved"))))

    ;; Now save the database.
    (let ((buf (current-buffer)))
      (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
      (with-temp-buffer
        (ebib--format-database-as-bibtex db)
        (write-region (point-min) (point-max) (ebib-db-get-filename db)))
      ;; (ebib--pop-to-buffer
      ;; (cond ((string= (buffer-name buf) "*Ebib-entry*")
      ;; (ebib--buffer 'entry))
      ;; ((string= (buffer-name buf) "*Ebib-index*")
      ;; (ebib--buffer 'index))))
      )
    (ebib--set-modified nil db))

  (advice-add 'ebib--save-database :override #'ps/ebib--save-database)

  (defun ps/ebib-reload-current-database ()
    "Reload the current database from disk."
    (interactive)
    (ebib--execute-when
      (entries
       (when (or (and (ebib-db-modified-p ebib--cur-db)
                      (yes-or-no-p "Database modified.  Really reload from file? "))
                 (y-or-n-p "Reload current database from file? "))
         (ps/ebib-reload-current-database-no-confirm)))
      (default
       (beep))))

  (advice-add 'ebib-reload-current-database :override #'ps/ebib-reload-current-database)

  ;; hack to solve the issue whereby hitting `RET' in
  ;; `ebib-index-mode' sometimes does not edit the entry at point
  (defun ps/ebib-edit-entry ()
    "Edit the current BibTeX entry."
    (interactive)
    (ebib--execute-when
      (entries
       (unless (string= (what-line) "Line 1")
         (ebib-prev-entry)
         (ebib-next-entry))
       (ebib--edit-entry-internal))
      (default
       (beep))))

  (advice-add 'ebib-edit-entry :override #'ps/ebib-edit-entry)

  (defun ps/ebib--extension-directories (extension)
    "Return directory associated with EXTENSION."
    (cond ((string= extension "pdf")
           ps/dir-library-pdf)
          ((string= extension "html")
           ps/dir-library-html)
          ((or (string= extension "webm")
               (string= extension "mp3")
               (string= extension "flac"))
           ps/dir-library-media)
          (t
           (user-error "Invalid file extension"))))

  (defun ps/ebib-attach-file (&optional most-recent)
    "Prompt the user for a file to attach to the current entry.

If MOST-RECENT is non-nil, attach the most recent file instead."
    (interactive)
    (ebib--execute-when
      (entries
       (let ((key (ebib--get-key-at-point)))
         (unless (ps/ebib-valid-key-p key)
           (user-error "Entry has an invalid key; pleasse regenerate it."))
         (let* ((field "file")
                (file-to-attach
                 (if most-recent
                     (ps/newest-file ps/dir-downloads)
                   (let ((initial-folder
                          (completing-read "Select folder: "
                                           (list
                                            ps/dir-downloads
                                            ps/dir-library-html
                                            ps/dir-library-pdf
                                            ps/dir-library-media))))
                     (read-file-name
                      "File to attach: "
                      ;; Use key as default selection if key-based file exists
                      ;; else default to `initial-folder'
                      (if (catch 'found
                            (dolist (extension ps/ebib-valid-file-extensions)
                              (when (f-file-p (file-name-concat
                                               initial-folder
                                               (file-name-with-extension key extension)))
                                (throw 'found extension))))
                          (file-name-concat initial-folder key)
                        initial-folder)))))
                (extension (file-name-extension file-to-attach))
                (destination-folder
                 (ps/ebib--extension-directories extension))
                (file-name (ps/ebib--rename-and-abbreviate-file
                            destination-folder key extension)))
           (when (or (not (f-file-p file-name))
                     (y-or-n-p "File exists. Overwrite? "))
             (rename-file file-to-attach file-name t))
           (ps/ebib--update-file-field-contents key file-name)
           (when (string= (file-name-extension file-name) "pdf")
             ;; open the pdf to make sure it displays the web page correctly
             (ps/ebib-open-pdf-file)
             ;; ocr the pdf if necessary
             (ps/ocr-pdf (format "'%s' '%s'" (expand-file-name file-name) (expand-file-name file-name)))))))
      (default
       (beep))))

  (defun ps/ebib-search-internet-archive (&optional field search-query)
    "Run a search on the Internet Archive."
    (interactive)
    (ps/ebib-search
     ps/internet-archive
     field
     search-query))

  (defun ps/ebib-search-google-scholar (&optional field search-query)
    "Run a search on Google Scholar."
    (interactive)
    (ps/ebib-search
     ps/google-scholar
     field
     search-query))

  (defun ps/ebib-search-wikipedia (&optional field search-query)
    "Run a search on Wikipedia."
    (interactive)
    (ps/ebib-search
     ps/wikipedia
     field
     search-query))

  (defun ps/ebib-search-connected-papers (&optional field search-query)
    "Run a search on Connected Papers."
    (interactive)
    (ps/ebib-search
     ps/connected-papers
     field
     search-query))

  (defun ps/ebib-search-library-genesis (&optional field search-query)
    "Run a search on Library Genesis."
    (interactive)
    (ps/ebib-search
     ps/library-genesis
     field
     search-query))

  (defun ps/ebib-search-amazon (&optional field search-query)
    "Run a search on Amazon."
    (interactive)
    (ps/ebib-search
     ps/amazon
     field
     search-query))

  (defun ps/ebib-search-university-of-toronto (&optional field search-query)
    "Run a search on the University of Toronto Libraries."
    (interactive)
    (ps/ebib-search
     ps/university-of-toronto
     field
     search-query))

  (defun ps/ebib-search-hathitrust (&optional field search-query)
    "Run a search on HathiTrust."
    (interactive)
    (ps/ebib-search
     ps/hathitrust
     field
     search-query))

  (defun ps/ebib-search-goodreads (&optional field search-query)
    "Run a search on Goodreads."
    (interactive)
    (ps/ebib-search
     ps/goodreads
     field
     search-query))

  (defun ps/ebib-search-worldcat (&optional field search-query)
    "Run a search on Worldcat."
    (interactive)
    (ps/ebib-search
     ps/worldcat
     field
     search-query))

  (defun ps/ebib-search (search-engine &optional field search-query)
    "Run a search with SEARCH-ENGINE.

If invoked from `ebib', search for the value of FIELD. If no
FIELD is given, use the identifier if available, unless point is
in 'title' field, otherwise the field at point.

If not invoked from `ebib', prompt for search query."
    (let* ((name (nth 0 search-engine))
           (prefix (nth 1 search-engine))
           (suffix (nth 2 search-engine))
           (search-query
            (if search-query
                (url-hexify-string search-query)
              (if (member major-mode (list 'ebib-entry-mode 'ebib-index-mode))
                  (let* ((field (or field
                                    ;; if no field given, set `field'...
                                    (cond
                                     ;; ...to "title" if point is on "title" field,
                                     ((equal (ebib--current-field) "title")
                                      "title")
                                     ;; ...to "isbn" or "doi" if either field present,
                                     (ps/ebib-get-isbn)
                                     ((ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db)
                                      "doi")
                                     ;; ...else to the field at point.
                                     (t
                                      (ebib--current-field)))))
                         (value (ebib-get-field-value
                                 field
                                 (ebib--get-key-at-point)
                                 ebib--cur-db
                                 'noerror
                                 'unbraced
                                 'xref)))
                    (cond ((equal field "title")
                           (url-hexify-string value))
                          ;; ((equal field "isbn")
                          ;; (s-replace "-" "" value))
                          (t
                           value)))
                (url-hexify-string
                 (read-string
                  (concat
                   name
                   ": ")))))))
      (browse-url (concat prefix search-query suffix))))

  (defun ps/ebib-search-multi (&optional field search-query search-group)
    "docstring"
    (let ((search-query (or search-query
                            (unless (member major-mode (list 'ebib-entry-mode 'ebib-index-mode))
                              (read-string "Search query: ")))))
      (dolist (search-engine search-group)
        (funcall search-engine field search-query))))

  (defun ps/ebib-search-book (&optional field search-query)
    "The list of search engines is specified by the variable
`ps/ebib-search-book'."
    (interactive)
    (ps/ebib-search-multi field search-query ps/ebib-search-book))

  (defun ps/ebib-download-book (&optional field search-query)
    "The list of search engines is specified by the variable
`ps/ebib-download-book'."
    (interactive)
    (ps/ebib-search-multi field search-query ps/ebib-download-book))

  (defun ps/ebib-download-isbn (&optional field search-query)
    "The list of search engines is specified by the variable
`ps/ebib-download-isbn'."
    (interactive)
    (ps/ebib-search-multi field search-query ps/ebib-download-book))

  (defun ps/ebib-search-article (&optional field search-query)
    "The list of search engines is specified by the variable
`ps/ebib-search-article'."
    (interactive)
    (ps/ebib-search-multi field search-query ps/ebib-search-article))

  (defun ps/ebib-download-article (&optional field search-query)
    "The list of search engines is specified by the variable
`ps/ebib-download-article'."
    (interactive)
    (ps/ebib-search-multi field search-query ps/ebib-search-article))

  (defun ps/ebib-search-book-by-title ()
    "docstring"
    (interactive)
    (if (or (ebib-get-field-value "booktitle" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
            (ebib-get-field-value "title" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
            (user-error "`title' field is empty!"))
        (ps/ebib-search-book "title")))

  (defun ps/ebib-download-book-by-title ()
    "docstring"
    (interactive)
    (if (or (ebib-get-field-value "booktitle" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
            (ebib-get-field-value "title" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
            (user-error "`title' field is empty!"))
        (ps/ebib-download-book "title")))

  (defun ps/ebib-search-book-by-isbn ()
    "docstring"
    (interactive)
    (if (ebib-get-field-value "isbn" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
        (ps/ebib-search-book "isbn")
      (user-error "`ISBN' field is empty!")))

  (defun ps/ebib-search-article-by-title ()
    "docstring"
    (interactive)
    (if (ebib-get-field-value "article" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
        (user-error "`title' field is empty!")
      (ps/ebib-search-article "title")))

  (defun ps/ebib-download-article-by-title ()
    "docstring"
    (interactive)
    (if (ebib-get-field-value "article" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
        (user-error "`title' field is empty!")
      (ps/ebib-download-article "title")))

  (defun ps/ebib-download-video (id)
    ""
    (let* ((key (ebib--get-key-at-point))
           (file-name
            (ps/ebib--rename-and-abbreviate-file
             ps/dir-library-media key "webm")))
      (youtube-dl id :directory ps/dir-library-media :destination key)
      (message (format "Downloading video from '%s'" (substring-no-properties id)))
      (ps/ebib--update-file-field-contents key file-name)))

  (defun ps/ebib-search-article-by-doi ()
    "docstring"
    (interactive)
    (if (ebib-get-field-value "doi" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)
        (ps/ebib-search-article "doi")
      (user-error "`DOI' field is empty!")))

  (defun ps/ebib-sentence-case ()
    "docstring"
    (interactive)
    (ebib--execute-when
      (entries
       (let* ((field (ebib--current-field))
              (value (ebib-get-field-value field (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref))
              (words (split-string value)))
         (setq words (mapcar
                      (lambda (word)
                        (if
                            ;; match words containing {} or \ which are probably
                            ;; LaTeX or protected words
                            (string-match "\\$\\|{\\|}\\|\\\\" word)
                            word
                          (s-downcase word)))
                      words))
         ;; capitalize first word
         (setf (car words) (s-capitalize (car words)))
         (setq value (mapconcat 'identity words " "))
         (ebib-set-field-value field value (ebib--get-key-at-point) ebib--cur-db 'overwrite 'unbraced)
         (ebib--store-multiline-text (current-buffer))
         (ebib--redisplay-field field)
         (ebib--redisplay-index-item field)
         (ebib-save-current-database nil)))
      (default
       (beep))))

  (defun ps/ebib-open-current-bibtex-file ()
    "Open bibtex file associated with the current entry."
    (interactive)
    (when-let ((file (ebib-db-get-filename ebib--cur-db)))
      (find-file file)))

  (defun ps/ebib-open-key (key)
    "Open the entry for KEY in Ebib."
    (ebib--execute-when
      (entries
       (let ((file))
         (if (member key (ebib-db-list-keys (nth 0 ebib--databases)))
             (setq file ps/file-bibliography-new)
           (setq file ps/file-bibliography-old))
         (ebib file key)
         (ebib--pop-to-buffer (ebib--buffer 'entry))))
      (default
       (ebib--error "No database is loaded"))))

  (defun ps/ebib-fix-authors ()
    "Rearrange the name of each author so that last name comes before
first name, and names are separated by a semicolon."
    (interactive)
    (ebib--execute-when
      (entries
       (let ((field "author")
             (authors-fixed))
         (when-let ((authors
                     (replace-regexp-in-string
                      "\n "
                      ""
                      (substring-no-properties
                       (ebib-get-field-value
                        field
                        (ebib--get-key-at-point)
                        ebib--cur-db
                        'noerror
                        'unbraced
                        'xref)))))
           (dolist (author (split-string authors ", "))
             (push (replace-regexp-in-string "\\(.*\\) \\(.*\\)" "\\2, \\1" author) authors-fixed))
           (ebib-set-field-value
            field
            (string-join authors-fixed "; ")
            (ebib--get-key-at-point)
            ebib--cur-db
            'overwrite)
           (ebib--store-multiline-text (current-buffer))
           (ebib--redisplay-field field)
           (ebib--redisplay-index-item field)
           (ebib-save-current-database nil))))
      (default
       (beep))))

  (defvar ps/ebib-sort-toggle 'Title)

  (defun ps/ebib-sort-toggle ()
    "Toggle between sorting by timestamp, author, and title."
    (interactive)
    (unless (string= (ebib-db-get-filename ebib--cur-db) ps/file-bibliography-new)
      (user-error (format "Due to performane issues, this command only works on database `%s'"
                          (file-name-nondirectory ps/file-bibliography-new))))
    (ebib--execute-when
      (entries
       (let ((order 'ascend))
         (cond ((eq ps/ebib-sort-toggle 'Timestamp)
                (setq ps/ebib-sort-toggle 'Author))
               ((eq ps/ebib-sort-toggle 'Author)
                (setq ps/ebib-sort-toggle 'Title))
               ((eq ps/ebib-sort-toggle 'Title)
                (setq ps/ebib-sort-toggle 'Timestamp)
                (setq order 'descend)))
         (ebib--index-sort (symbol-name ps/ebib-sort-toggle) order)
         (goto-char (point-min))
         (message (format "Sorting by %s" ps/ebib-sort-toggle))))
      (default
       (beep))))

  (defun ps/ebib-merge-databases (source target)
    "Move the contents of SOURCE to TARGET."
    (find-file-noselect source)
    (find-file-noselect target)
    (with-current-buffer (file-name-nondirectory target)
      (goto-char (point-max))
      (insert-file-contents source))
    (with-current-buffer (file-name-nondirectory source)
      (erase-buffer)))

  (defun ps/ebib-merge-old-new ()
    "Merge the old and new bibliography files."
    (interactive)
    (ps/ebib-merge-databases ps/file-bibliography-old ps/file-bibliography-new))

  (defun ps/ebib-end-of-index-buffer ()
    "Move to the end of the index buffer."
    (interactive)
    (when (equal major-mode 'ebib-index-mode)
      (goto-char (point-max))
      (forward-line -1)))

  (defun ps/ebib-duplicate-entry ()
    "Duplicate the entry at point."
    (interactive)
    (ebib--execute-when
      (entries
       (let ((key (ebib--get-key-at-point)))
         (save-window-excursion
           (find-file zotra-default-bibliography)
           (goto-char (point-max))
           (ebib--format-entry key ebib--cur-db)
           (bibtex-narrow-to-entry)
           (goto-char (point-min))
           (replace-regexp "^\\(@.*{.*\\)," "\\1-dup,")
           (save-buffer))
         (ebib-switch-to-database-nth 1)
         (ps/ebib-reload-current-database-no-confirm)))
      (default
       (beep))))

  :hook
  (ebib-index-mode-hook . doom-modeline-mode)
  (ebib-entry-mode-hook . visual-line-mode)

  :general
  ("A-i" 'ps/ebib-open-or-switch)
  (ebib-multiline-mode-map
   "s-c" 'ebib-quit-multiline-buffer-and-save)
  (ebib-entry-mode-map
   "TAB" 'ebib-goto-next-set
   "<backtab>" 'ebib-goto-prev-set
   "SPC" 'ps/ebib-open-file-dwim
   "H-s" 'ebib-save-current-database
   "!" 'ebib-generate-autokey
   "," 'ps/ebib-title-case
   "." 'ps/ebib-sentence-case
   "/" (lambda! (ps/ebib-attach-file t))
   "?" 'ps/ebib-attach-file
   "a" 'ps/ebib-search-amazon
   "A" 'ebib-add-field
   "b" 'ps/ebib-open-current-bibtex-file
   "c" 'ebib-copy-current-field-contents
   "D" 'ebib-delete-current-field-contents
   "d" 'ps/ebib-download-dwim
   "E" 'ebib-edit-keyname
   "g" 'ps/ebib-search-library-genesis
   "G" 'ps/ebib-search-goodreads
   "h" 'ps/ebib-open-html-file
   "i" 'ebib-browse-doi
   "i" 'ps/ebib-download-by-identifier
   "I" 'ps/ebib-search-book-by-isbn
   "o" 'ps/ebib-search-connected-papers
   "p" 'ps/ebib-open-pdf-file
   "P" 'ps/ebib-open-pdf-file-externally
   "Q" 'ebib-quit
   "r" 'ps/ebib-rename-files
   "s" 'ps/ebib-search-dwim
   "t" 'ps/ebib-search-book-dwim
   "u" 'ebib-browse-url
   "v" 'ps/ebib-open-webm-file
   "V" 'ps/ebib-search-internet-archive
   "w" 'ps/ebib-search-worldcat
   "W" 'ps/ebib-search-wikipedia
   "x" 'ps/ebib-search-university-of-toronto
   "y" 'ps/ebib-search-hathitrust
   "z" 'ps/ebib-search-google-scholar
   "1" (lambda! (ebib-view-file-in-field 1))
   "2" (lambda! (ebib-view-file-in-field 2)))
  (ebib-index-mode-map
   "<return>" 'ebib-edit-entry
   "A-C-s-SPC" 'ps/ebib-end-of-index-buffer
   "d" 'ps/ebib-duplicate-entry
   "D" 'ebib-delete-entry
   "k" 'ebib-prev-entry
   "l" 'ebib-next-entry
   "s" 'ps/ebib-sort-toggle
   "H-s" 'ebib-save-current-database
   "Q" 'ebib-quit))

(use-package biblio
  :defer 15
  :config
  (defvar ps/biblio-arxiv-regexp "\\([[:digit:]]\\{4\\}\\.[[:digit:]]\\{4,5\\}\\)")
  (defun ps/biblio-arxiv-id-p (string)
    "Return `t' if string has the form of an arXiv ID."
    (string-match ps/biblio-arxiv-regexp string)))

(use-package zotra
  :straight (zotra
             :host github
             :repo "mpedramfar/zotra")
  :demand t
  :init
  (defun ps/zotra-run-translator-server ()
    "Start translator server in the background."
    (interactive)
    (let ((shell-command-buffer-name-async "*zotra-translation-server*"))
      (async-shell-command
       (format
        "cd %s; nvm use 14; npm start"
        ps/dir-translation-server))))

  (run-with-timer 5 nil 'ps/zotra-run-translator-server)

  :custom
  (zotra-use-curl nil)
  (zotra-url-retrieve-timeout 10)
  (zotra-default-bibliography ps/file-bibliography-new)
  (zotra-default-entry-format "biblatex")
  (zotra-after-add-entry-hook nil)
  (zotra-download-attachment-default-directory ps/dir-downloads)
  (zotra-backend 'translation-server)
  (zotra-cli-command (file-name-concat ps/dir-dropbox "source/zotra-cli/bin/index.js"))

  :config
  (defun ps/zotra-add-entry (url-or-search-string &optional is-search bibfile entry-format)
    (let ((bibfile
           (or bibfile zotra-default-bibliography
               (completing-read
                "Bibfile: "
                (append (directory-files "." t ".*\\.bib$")
                        (org-cite-list-bibliography-files))))))
      (find-file bibfile)
      (widen)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (insert (zotra-get-entry url-or-search-string is-search entry-format))
      (save-excursion
        (save-restriction
          (bibtex-narrow-to-entry)
          (bibtex-beginning-of-entry)
          (goto-char (point-max))
          (when (not (looking-at "^")) (insert "\n"))
          (advice-add 'select-safe-coding-system-interactively :override #'ps/select-safe-coding-system-interactively)
          (save-buffer)
          (advice-remove 'select-safe-coding-system-interactively #'ps/select-safe-coding-system-interactively)
          (run-hooks 'zotra-after-add-entry-hook)))))

  (defun ps/zotra-after-add-entry-hook-function ()
    "Function to trigger with `zotra-after-add-entry-hook'."
    (revert-buffer nil t)
    (goto-char (point-max))
    (bibtex-set-field "timestamp" (format-time-string ebib-timestamp-format nil "GMT"))
    (bibtex-clean-entry)
    (org-ref-clean-bibtex-entry)
    (save-buffer)
    (ps/ebib-reload-current-database-no-confirm)
    (let ((citekey (ps/bibtex-get-key)))
      (ebib ps/file-bibliography-new citekey)
      (ps/ebib-open-key citekey)))

  (defun ps/select-safe-coding-system-interactively (from to codings unsafe
                                                          &optional rejected default)
    "Select interactively a coding system for the region FROM ... TO.
FROM can be a string, as in `write-region'.
CODINGS is the list of base coding systems known to be safe for this region,
  typically obtained with `find-coding-systems-region'.
UNSAFE is a list of coding systems known to be unsafe for this region.
REJECTED is a list of coding systems which were safe but for some reason
  were not recommended in the particular context.
DEFAULT is the coding system to use by default in the query."
    ;; At first, if some defaults are unsafe, record at most 11
    ;; problematic characters and their positions for them by turning
    ;;	(CODING ...)
    ;; into
    ;;	((CODING (POS . CHAR) (POS . CHAR) ...) ...)
    (if unsafe
        (setq unsafe
              (mapcar (lambda (coding)
                        (cons coding
                              (if (stringp from)
                                  (mapcar (lambda (pos)
                                            (cons pos (aref from pos)))
                                          (unencodable-char-position
                                           0 (length from) coding
                                           11 from))
                                (mapcar (lambda (pos)
                                          (cons pos (char-after pos)))
                                        (unencodable-char-position
                                         from to coding 11)))))
                      unsafe)))

    (setq codings (sanitize-coding-system-list codings))

    (let ((window-configuration (current-window-configuration))
          (bufname (buffer-name))
          coding-system)
      (save-excursion
        ;; If some defaults are unsafe, make sure the offending
        ;; buffer is displayed.
        (when (and unsafe (not (stringp from)))
          (pop-to-buffer bufname)
          (goto-char (apply #'min (mapcar (lambda (x) (or (car (cadr x)) (point-max)))
                                          unsafe))))
        ;; Then ask users to select one from CODINGS while showing
        ;; the reason why none of the defaults are not used.
        (with-output-to-temp-buffer "*Warning*"
          (with-current-buffer standard-output
            (if (and (null rejected) (null unsafe))
                (insert "No default coding systems to try for "
                        (if (stringp from)
                            (format "string \"%s\"." from)
                          (format-message "buffer `%s'." bufname)))
              (insert
               "These default coding systems were tried to encode"
               (if (stringp from)
                   (concat " \"" (if (> (length from) 10)
                                     (concat (substring from 0 10) "...\"")
                                   (concat from "\"")))
                 (format-message
                  " the following\nproblematic characters in the buffer `%s'"
                  bufname))
               ":\n")
              (select-safe-coding-system--format-list unsafe)
              (when rejected
                (insert "These safely encode the text in the buffer,
but are not recommended for encoding text in this context,
e.g., for sending an email message.\n ")
                (dolist (x rejected)
                  (princ " ") (princ x))
                (insert "\n"))
              (when unsafe
                (insert (if rejected "The other coding systems"
                          "However, each of them")
                        (substitute-command-keys
                         " encountered characters it couldn't encode:\n"))
                (dolist (coding unsafe)
                  (insert (format "  %s cannot encode these:" (car coding)))
                  (let ((i 0)
                        (func1
                         (lambda (bufname pos)
                           (when (buffer-live-p (get-buffer bufname))
                             (pop-to-buffer bufname)
                             (goto-char pos))))
                        (func2
                         (lambda (bufname pos coding)
                           (when (buffer-live-p (get-buffer bufname))
                             (pop-to-buffer bufname)
                             (if (< (point) pos)
                                 (goto-char pos)
                               (forward-char 1)
                               (search-unencodable-char coding)
                               (forward-char -1))))))
                    (dolist (elt (cdr coding))
                      (insert " ")
                      (if (stringp from)
                          (insert (if (< i 10) (cdr elt) "..."))
                        (if (< i 10)
                            (insert-text-button
                             (cdr elt)
                             :type 'help-xref
                             'face 'link
                             'help-echo
                             "mouse-2, RET: jump to this character"
                             'help-function func1
                             'help-args (list bufname (car elt)))
                          (insert-text-button
                           "..."
                           :type 'help-xref
                           'face 'link
                           'help-echo
                           "mouse-2, RET: next unencodable character"
                           'help-function func2
                           'help-args (list bufname (car elt)
                                            (car coding)))))
                      (setq i (1+ i))))
                  (insert "\n"))
                (insert (substitute-command-keys "\

Click on a character (or switch to this window by `\\[other-window]'\n\
and select the characters by RET) to jump to the place it appears,\n\
where `\\[universal-argument] \\[what-cursor-position]' will give information about it.\n"))))
            (insert (substitute-command-keys "\nSelect \
one of the safe coding systems listed below,\n\
or cancel the writing with \\[keyboard-quit] and edit the buffer\n\
   to remove or modify the problematic characters,\n\
or specify any other coding system (and risk losing\n\
   the problematic characters).\n\n"))
            (let ((pos (point))
                  (fill-prefix "  "))
              (dolist (x codings)
                (princ "  ") (princ x))
              (insert "\n")
              (fill-region-as-paragraph pos (point)))))

        ;; Read a coding system.
        (setq coding-system 'utf-8)
        (setq last-coding-system-specified coding-system))

      (kill-buffer "*Warning*")
      (set-window-configuration window-configuration)
      coding-system))

  ;; Ugly hack to fix encoding issues.
  ;; TODO: Fix this properly.
  (advice-add 'zotra-add-entry :override #'ps/zotra-add-entry)

  ;; (defun ps/zotra-add-entry-from-search-advice ()
  ;; "Functionsps/ebib-sort-toggle to call after `zotra-add-entry-from-search' is run."
  ;; (ebib-switch-to-database-nth 1)
  ;; (ps/ebib-reload-current-database-no-confirm))

  ;; (advice-add 'zotra-add-entry-from-search :after #'ps/zotra-add-entry-from-search-advice)

  :hook
  (zotra-after-add-entry-hook . ps/zotra-after-add-entry-hook-function)

  :general
  (ebib-index-mode-map
   "i" 'zotra-add-entry-from-search))

(use-package scihub
  :custom
  (scihub-download-directory ps/dir-downloads)
  (scihub-open-after-download nil)
  (scihub-fetch-domain 'scihub-fetch-domains-lovescihub)
  ;; run `scihub-homepage' for a list of available scihub webservers
  ;; if the URL below stops working
  (scihub-homepage "http://sci-hub.wf"))

(use-package anki-editor
  :disabled
  ;; this version handles mathjax correctly
  ;; :straight (:fork (:repo "louietan/anki-editor" :branch "develop"))
  ;; this version simplifies the tree structure
  ;; :straight
  ;; (:type git :host github :repo "louietan/anki-editor"
  ;; :fork (:host github :repo "leoc/anki-editor"
  ;; :branch "develop"))
  :if (equal (system-name) ps/computer-hostname-pablo)
  :init
  (setq-default anki-editor-use-math-jax t) ; github.com/louietan/anki-editor/issues/60#issuecomment-617441799
  ;; create custom key map
  (progn
    (defvar anki-editor-mode-map (make-sparse-keymap))
    (add-to-list 'minor-mode-map-alist (cons 'anki-editor-mode
                                             anki-editor-mode-map)))

  :custom
  (anki-editor-create-decks t)
  (anki-editor-org-tags-as-anki-tags t)
  :config
  (defun ps/anki-editor-open-note-externally ()
    "Copy note id to clipboard, switch to Anki desktop, and open note in browser."
    (interactive)
    (let ((note-id (org-entry-get nil "ANKI_NOTE_ID")))
      (if (not note-id)
          (error "Note id not found")
        (progn
          (kill-new (concat "nid:" note-id))
          (shell-command "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"496A3425-8985-4117-AE0F-ABD6DC85FB9F\"'")))))

  (defun ps/anki-editor-push-notes-under-heading (&optional match scope)
    "Push notes under heading to Anki."
    (interactive)
    (anki-editor-push-notes '(4) match scope))

  ;; the two modified functions below allow for notes with empty fields to be pushed without error
  ;; github.com/leoc/anki-editor/pull/1
  (defun ps/anki-editor--build-field-from-content-at-point (name)
    "Build a field with NAME entry from the heading at point."
    (let* ((element (org-element-at-point))
           (format (anki-editor-entry-format))
           (begin (cl-loop for eoh = (org-element-property :contents-begin element)
                           then (org-element-property :end subelem)
                           while eoh
                           for subelem = (progn
                                           (goto-char eoh)
                                           (org-element-context))
                           while (memq (org-element-type subelem)
                                       '(drawer planning property-drawer))
                           finally return (and eoh (org-element-property :begin subelem))))
           (end (org-element-property :contents-end element))
           (raw (or (and begin
                         end
                         (buffer-substring-no-properties
                          begin
                          ;; in case the buffer is narrowed,
                          ;; e.g. by `org-map-entries' when
                          ;; scope is `tree'
                          (min (point-max) end)))
                    "")))
      (cons name (anki-editor--export-string raw format))))

  (defun ps/anki-editor--build-fields ()
    "Build a list of fields from subheadings of current heading.

Return a list of cons of (FIELD-NAME . FIELD-CONTENT)."
    (save-excursion
      (cl-loop with inhibit-message = t ; suppress echo message from `org-babel-exp-src-block'
               initially (unless (org-goto-first-child)
                           (cl-return `(,(anki-editor--build-field-from-content-at-point "Back"))))
               for last-pt = (point)
               for element = (org-element-at-point)
               for heading = (substring-no-properties
                              (org-element-property :raw-value element))
               for format = (anki-editor-entry-format)
               ;; contents-begin includes drawers and scheduling data,
               ;; which we'd like to ignore, here we skip these
               ;; elements and reset contents-begin.
               for begin = (cl-loop for eoh = (org-element-property :contents-begin element)
                                    then (org-element-property :end subelem)
                                    while eoh
                                    for subelem = (progn
                                                    (goto-char eoh)
                                                    (org-element-context))
                                    while (memq (org-element-type subelem)
                                                '(drawer planning property-drawer))
                                    finally return (and eoh (org-element-property :begin subelem)))
               for end = (org-element-property :contents-end element)
               for raw = (or (and begin
                                  end
                                  (buffer-substring-no-properties
                                   begin
                                   ;; in case the buffer is narrowed,
                                   ;; e.g. by `org-map-entries' when
                                   ;; scope is `tree'
                                   (min (point-max) end)))
                             "")
               for content = (anki-editor--export-string raw format)
               collect (cons heading content)
               ;; proceed to next field entry and check last-pt to
               ;; see if it's already the last entry
               do (org-forward-heading-same-level nil t)
               until (= last-pt (point)))))

  (advice-add #'anki-editor--build-fields :override #'ps/anki-editor--build-fields)
  (advice-add #'anki-editor--build-field-from-content-at-point :override #'ps/anki-editor--build-field-from-content-at-point)

  :general
  ;; ("A-i" 'anki-editor-mode)
  (anki-editor-mode-map
   "s-z" 'anki-editor-cloze-region
   "s-i" 'anki-editor-insert-note
   "s-h" 'ps/anki-editor-push-notes-under-heading
   "s-c" 'anki-editor-push-new-notes
   "s-a" 'anki-editor-push-notes ; push all notes
   "s-x" 'ps/anki-editor-open-note-externally))

(use-package org-anki
  :disabled)

(use-package org-drill
  :disabled
  :config
  (add-to-list 'org-modules 'org-drill))

(use-feature simple
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (read-mail-command 'mu4e))

(use-package smtpmail-multi
  :after mu4e
  :demand t
  :custom
  (smtpmail-multi-accounts
   `((gmail . (,ps/personal-gmail
               "smtp.gmail.com"
               587
               ,ps/personal-gmail
               starttls nil nil nil))
     (gpe . (,ps/personal-gpe-email
             "smtp.gmail.com"
             587
             ,ps/personal-gpe-email
             starttls nil nil nil))))

  (smtpmail-multi-associations
   `((,ps/personal-gmail gmail)
     (,ps/personal-gpe-email gpe)))

  (smtpmail-multi-default-account (quote gmail)))

(use-feature sendmail
  :custom
  (send-mail-function 'smtpmail-multi-send-it))

(use-feature message
  :custom
  (message-kill-buffer-on-exit t) ; `message-send-and-exit' kills buffer, rather than burying it
  (message-send-mail-function 'smtpmail-multi-send-it)

  :hook
  (message-mode-hook . (lambda () (auto-fill-mode -1)))
  (message-send-hook . message-lint)

  :general
  ((message-mode-map org-msg-edit-mode-map)
   "s-c" 'message-send-and-exit
   "s-f" 'message-goto-from
   "s-s" 'message-goto-subject
   "s-t" 'message-goto-to
   "s-A-b" 'message-goto-bcc
   "s-A-c" 'message-goto-cc
   "s-A-s" 'message-send)
  (message-mode-map
   "s-b" 'message-goto-body))

(use-package message-extras
  :straight (message-extras
             :host github
             :repo "oantolin/emacs-config"
             :files ("my-lisp/message-extras.el"))
  :demand t

  :hook
  (eww-mode-hook . shr-heading-setup-imenu)

  :general
  (eww-mode-map
   "A-C-s-r" 'shr-heading-previous
   "A-C-s-f" 'shr-heading-next))

(use-feature mml
  :general
  (message-mode-map
   "s-a" 'mml-attach-file))

(use-package mu4e
  :if (equal (system-name) ps/computer-hostname-pablo)
  :defer 5
  :straight (:local-repo
             "/opt/homebrew/Cellar/mu/1.8.14/share/emacs/site-lisp/mu/mu4e"
             :pre-build
             ())
  :init
  (setq ps/mu4e-inbox-folder "/Inbox")
  (setq ps/mu4e-daily-folder "/Daily")

  :custom
  ;; (mu4e-debug t) ; uncomment when debugging
  (mu4e-split-view 'single-window)
  (mu4e-headers-show-target nil)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 180)
  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder "/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/[Gmail]/All Mail")
  (mu4e-trash-folder "/[Gmail]/Trash")
  (mu4e-attachment-dir ps/dir-downloads)
  (mu4e-change-filenames-when-moving t)
  (mu4e-maildir-shortcuts
   `((,ps/mu4e-inbox-folder . ?i)
     (,ps/mu4e-daily-folder . ?y)
     (,mu4e-drafts-folder  . ?d)
     (,mu4e-sent-folder    . ?t)
     (,mu4e-trash-folder   . ?x)
     (,mu4e-refile-folder  . ?a))) ; required for correct Gmail refiling
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed t)
  (mu4e-confirm-quit nil)
  (mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (mu4e-headers-include-related nil)
  (mu4e-headers-results-limit 1000)
  (mu4e-headers-visible-lines 25)
  (mu4e-hide-index-messages t)
  (mu4e-html2text-command 'mu4e-shr2text) ; requires `mu4e-contrib'
  (mu4e-sent-messages-behavior 'delete) ; Gmail already keeps a copy
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  ;; performance improvements
  ;; groups.google.com/g/mu-discuss/c/hRRNhM5mwr0
  ;; djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html
  (mu4e-index-cleanup t) ; `nil' improves performance"
  (mu4e-index-lazy-check) ; `t' improves performance"

  :config
  (require 'mu4e-contrib)

  (mu4e t)
  (defun ps/mu4e-gmail-fix-flags (mark msg)
    (message "fixing flags") ; for testing
    (cond ((eq mark 'trash)  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          ((eq mark 'refile) (mu4e-action-retag-message msg "-\\Inbox"))
          ((eq mark 'flag)   (mu4e-action-retag-message msg "+\\Starred"))
          ((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred"))))

  (defun ps/mu4e-headers-archive ()
    "In headers mode, archive message at point, without asking for
confirmation."
    (interactive)
    (mu4e-headers-mark-for-refile)
    (mu4e-mark-execute-all t))

  (defun ps/mu4e-view-archive ()
    "In view mode, archive message at point, without asking for
confirmation."
    (interactive)
    (mu4e-view-mark-for-refile)
    (mu4e-mark-execute-all t))

  (defun ps/mu4e-headers-org-capture (&optional arg)
    "In headers mode, capture message at point with `org-capture'
 and, unless invoked with a prefix argument, archive it."
    (interactive)
    (org-capture nil "e")
    (unless arg
      (ps/mu4e-headers-archive)))

  (defun ps/mu4e-view-org-capture (&optional arg)
    "In view mode, capture message at point with `org-capture' and,
 unless invoked with a prefix argument, archive it."
    (interactive)
    (org-capture nil "e")
    (unless arg
      (ps/mu4e-view-archive)))

  (defun ps/mu4e-headers-trash ()
    "In headers mode, trash message at point without asking for
confirmation."
    (interactive)
    (mu4e-headers-mark-for-trash)
    (mu4e-mark-execute-all t))

  (defun ps/mu4e-view-trash ()
    "In view mode, trash message at point without asking for
confirmation."
    (interactive)
    (mu4e-view-mark-for-trash)
    (mu4e-mark-execute-all t))

  (defun ps/mu4e-headers-move ()
    "In headers mode, move and execute message at point without
asking for confirmation."
    (interactive)
    (mu4e-headers-mark-for-move)
    (mu4e-mark-execute-all t))

  (defun ps/mu4e-view-move ()
    "In view mode, move and execute message at point without asking
for confirmation."
    (interactive)
    (mu4e-view-mark-for-move)
    (mu4e-mark-execute-all t))

  ;; github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org#creating-a-mail-processing-workflow
  (defun ps/store-link-to-mu4e-query ()
    (interactive)
    (let ((org-mu4e-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))

  ;; github.com/danielfleischer/mu4easy#mu4e
  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                ;; Here's the main difference to the regular trash mark, no +T
                ;; before -N so the message is not marked as IMAP-deleted:
                :action (lambda (docid msg target)
                          (mu4e~proc-move docid
                                          (mu4e~mark-check-target target) "+S-u-N"))))

  (defun ps/mu4e-view-in-gmail ()
    "Open Gmail in a browser and view message at point in it."
    (interactive)
    (let* ((id (url-hexify-string
                (plist-get (mu4e-message-at-point) :message-id)))
           (url (concat "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A" id)))
      (browse-url url)))

  ;; github.com/djcb/mu/issues/2198
  ;; djcbsoftware.nl/code/mu/mu4e/Retrieving-mail.html
  (defun ps/mu4e-reindex-db ()
    "Reindex `mu' database."
    (interactive)
    (when (shell-command "pkill -2 -u $UID mu")
      (shell-command "sleep 1")
      (shell-command "mu index")))

  (defun ps/mu4e-copy-sum ()
    "Copy amount in subject line."
    (interactive)
    (when (eq major-mode 'mu4e-headers-mode)
      (save-excursion
        (re-search-forward "\\(\\$\\)\\([[:digit:]]+.[[:digit:]]+\\)")
        (kill-new (match-string 2)))))

  (defun ps/mu4e-compose-new-externally ()
    "Start writing a new message in Gmail."
    (interactive)
    (browse-url "https://mail.google.com/mail/u/0/#inbox?compose=new"))

  (defun ps/mu4e-mark-execute-all-no-confirm ()
    "Execute the actions for all marked messages in this buffer,
without asking for user confirmation."
    (interactive)
    (mu4e-mark-execute-all))

  (defun ps/mu4e-goto-archive ()
    "Go to `archive' folder."
    (interactive)
    (mu4e-headers-search (concat "maildir:\"" mu4e-archive-folder "\"")))

  (defun ps/mu4e-goto-daily ()
    "Go to `daily' folder."
    (interactive)
    (mu4e-headers-search (concat "maildir:\"" mu4e-daily-folder "\"")))

  (defun ps/mu4e-goto-drafts ()
    "Go to `drafts' folder."
    (interactive)
    (mu4e-headers-search (concat "maildir:\"" mu4e-drafts-folder "\"")))

  (defun ps/mu4e-goto-inbox ()
    "Go to `inbox' folder."
    (interactive)
    (mu4e-headers-search (concat "maildir:\"" mu4e-inbox-folder "\"")))

  (defun ps/mu4e-goto-sent ()
    "Go to `sent' folder."
    (interactive)
    (mu4e-headers-search (concat "maildir:\"" mu4e-sent-folder "\"")))

  (defun ps/mu4e-goto-trash ()
    "Go to `trash' folder."
    (interactive)
    (mu4e-headers-search (concat "maildir:\"" mu4e-trash-folder "\"")))

  (defun ps/mu4e-update-all-mail (arg)
    "Get new mail by running `mu4e-get-mail-command' set to `mbsync
-a'."
    (interactive "P")
    (let ((mu4e-get-mail-command
           (if arg
               "mbsync -a --pull-new"
             "mbsync -a")))
      (mu4e-update-mail-and-index t)))

  (defun ps/mu4e-set-account ()
    "Set the account for composing a message."
    (let ((mail
           (cdar
             (ignore-errors
               (mu4e-message-field mu4e-compose-parent-message :to)))))
      (if mail
          (setq user-mail-address mail)
        (setq user-mail-address ps/personal-gmail))))

  (defun ps/mu4e-headers-mark-read-and-archive ()
    "In headers mode, mark message at point and read and archive
it, without asking for confirmation."
    (interactive)
    (mu4e-headers-mark-for-read)
    (mu4e-mark-execute-all t)
    (forward-line -1)
    (ps/mu4e-headers-archive))

  ;; I believe the functions below are no longer needed
  (defun ps/mu4e-view-mode-hook-functions ()
    "Functions to be called by `mu4e-view-mode-hook'."
    (toggle-truncate-lines 1)
    (set-face-attribute
     'variable-pitch nil :family ps/face-variable-pitch :height 1.25))

  (defun ps/mu4e-view-mode-leave-hook-functions ()
    "Functions to be called by `change-major-mode-hook' when
 leaving `mu4e-view-mode'."
    (when (eq major-mode 'mu4e-view-mode)
      (set-face-attribute
       'variable-pitch nil :family ps/face-variable-pitch :height 1.4)))

  :hook
  (after-change-major-mode-hook . ps/mu4e-view-mode-leave-hook-functions)
  ;; (mu4e-view-mode-hook . ps/mu4e-view-mode-hook-functions)
  (mu4e-mark-execute-pre-hook . ps/mu4e-gmail-fix-flags)
  (mu4e-compose-pre-hook . org-msg-mode)
  (mu4e-compose-pre-hook . ps/mu4e-set-account)

  :general
  ("A-m" 'mu4e)
  ((mu4e-main-mode-map mu4e-headers-mode-map mu4e-view-mode-map)
   "c" 'mu4e-compose-new)
  ((mu4e-headers-mode-map mu4e-view-mode-map)
   "$" 'ps/mu4e-copy-sum
   "," 'mu4e-copy-message-path
   "<" 'mu4e-headers-split-view-shrink
   ">" 'mu4e-headers-split-view-grow
   "s-f" 'mu4e-compose-forward
   "i" 'mu4e-select-other-view
   "r" 'mu4e-compose-reply)
  (mu4e-main-mode-map
   "a" 'ps/mu4e-goto-archive
   "d" 'ps/mu4e-goto-drafts
   "g" 'ps/mu4e-compose-new-externally
   "h" 'mu4e-display-manual
   "i" 'ps/mu4e-goto-inbox
   "j" 'mu4e~headers-jump-to-maildir
   "r" 'ps/mu4e-reindex-db
   "s" 'mu4e-headers-search
   "t" 'ps/mu4e-goto-send
   "u" 'mu4e-update-mail-and-index
   "y" 'ps/mu4e-goto-daily
   "x" 'ps/mu4e-goto-trash)
  (mu4e-headers-mode-map
   "*" 'mu4e-headers-mark-all
   "A" 'mu4e-headers-mark-all-unread-read
   "d" 'mu4e-headers-mark-for-delete
   "D" 'ps/mu4e-headers-trash
   "e" 'ps/mu4e-headers-archive
   "E" 'ps/mu4e-headers-mark-read-and-archive
   "f" 'ps/mu4e-headers-view-message
   "k" 'mu4e-headers-prev
   "l" 'mu4e-headers-next
   "m" 'mu4e-headers-mark-for-something
   "o" 'ps/mu4e-headers-org-capture
   "R" 'mu4e-headers-mark-for-refile
   "V" 'mu4e-headers-mark-for-move
   "v" 'ps/mu4e-headers-move
   "x" 'ps/mu4e-mark-execute-all-no-confirm)
  (mu4e-view-mode-map
   "," 'mu4e-view-headers-next
   "." 'mu4e-view-headers-prev
   "d" 'mu4e-view-mark-for-delete
   "D" 'ps/mu4e-view-trash
   "e" 'ps/mu4e-view-archive
   "x" 'ps/mu4e-view-in-gmail
   "L" 'mu4e-view-save-attachments
   "m" 'mu4e-view-mark-for-something
   "o" 'ps/mu4e-view-org-capture
   "v" 'ps/mu4e-view-move
   "w" 'mu4e-copy-message-path
   "A-C-s-u" nil
   "A-C-s-p" nil))

(use-feature mu4e-org
  :disabled
  :after mu4e
  :demand t)

(use-package mu4e-alert
  :after mu4e
  :demand t
  :custom
  ;; Notify about unread emails in inbox only
  (mu4e-alert-interesting-mail-query "flag:unread AND maildir:/inbox")

  :config
  (mu4e-alert-enable-mode-line-display))

(use-package org-msg
  :after (org mu4e)
  :defer 3
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-recipient-names `((,ps/personal-gmail . "Pablo")))
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives '((new		. (text html))
                                  (reply-to-html	. (text html))
                                  (reply-to-text	. (text))))
  (org-msg-convert-citation t)
  (org-msg-signature ps/personal-signature)

  :config
  (org-msg-mode)

  (defun ps/org-msg-grammarly ()
    "Enable `grammarly-mode' in `org-msg-edit-mode'."
    (interactive)
    (if (eq major-mode 'org-mode)
        (org-msg-edit-mode)
      (org-mode)
      (require 'lsp-grammarly)
      (lsp)))

  (defun ps/org-msg-toggle-accounts ()
    "Toggle between personal and GPE email accounts."
    (interactive)
    (if (eq user-mail-address ps/personal-gmail)
        (setq user-mail-address ps/personal-gpe-email)
      (setq user-mail-address ps/personal-gmail))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "From: .*" nil t)
        (replace-match (format "From: %s <%s>" ps/personal-name user-mail-address)))
      (let ((new-signature
             (if (eq user-mail-address ps/personal-gmail)
                 ps/personal-signature
               ps/gpe-signature))
            (current-signature
             (if (eq user-mail-address ps/personal-gmail)
                 ps/gpe-signature
               ps/personal-signature)))
        (while (search-forward current-signature nil t)
          (replace-match new-signature)))))

  (defun ps/org-msg-kill-message ()
    "Save the current message to the kill ring."
    (interactive)
    (goto-char (org-msg-start))
    (re-search-forward "^:END:\n")
    (let ((beg (point)))
      (goto-char (org-msg-end))
      (search-backward "#+begin_signature" nil t)
      (kill-region beg (point))))

  (defun ps/org-msg-open-in-wordtune ()
    "Save the current message to the kill ring and open it in
Wordtune."
    (interactive)
    (ps/org-msg-kill-message)
    (browse-url "https://app.wordtune.com/v2/editor/"))

  (defun ps/org-msg-open-in-grammarly ()
    "Save the current message to the kill ring and open it in
Grammarly."
    (interactive)
    (ps/org-msg-kill-message)
    (browse-url "https://app.grammarly.com/ddocs/1929393566"))

  :general
  (org-msg-edit-mode-map
   "s-a" 'org-msg-attach
   "s-b" 'org-msg-goto-body
   "s-g" 'ps/org-msg-open-in-grammarly
   "s-x" 'ps/org-msg-kill-message
   "s-w" 'ps/org-msg-open-in-wordtune)
  (org-mode-map
   "A-s-g" 'ps/org-msg-grammarly))

(use-package htmlize
  :disabled
  :custom
  (htmlize-ignore-face-size nil))

(use-package telega
  ;; :demand t
  :defer 5
  :custom
  (telega-server-libs-prefix "/opt/homebrew")
  (setq telega-chat-input-markups '("org"))
  (telega-use-images t)
  (telega-emoji-font-family 'noto-emoji)
  (telega-emoji-use-images nil)
  (telega-filters-custom '(("Main" . main)
                           ("Important" or mention
                            (and unread unmuted))
                           ("Archive" . archive)
                           ("Online" and
                            (not saved-messages) (user is-online))
                           ("Groups" type basicgroup supergroup)
                           ("Channels" type channel)))
  (telega-completing-read-function 'completing-read)

  :config
  (defun ps/telega-switch-to ()
    "Switch to the most recent telega buffer, if one exists, else
 start telega. Always display telega buffers in the rightmost
 window. Repeated invocations of the command will move point from
 the middle of the chatbuf to the end of it, then to the root
 buffer, then to the beginning of it."
    (interactive)
    (let* ((telega-buffer-root "*Telega Root*")
           (telega-buffer
            (catch 'tag
              (dolist (buffer (buffer-list))
                (when (with-current-buffer buffer
                        (member major-mode '(telega-root-mode telega-chat-mode)))
                  (throw 'tag (buffer-name buffer))))))
           (current-buffer (current-buffer))
           (telega-buffer-is-current (string= (buffer-name current-buffer) telega-buffer)))
      (if telega-buffer
          (if telega-buffer-is-current
              (if (string> telega-buffer telega-buffer-root)
                  (if (ps/bollp)
                      (switch-to-buffer telega-buffer-root)
                    (end-of-buffer))
                (beginning-of-buffer)
                (forward-line 3))
            (switch-to-buffer telega-buffer))
        (telega))
      (let ((telega-buffer (current-buffer)))
        (ps/window-split-if-unsplit)
        (if (> (frame-width) ps/frame-width-threshold)
            (winum-select-window-3)
          (winum-select-window-2))
        (let ((rightmost-window (selected-window))
              (other-window (previous-window)))
          ;; When the command is invoked from a Telega buffer which
          ;; isn't on the rightmost window, we display on that window
          ;; the most recent non-telega buffer.
          (when telega-buffer-is-current
            (setq current-buffer
                  (catch 'tag
                    (dolist (buffer (buffer-list))
                      (unless (with-current-buffer buffer
                                (member major-mode '(telega-root-mode telega-chat-mode)))
                        (throw 'tag buffer))))))
          (set-window-buffer (selected-window) telega-buffer)
          (set-window-buffer other-window current-buffer)))))

  (defun ps/telega-chat-org-capture ()
    "Capture chat message at point with `org-capture'."
    (interactive)
    (org-store-link nil)
    (org-capture nil "n"))

  (defun ps/telega-chat-org-capture-leo ()
    "Capture chat message at point with `org-capture' and turn it
into a task for Leo."
    (interactive)
    (telega-msg-copy-text (telega-msg-at (point)))
    (org-capture nil "lt"))

  (defun ps/telega-move-downloaded-file (file)
    "Move downloaded file(s) to `ps/dir-downloads' directory."
    (let* ((old-path (plist-get (plist-get file :local) :path))
           (file-name (file-name-nondirectory old-path))
           (new-path (concat ps/dir-downloads "/" file-name)))
      (rename-file old-path new-path)))


  (defun ps/telega-docs-change-notify (&optional change-begins change-ends)
    "TODO: write docstring"
    (interactive (list (region-beginning) (region-end)))
    (unless (telega-server-live-p)
      (user-error "Please launch Telega before running this command."))
    (if (equal (buffer-file-name) ps/file-tlon-docs)
        (progn
          (unless (region-active-p)
            (user-error "Please select the region containing the changes you introduced."))
          (let ((docs-section (org-get-heading)))
            (telega-chat--pop-to-buffer (telega-chat-get "-661475865"))
            (insert (format "FYI: I've made some changes to `docs.org` in section '%s' (%s–%s). Run `ps/telega-docs-change-open` (`.`) with point on this message to see the changes." docs-section change-begins change-ends))))
      (user-error "You aren't visiting `docs.org'!")))

  (defun ps/telega-docs-change-open (msg)
    "TODO: write docstring"
    (interactive (list (telega-msg-for-interactive)))
    (let* ((content (plist-get msg :content))
           (msg-text (or (telega-tl-str content :text)
                         (telega-tl-str content :caption)
                         ;; See FR https://t.me/emacs_telega/34839
                         (and (telega-msg-match-p msg '(type VoiceNote))
                              (telega-tl-str (plist-get content :voice_note)
                                             :recognized_text)))))
      (with-temp-buffer
        (insert msg-text)
        (goto-char (point-min))
        (re-search-forward "(\\([[:digit:]]*\\)–\\([[:digit:]]*\\))")
        (let ((change-begins (string-to-number (match-string 1)))
              (change-ends (string-to-number (match-string 2))))
          (find-file ps/file-tlon-docs)
          (org-show-all)
          (org-hide-drawer-all)
          (org-highlight change-begins change-ends)
          (goto-char change-begins))
        (message "The highlighting is not persistent and will disappear when you close the buffer. You can also remove it by running `ps/org-unhighlight' or by reverting the buffer."))))

  (defun ps/telega-filters-push-archive ()
    "Set active filters list to `archive'."
    (interactive)
    (telega-filters-push '(archive)))

  (defun ps/telega-filters-push-main ()
    "Set active filters list to `main'."
    (interactive)
    (telega-filters-push '(main)))

  (defun ps/telega-chat-mode ()
    (require 'company)
    (add-hook 'completion-at-point-functions
              #'telega-chatbuf-complete-at-point nil 'local))

  (telega-mode-line-mode 1)

  :hook
  (telega-chat-mode-hook . ps/telega-chat-mode)
  (telega-chat-mode-hook . (lambda () (setq default-directory ps/dir-downloads)))
  ;; (telega-chat-mode-hook . telega-autoplay-mode) ; causes massive slowdown
  ;; (telega-chat-mode-hook . (lambda () (setq line-spacing nil)))

  :general
  ("C-f" 'ps/telega-switch-to)
  ((telega-msg-button-map telega-root-mode-map)
   "k" 'telega-button-backward
   "l" 'telega-button-forward
   "o" 'ps/telega-chat-org-capture)
  (telega-chat-mode-map
   "M-p" nil
   "<return>" 'newline
   "A-C-s-r" 'telega-chatbuf-beginning-of-thing
   "C-<return>" 'telega-chatbuf-input-send
   "s-<return>" (lambda! (telega-chatbuf-input-send "markdown1"))
   "s-," 'telega-chatbuf-goto-pinned-message
   "s-a" 'telega-chatbuf-attach
   "s-c" 'telega-mnz-chatbuf-attach-code
   "s-d" 'telega-chatbuf-goto-date
   "s-f" 'telega-chatbuf-filter
   "s-m" 'telega-chatbuf-attach-media
   "s-r" 'telega-msg-add-reaction
   "s-s" 'telega-chatbuf-filter-search
   "s-t" 'telega-sticker-choose-favorite-or-recent
   "s-v" 'ps/markdown-paste-from-org
   "A-s-v" 'telega-chatbuf-attach-clipboard
   "s-z" 'telega-mnz-chatbuf-attach-code
   "A-s-e" 'telega-chatbuf-edit-prev
   "A-s-s" 'ps/telega-chatbuf-attach-most-recent-screenshot
   "A-s-t" 'ps/telega-chatbuf-attach-most-recent-file
   "A-s-v" 'telega-chatbuf-attach-clipboard
   "" 'telega-chatbuf-attach-voice-note)
  (telega-msg-button-map
   "<return>" 'push-button
   "." 'ps/telega-docs-change-open
   "," 'telega-chatbuf-goto-pinned-message
   "a" (lambda! (end-of-buffer) (ps/kill-this-buffer))
   "F" 'telega-msg-forward-marked-or-at-point
   "C" 'telega-msg-copy-link
   "L" 'ps/telega-chat-org-capture-leo
   "s" 'telega-chatbuf-filter-search
   "w" 'telega-browse-url
   "W" 'telega-chatbuf-filter-cancel)
  (telega-chat-button-map
   "a" nil
   "o" nil)
  (telega-root-mode-map
   "SPC" 'telega-root-next-unread
   "." 'telega-chat-with
   "a" 'telega-chat-toggle-archive
   "m" 'telega-chat-toggle-muted)
  (telega-root-view-map
   "a" 'ps/telega-filters-push-archive
   "m" 'ps/telega-filters-push-main)
  (telega-webpage-mode-map
   "x" 'telega-webpage-browse-url)
  (dired-mode-map
   "A-s-a" 'ps/telega-dired-attach-send))

(use-feature telega-mnz
  :after telega
  :demand t
  :custom
  (telega-mnz-use-language-detection nil)

  :config
  (add-hook 'telega-load-hook 'global-telega-mnz-mode))

  ;; :hook
  ;; (telega-load-hook . global-telega-mnz-mode))

(use-feature telega-dired-dwim
  :after telega
  :demand t
  :config
  ;; copied from github.com/zevlg/telega.el/issues/231
  (defun ps/telega-dired-attach-func (file)
    "Identify msg type for FILE."
    (let ((file-ext (file-name-extension file)))
      (cond ((member file-ext '("mp3" "flac"))
             #'telega-chatbuf-attach-audio)
            ((member file-ext '("mp4" "mkv"))
             #'telega-chatbuf-attach-video)
            ((image-type-from-file-name file)
             #'telega-chatbuf-attach-photo)
            (t
             #'telega-chatbuf-attach-file))))

  (defun ps/telega-dired-attach-send ()
    "Send the marked files."
    (interactive)
    (let ((dired-files (dired-get-marked-files)))
      (unless dired-files
        (user-error "No marked files"))
      (with-current-buffer (telega-chat--pop-to-buffer
                            (telega-completing-read-chat
                             (format "Send %d files to: " (length dired-files))))
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (dolist (file dired-files)
            (funcall (ps/telega-dired-attach-func file) file))))))

  (defun ps/telega-chatbuf-attach-most-recent-screenshot ()
    "Attach most recently captured screenshot as photo."
    (interactive)
    (if-let ((screenshot (ps/newest-file default-directory "\\.png$")))
        (telega-chatbuf-attach-photo screenshot)
      (user-error (format "No screenshots found in %s" default-directory))))

  (defun ps/telega-chatbuf-attach-most-recent-file ()
    "Attach most recently saved file in `downloads' folder."
    (interactive)
    (if-let ((file (ps/newest-file ps/dir-downloads)))
          (telega-chatbuf-attach-file file)
        (user-error (format "No files found in %s" ps/dir-downloads)))))

(use-feature ol-telega
  :after telega
  :demand t)

(use-package slack
  :disabled
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after auth-source-pass
  ;; :demand t
  :defer 60
  :commands slack-select-rooms
  :custom
  (slack-file-dir ps/dir-downloads)
  (slack-prefer-current-team t)

  :config
  (slack-register-team
   :default t
   :name "EA Forum Moderators"
   :token (auth-source-pick-first-password
           :host "eaforummoderators"
           :user ps/personal-gmail))

  (slack-register-team
   :name "CEA Core"
   :token (auth-source-pick-first-password
           :host "cea-core"
           :user ps/personal-gmail))

  (slack-register-team
   :name "Altruismo Eficaz y Racionalidad"
   :token (auth-source-pick-first-password
           :host "altruismo-eficaz"
           :user ps/personal-gmail)
   :cookie (auth-source-pick-first-password
            :host "altruismo-eficaz^cookie"
            :user (concat ps/personal-gmail "^cookie")))

  (slack-register-team
   :name "Samotsvety Forecasting"
   :token (auth-source-pick-first-password
           :host "samotsvety"
           :user ps/personal-gmail)
   :cookie (auth-source-pick-first-password
            :host "samotsvety^cookie"
            :user (concat ps/personal-gmail "^cookie")))

  (slack-register-team
   :name "Future Fund Regrantors"
   :token (auth-source-pick-first-password
           :host "futurefundregrantors"
           :user ps/personal-gmail)
   :cookie (auth-source-pick-first-password
            :host "futurefundregrantors^cookie"
            :user (concat ps/personal-gmail "^cookie")))

  (slack-register-team
   :name "EA Bahamas"
   :token (auth-source-pick-first-password
           :host "eabahamas"
           :user ps/personal-gmail)
   :cookie (auth-source-pick-first-password
            :host "eabahamas^cookie"
            :user (concat ps/personal-gmail "^cookie")))

  (slack-register-team
   :name "EAOxfordOffice"
   :token (auth-source-pick-first-password
           :host "eaoxfordoffice"
           :user ps/personal-email)
   :cookie (auth-source-pick-first-password
            :host "eaoxfordoffice^cookie"
            :user (concat ps/personal-email "^cookie")))

  (defun ps/slack-chat-org-capture ()
    "Capture Slack message at point with `org-capture'."
    (interactive)
    (org-capture nil "s"))

  (slack-start)

  :hook
  (slack-buffer-mode-hook . (lambda () (setq line-spacing nil)))

  :general
  ("A-s" 'slack-channel-select)
  ((slack-mode-map slack-buffer-mode-map)
   "s-a" 'slack-all-threads
   "s-c" 'slack-channel-select
   "s-g" 'slack-group-select
   "s-m" 'slack-im-select
   "H-s-t" 'slack-change-current-team
   "s-u" 'slack-select-rooms
   "H-s-u" 'slack-select-unread-rooms) ; `slack-all-unreads' not working
  ((slack-thread-message-buffer-mode-map slack-message-buffer-mode-map)
   "d" 'slack-thread-show-or-create
   "e" 'slack-message-edit
   "k" 'slack-buffer-goto-prev-message
   "l" 'slack-buffer-goto-next-message
   "o" 'ps/slack-chat-org-capture
   "r" 'slack-message-add-reaction
   "R" 'slack-message-remove-reaction
   "z" 'slack-message-write-another-buffer)
  (slack-message-compose-buffer-mode-map
   "s-c" 'slack-message-send-from-buffer
   "s-f" 'slack-message-select-file
   "s-m" 'slack-message-embed-mention))

(use-package ol-emacs-slack
  :disabled
  :straight (ol-emacs-slack
             :host github
             :repo "ag91/ol-emacs-slack")
  :after slack
  :demand t)

(use-feature erc
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after auth-source-pass
  :custom
  (erc-server "irc.libera.chat")
  (erc-user-full-name user-full-name)
  (erc-nick (auth-source-pass-get "user" "auth-sources/erc/libera"))
  (erc-password (auth-source-pass-get 'secret "auth-sources/erc/libera"))
  (erc-prompt-for-nickserv-password nil)
  ;; erc-track-shorten-start 8 ; characters to display in modeline
  (erc-autojoin-channels-alist '(("irc.libera.chat")))
  (erc-kill-buffer-on-part nil)
  (erc-auto-query t)

  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)

  (defun ps/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title))))

(use-package circe)

(use-feature browse-url
  :custom
  (browse-url-browser-function 'browse-url-default-macosx-browser)
  (browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
  (browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (browse-url-handlers
   '(("gnu\\.org"  . eww-browse-url)
     ("protesilaos\\.com" . eww-browse-url)
     ("orgmode\\.org" . eww-browse-url)
     ;; My VPN settings apply to Firefox only (via split tunneling),
     ;; so the websites requiring VPN access are listed below, and
     ;; will be opened with this browser.
     ;; [2022-10-30 Sun] Disabling until I configure split tunneling
     ;; ("libgen" . browse-url-firefox)
     ;; ("hathitrust\\.org" . browse-url-firefox)
     ;; Use the default MacOS browser for all other websites
     ("." . browse-url-default-macosx-browser)))

  :general
  (mhtml-mode-map
   "s-x" 'browse-url-of-buffer)
  (dired-mode-map
   "e" 'browse-url-of-dired-file))

(use-feature shr
  :defer 15
  :custom
  (shr-bullet "• ")
  (shr-use-colors nil)
  (shr-use-fonts t)
  (shr-image-animate nil)
  (shr-width nil)
  (shr-max-width 120)
  (shr-discard-aria-hidden t)
  (shr-cookie-policy t)

  :general
  (mhtml-mode-map
   "s-w" 'shr-render-buffer))

(use-package shr-tag-pre-highlight
  :after shr
  :defer 30
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package shr-heading
  :straight (shr-heading
             :host github
             :repo "oantolin/emacs-config"
             :files ("my-lisp/shr-heading.el"))
  :demand t
  :hook
  (eww-mode-hook . shr-heading-setup-imenu)
  :general
  (eww-mode-map
   "A-C-s-r" 'shr-heading-previous
   "A-C-s-f" 'shr-heading-next))

(use-feature eww
  ;; :defer 30
  :custom
  (eww-search-prefix "https://www.google.com/search?q=")
  (eww-restore-desktop t)
  (eww-desktop-remove-duplicates t)
  (eww-header-line-format nil)
  (eww-download-directory ps/dir-downloads)
  (eww-suggest-uris
   '(eww-links-at-point
     thing-at-point-url-at-point))
  (eww-history-limit 1000)
  (eww-browse-url-new-window-is-tab nil)

  :config
  ;; inspired by Prot
  (defun ps/rename-buffer ()
    "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
    (let ((name (if (eq "" (plist-get eww-data :title))
                    (plist-get eww-data :url)
                  (plist-get eww-data :title))))
      (rename-buffer (format "%s" (truncate-string-to-width name 14)) t)))

  (defun ps/org-open-at-point-with-eww ()
    "Open org link at point with eww."
    (interactive)
    (let ((browse-url-handlers nil)
          (browse-url-browser-function #'eww-browse-url))
      (org-open-at-point)))

  (defun ps/url-to-pdf (url)
    ;; TODO: Default to eww buffer or kill-ring if content is URL.
    "Generate PDF of URL."
    (interactive "MUrl: ")
    (let* ((title (ps/org-web-tools--org-title-for-url url))
           (slug (org-hugo-slug title))
           (file-name (file-name-with-extension slug ".pdf"))
           (output-file (file-name-concat ps/dir-downloads file-name))
           (shell-command-buffer-name-async title))
      (async-shell-command
       (format
        "'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' --headless --print-to-pdf-no-header --print-to-pdf='%s' %s"
        output-file
        url))))

  (defvar ps/eww-readable-exceptions
    '("protesilaos\\.com"))

  (defun ps/eww-readable-autoview ()
    "Automatically view the main 'readable' parts of the current web
page for all websites except those in `ps/eww-readable-exceptions'."
    (let ((current-url (eww-current-url)))
      (dolist (url ps/eww-readable-exceptions)
        (unless (string-match-p (concat "^\\(?:https?://\\)?\\(?:www\\.\\)?" url) current-url)
          (eww-readable)))))

  ;; The following four commands copied from
  ;; github.com/gopar/.emacs.d#eww
  (defun ps/eww-edit-current-url (&optional arg)
    (interactive)
    (let* ((url (eww-copy-page-url))
           (uris (eww-suggested-uris)))
      (setq url (read-string "Edit URL or new search: " url 'eww-promt-history uris))
      (setq url (eww--dwim-expand-url url))
      (eww url (if arg 4 nil))))

  (defun ps/open-eww-with-recent-kill-ring (&optional arg)
    "Open current EWW with most recent item in kill ring.
If prefix arg is passed, then open in new EWW buffer."
    (interactive "P")
    (if arg
        (with-current-buffer
            (if (eq major-mode 'eww-mode) (clone-buffer)
              (generate-new-buffer "*eww*"))
          (eww-mode)
          (eww (current-kill 0)))
      (eww (current-kill 0))))

  (defun ps/eww-go-up-url-hierarchy ()
    "Go up the URL hierarchy."
    (interactive)
    (let* ((url (url-generic-parse-url (eww-current-url)))
           (filepath (url-filename url))
           (paths (s-split "/" filepath))
           (new-path (s-join "/" (butlast paths 1)))
           (new-url nil))
      (setq new-url (url-parse-make-urlobj
                     (url-type url)
                     (url-user url)
                     (url-password url)
                     (url-host url)
                     (url-port url)
                     new-path
                     (url-target url)
                     nil
                     (url-fullness url)))
      (eww-browse-url (url-recreate-url new-url))))

  (defun ps/eww-go-to-root-url-hierarchy ()
    "Go to root of current URL hierarchy"
    (interactive)
    (let* ((url (url-generic-parse-url (eww-current-url)))
           (new-url nil))
      (setq new-url (url-parse-make-urlobj
                     (url-type url)
                     (url-user url)
                     (url-password url)
                     (url-host url)
                     (url-port url)
                     ""
                     (url-target url)
                     nil
                     (url-fullness url)))
      (eww-browse-url (url-recreate-url new-url))))

  :hook
  (eww-after-render-hook . ps/rename-buffer)
  (eww-after-render-hook . ps/eww-readable-autoview)

  :general
  ("A-w" 'eww)
  (eww-mode-map
   "[" 'eww-previous-url
   "]" 'eww-next-url
   "j" 'eww-back-url
   ";" 'eww-forward-url
   "r" 'eww-readable
   "g" nil
   "g e" 'ps/eww-edit-current-url
   "g u" 'ps/eww-go-up-url-hierarchy
   "g U" 'ps/eww-go-to-root-url-hierarchy
   "p" 'ps/open-eww-with-recent-kill-ring
   "o" 'eww-toggle-fonts
   "p" 'ps/eww-pdf-of-current-webpage
   ":" (lambda! (eww-follow-link '(4)))
   "x" 'eww-browse-with-external-browser))

(use-package w3m
  :custom
  (mm-text-html-renderer 'w3m)
  :general
  ((w3m-mode-map mu4e-view-mode-map)
   "s-<return>" 'w3m-view-url-with-browse-url)) ; open externally

(use-package elfeed
  :custom
  (elfeed-db-directory (file-name-concat ps/dir-emacs "var/elfeed"))

  :config
  (setq-default elfeed-search-filter "+unread -wiki")

  ;; Borrowed from Prot
  (defun ps/elfeed-show-visit-in-eww (&optional link)
    "Browse current entry's link or optional LINK in `eww'. Only show
the readable part once the website loads. This can fail on
poorly-designed websites."
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (or link (elfeed-entry-link entry))))
      (eww link)
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))

  (defun ps/elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun ps/elfeed-full-update ()
    "*Really* update feeds!"
    (interactive)
    (let ((elfeed-search-buffer "*elfeed-search*"))
      (when (and (get-buffer elfeed-search-buffer)
                 (not (equal (buffer-name) elfeed-search-buffer)))
        (kill-buffer elfeed-search-buffer)))
    (elfeed-org)
    (elfeed-unjam)
    (elfeed-update))

  (defun ps/elfeed-kill-link-url-of-entry ()
    "Add link of current entry to kill ring."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Copied link: %s" link)
        (kill-new link))))

  (defun ps/elfeed-filter-tags (tags)
    (elfeed-search-set-filter tags)
    (if (string= tags "")
        (message "Showing everything")
      (message (concat "Showing " tags))))

  (defvar ps/elfeed-toggle-read-entries t)
  (defun ps/elfeed-toggle-read-entries ()
    "Toggle between showing and hiding read entries."
    (interactive)
    (if ps/elfeed-toggle-read-entries
        (ps/elfeed-filter-tags "")
      (ps/elfeed-filter-tags "+unread"))
    (setq ps/elfeed-toggle-read-entries (not ps/elfeed-toggle-read-entries)))

  (defvar ps/elfeed-toggle-wiki-entries t)
  (defun ps/elfeed-toggle-wiki-entries ()
    "Toggle between showing all, or only 'wiki', unread entries."
    (interactive)
    (if ps/elfeed-toggle-wiki-entries
        (ps/elfeed-filter-tags "+unread +wiki")
      (ps/elfeed-filter-tags "+unread -wiki"))
    (setq ps/elfeed-toggle-wiki-entries (not ps/elfeed-toggle-wiki-entries)))

  (defun ps/elfeed-toggle-fixed-pitch ()
    "Toggle between fixed pitch and variable pitch."
    (interactive)
    (if shr-use-fonts
        (setq shr-use-fonts nil)
      (setq shr-use-fonts t))
    (elfeed-show-refresh))

  (defun ps/elfeed-toggle-session ()
    "Start or end an `elfeed' session."
    (interactive)
    (if (or
         (equal major-mode 'elfeed-search-mode)
         (equal major-mode 'elfeed-show-mode))
        (progn
          ;; (global-writeroom-mode 0)
          (kill-matching-buffers "^\*elfeed\-*\*" nil t))
      (elfeed)
      (when (< elfeed-search-last-update
               (time-to-seconds (time-subtract (current-time) (seconds-to-time (* 60 60 2)))))
        (elfeed-update))
      ;; (global-writeroom-mode 1)
      ))

  ;; Not working
  ;; xenodium.com/open-emacs-elfeed-links-in-background/
  (defun ps/elfeed-search-browse-background-url ()
    "Open current `elfeed' entry (or region entries) in browser without losing focus."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (cl-assert (memq system-type '(darwin)) t "open command is macOS only")
              (start-process (concat "open " (elfeed-entry-link entry))
                             nil "open" "--background" (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))

  ;; github.com/skeeto/elfeed/issues/190#issuecomment-384346895
  (setq elfeed-show-mode-hook
        (lambda ()
          (set-face-attribute 'variable-pitch (selected-frame)
          :font (font-spec :family ps/face-variable-pitch :size 14))
          (setq fill-column (current-fill-column))
          ;; (setq-local shr-width (current-fill-column))
          (setq elfeed-show-entry-switch #'ps/show-elfeed)))

  (defun ps/show-elfeed (buffer)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (fill-individual-paragraphs (point) (point-max))
      (setq buffer-read-only t))
    (switch-to-buffer buffer)
    (elfeed-show-refresh))

  ;; (run-with-idle-timer (* 60 10) t 'elfeed-update)

  :hook
  (elfeed-show-mode-hook . shr-heading-setup-imenu)

  :general
  ;; ("A-f" (lambda! (elfeed) (ps/elfeed-full-update)))
  ("A-f" 'ps/elfeed-toggle-session)
  (eww-mode-map
   "c" 'elfeed-kill-link-url-at-point)
  (elfeed-show-mode-map
   "q" nil)
  (elfeed-search-mode-map
   "A" 'ps/elfeed-mark-all-as-read
   "d" 'elfeed-update
   "e" 'ps/elfeed-toggle-read-entries
   "f" 'ps/avy-elfeed-search-show-entry
   "q" 'ps/elfeed-toggle-session
   "U" 'ps/elfeed-full-update
   "j" 'elfeed-unjam
   "o" 'elfeed-org
   "w" 'ps/elfeed-toggle-wiki-entries)
  (elfeed-show-mode-map
   "S-<return>" 'eww-follow-link
   "<return>" (lambda! (eww-follow-link '(4)))
   "<tab>" (lambda! (elfeed-show-next-link) (recenter))
   "b" nil
   "q" 'elfeed
   "i" 'ps/elfeed-toggle-fixed-pitch
   "j" 'elfeed-show-next
   ";W" 'elfeed-show-prev
   "v" 'ps/elfeed-show-visit-in-eww
   "w" 'ps/elfeed-kill-link-url-of-entry
   "x" 'elfeed-show-visit))

(use-package elfeed-org
  :after elfeed
  :demand t
  :custom
  (rmh-elfeed-org-files (list ps/file-feeds-pablo ps/file-tlon-feeds))

  :config
  (elfeed-org))

(use-package google-this
  :config
  ;; Tweak original function to offer contents of kill ring if there
  ;; is no symbol or word at point.
  (defun ps/google-this-pick-term (prefix)
    "Decide what \"this\" is and return it.
PREFIX determines quoting."
    (let* ((term (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (or (thing-at-point 'symbol)
                       (thing-at-point 'word)
                       (current-kill 0))))
         (term (read-string (concat "Googling [" term "]: ") nil nil term)))
      term))

  (advice-add 'google-this-pick-term :override #'ps/google-this-pick-term)

  :general
  ("H-g" 'google-this-search)
  :bind-keymap
  ("H-G" . google-this-mode-submap))

(use-package engine-mode
  :init
  (engine-mode)
  :config
  (defengine Amazon
    "https://www.amazon.com/s?k=%s"
    :keybinding "a")
  (defengine EABlogs
    "https://cse.google.com/cse?cx=013594344773078830993:k3igzr2se6y&q=%s"
    :keybinding "b")
  (defengine MercadoLibre
    "https://listado.mercadolibre.com.ar/%s"
    :keybinding "c")
  (defengine EAForum
    "https://forum.effectivealtruism.org/search?terms=%s"
    :keybinding "f")
  (defengine GitHub
    "https://github.com/search?q=%s&type=Code"
    :keybinding "g")
  (defengine IMDb
    "http://www.imdb.com/find?s=all;q=%s"
    :keybinding "i")
  (defengine LibGen
    "http://libgen.li/index.php?req=%s"
    :keybinding "l")
  (defengine Metaforecast
    "https://metaforecast.org/?query=%s"
    :keybinding "m")
  (defengine GoogleMaps
    "https://www.google.com/maps?q=%s"
    :keybinding "p")
  (defengine Reddit
    "https://www.reddit.com/search?q=%s"
    :keybinding "r")
  (defengine GoogleScholar
    "https://scholar.google.com/scholar?hl=en&q=%s"
    :keybinding "s")
  (defengine GoogleTranslate
    "https://translate.google.com/#auto/en/%s"
    :keybinding "t")
  (defengine SciHub
    "https://sci-hub.se/%s"
    :keybinding "u")
  (defengine Wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine YouTube
    "http://www.youtube.com/results?search_query=%s"
    :keybinding "y")
  (engine/set-keymap-prefix (kbd "A-H-g")))

(use-package eaf
  :disabled
  :straight (eaf
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app" "*.json")
             :includes (eaf-browser) ; Straight won't try to search for these packages when we make further use-package invocations for them
             :pre-build (("python3" "install-eaf.py" "--install" "browser" "--ignore-sys-deps"))
             ))

(use-package eaf-browser
  :custom
  (eaf-browser-continue-where-left-off t)
  (browse-url-browser-function 'eaf-open-browser)
  (eaf-browser-enable-adblocker t)
  ;; the history file is stored in `.emacs.d/eaf/browser/history/log.txt'
  (eaf-browser-chrome-history-file "/Users/pablostafforini/Library/Application Support/Google/Chrome/Default/History")
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key ps/kill-this-buffer "q" eaf-browser-keybinding))

(use-package osm
  :straight (osm
             :host github
             :repo "minad/osm")
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

(defalias 'epa--decode-coding-string 'decode-coding-string) ; github.com/sfromm/emacs.d#twitter
(use-package twittering-mode
  ;; :if (equal (system-name) ps/computer-hostname-pablo)
  :defer t
  :custom
  (twittering-use-master-password t)
  (twittering-icon-mode t)
  (twittering-use-icon-storage t)
  (twittering-icon-storage-limit 10000)
  (twittering-timeline-header "Refresh buffer contents\n")
  (twittering-timeline-footer "")
  (twittering-status-format "%FACE[font-lock-function-name-face]{  @%s}  %FACE[italic]{%@}  %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}
%FOLD[   ]{%FILL{%t}%QT{
%FOLD[   ]{%FACE[font-lock-function-name-face]{@%s}\t%FACE[shadow]{%@}
%FOLD[ ]{%FILL{%t}}
}}}
%FACE[twitter-divider]{                                                                                                }
")

  :config
  (defun ps/twittering-account-select (arg)
    "docstring"
    (interactive
     (list
      (completing-read "Select account: " '("EA News" "Future Matters" "GPE"))))
    (cond ((string= arg "EA News") (ps/twittering-ea-news))
          ((string= arg "Future Matters") (ps/twittering-future-matters))
          ((string= arg "GPE") (ps/twittering-gpe))))

  (defface twitter-divider
    '((((background dark))  (:underline (:color "#141519")))
      (((background light)) (:underline (:color "#d3d3d3"))))
    "The vertical divider between tweets."
    :group 'twittering-mode)

  ;; github.com/hayamiz/twittering-mode/issues/83#issuecomment-343649348
  (defun ps/twittering-reload ()
    "Reload `twittering-mode'."
    (require 'twittering-mode)
    ;; Clear existing twit buffers
    (mapcar
     (lambda (buffer)
       (twittering-deactivate-buffer buffer)
       (kill-buffer buffer))
     (twittering-get-buffer-list))
    (twittering-unregister-killed-buffer)
    ;; Clear variables
    (setq twittering-private-info-file-loaded nil)
    (setq twittering-account-authorization nil)
    (setq twittering-oauth-access-token-alist nil)
    (setq twittering-buffer-info-list nil)
    (setq twittering-timeline-data-table (make-hash-table :test 'equal))
    (twit))

  (defun ps/twittering-ea-news ()
    "Start a `twittering-mode' session with the `ea_dot_news'
account."
    (interactive)
    (setq twittering-private-info-file (expand-file-name "~/.twittering-mode-ea-news.gpg"))
    (auth-source-pass-get 'secret "auth-sources/twitter/ea_dot_news")
    (ps/twittering-reload))

  (defun ps/twittering-future-matters ()
    "Start a `twittering-mode' session with the `futurematters_'
account."
    (setq twittering-private-info-file (expand-file-name "~/.twittering-mode-future-matters.gpg"))
    (auth-source-pass-get 'secret "auth-sources/twitter/futurematters_")
    (ps/twittering-reload))

  (defun ps/twittering-mode-get-uri ()
    "Get URI of tweet at point."
    (or (get-text-property (point) 'uri)
        (if (get-text-property (point) 'field)
            (let* ((id (get-text-property (point) 'id))
                   (status (twittering-find-status id)))
              (twittering-get-status-url-from-alist status))
          nil)))

  (defun ps/twittering-mode-open-externally ()
    "Open URI of tweet at point with external web browser."
    (interactive)
    (browse-url (ps/twittering-mode-get-uri)))

  (defun ps/twittering-mode-open-internally ()
    "Open URI of tweet at point with internal web browser."
    (interactive)
    (eaf-open (eaf-wrap-url (ps/twittering-mode-get-uri)) "browser"))

  (defun ps/twittering-mode-search-people-externally (&optional query)
    "Search for Twitter users in an external browser."
    (interactive)
    (let ((query (or query
                     (read-string "Search for person: "))))
      (browse-url (concat
                   "https://twitter.com/search?q="
                   query
                   "&src=typed_query&f=user"))))

  (defun ps/twittering-mode-org-capture-future-matters-research ()
    "Launch 'Future Matters: Research' `org-capture' template."
    (interactive)
    (twittering-push-uri-onto-kill-ring)
    (org-capture nil "lr"))

  (defun ps/twittering-mode-org-capture-future-matters-news ()
    "Launch 'Future Matters: News' `org-capture' template."
    (interactive)
    (twittering-push-uri-onto-kill-ring)
    (org-capture nil "ln"))

  ;; what follows copied from github.com/sfromm/emacs.d#twitter
  (defun ps/twittering-toggle-icons ()
    "Toggle use of icons in twittering mode."
    (interactive)
    (if (eq twittering-icon-mode t)
        (twittering-icon-mode nil)
      (twittering-icon-mode t)))

  (defun ps/twittering-add-image-format (format-table-func status-sym prefix-sym)
    "Adds the I format code to display images in the twittering-mode format table."
    (let ((format-table (funcall format-table-func status-sym prefix-sym)))
      (push `("I" .
              (let* ((entities (cdr (assq 'entity ,status-sym)))
                     text)
                (mapc (lambda (url-info)
                        (setq text (or (cdr (assq 'media-url url-info)) "")))
                      (cdr (assq 'media entities)))
                (if (string-equal "" text)
                    text
                  (let ((twittering-convert-fix-size 720))
                    (twittering-make-icon-string nil nil text))))) format-table)))

  (advice-add #'twittering-generate-format-table :around #'ps/twittering-add-image-format)

  ;; github.com/sfromm/emacs.d#twitter
  (defun ps/org-twittering-store-link ()
    "Store a link to a tweet."
    (when (and (twittering-buffer-p) (twittering-get-id-at))
      (let ((status (twittering-find-status (twittering-get-id-at))))
        (apply 'org-store-link-props
               :type "twittering"
               :link (concat "twittering:"
                             (or (cdr (assq 'retweeting-id status))
                                 (cdr (assq 'id status))))
               :description (format "@%s: %s"
                                    (cdr (assq 'user-screen-name status))
                                    (cdr (assq 'text status)))
               :url (twittering-get-status-url-from-alist status)
               :date
               (format-time-string (org-time-stamp-format)
                                   (cdr (assq 'created-at status)))
               :date-timestamp
               (format-time-string (org-time-stamp-format t)
                                   (cdr (assq 'created-at status)))
               (apply 'append
                      (mapcar
                       (lambda (sym)
                         (let ((name (symbol-name sym)))
                           `(,(intern (concat ":" name))
                             ,(or (cdr (assq sym status))
                                  (concat "[no " name "]")))))
                       '(text
                         id
                         user-id user-name user-screen-name user-description
                         user-url user-location
                         source source-url
                         retweeting-user-id retweeting-user-name
                         retweeting-user-screen-name
                         retweeting-user-description
                         retweeting-user-url
                         retweeting-user-location
                         retweeting-source retweeting-source-url)))))))

  (org-link-set-parameters "twittering"
                           :follow #'ps/org-twittering-open
                           :store #'ps/org-twittering-store-link)

  (defun ps/org-twittering-open (id-str)
    (twittering-visit-timeline (concat ":single/" id-str)))

  :general
  ("A-t" 'ps/twittering-account-select)
  (twittering-mode-map
   "s-m" 'twittering-replies-timeline
   "s-r" 'twittering-mentions-timeline
   "g" (lambda! (twittering-get-and-render-timeline)) ; refresh
   "c" 'twittering-push-tweet-onto-kill-ring
   "d" 'twittering-delete-status
   "A-C-s-f" 'twittering-goto-next-status
   "A-C-s-r" 'twittering-goto-previous-status
   "n" 'ps/twittering-mode-org-capture-future-matters-news
   "P" 'ps/twittering-mode-search-people-externally
   "q" 'twittering-kill-buffer
   "r" 'ps/twittering-mode-org-capture-future-matters-research
   "s" 'twittering-search
   "S" 'ps/twittering-mode-search-people-externally
   "t" 'twittering-update-status-interactive
   "u" 'twittering-push-uri-onto-kill-ring
   "x" 'ps/twittering-mode-open-externally
   "X" 'ps/twittering-mode-open-internally)
  (twittering-edit-mode-map
   "s-p" 'ps/twittering-mode-search-people-externally
   "s-c" 'twittering-edit-post-status))



(use-package md4rd
  :disabled
  :custom
  (md4rd-subs-active '(emacs))
  (md4rd--oauth-access-token
   (auth-source-pass-get 'secret "auth-sources/reddit"))
  (md4rd--oauth-refresh-token
   (auth-source-pass-get "refresh" "auth-sources/reddit"))

  :config
  (run-with-timer 0 3540 'md4rd-refresh-login)

  :hook
  (md4rd-mode-hook . md4rd-indent-all-the-lines))

(use-package org-download
  :after org
  :general
  ("H-s-v" 'org-download-clipboard))

(use-package org-web-tools
  :defer 30
  :commands ps/org-web-tools--youtube-dl
  :config
  ;; This function slightly tweaks `org-web-tools-insert-link-for-url'
  ;; so that it can be used with `org-capture'
  ;; blog.lazkani.io/posts/text-editors/bookmark-with-org-capture/
  (defun ps/org-web-tools-insert-link-for-clipboard-url ()
    "Extend org-web-tools-inster-link-for-url to take URL from
clipboard or kill-ring"
    (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

  ;; Version for EA Forum
  (defun ps/org-web-tools-insert-link-for-clipboard-url-ea-forum ()
    "Extend org-web-tools-inster-link-for-url to take URL from
clipboard or kill-ring"
    (replace-regexp-in-string
     " - EA Forum" ""
     (org-web-tools--org-link-for-url (org-web-tools--get-first-url))))

  (cl-defun ps/org-web-tools--org-title-for-url (&optional (url (org-web-tools--get-first-url)))
    "Return title of HTML page at URL. If URL is not given, look for
first URL in `kill-ring'. If page at URL has no title, return
URL."
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html)))
      (if title
          title
        (message "HTML page at URL has no title")
        url)))

  (defun ps/org-web-tools-insert-title-for-url-ea-forum (url)
    "Insert title of HTML page at URL.

If URL is not given, look for first URL in `kill-ring'."
    (interactive (list (org-web-tools--get-first-url)))
    (insert
     (replace-regexp-in-string
      " - EA Forum" ""
      (concat "Articles/" (ps/org-web-tools--org-title-for-url url)))))

  (defun ps/org-web-tools-insert-title-for-clipboard-url-ea-forum ()
    "Extend `org-web-tools-inster-link-for-url' to take URL from
clipboard or kill-ring"
    (replace-regexp-in-string
     " - EA Forum" ""
     (ps/org-web-tools--org-title-for-url (org-web-tools--get-first-url))))

  (defun ps/org-web-tools--youtube-dl (url)
    "Create org link to local copy of YouTube video downloaded from
URL. To be used in conjunction with associated `org-capture'
template."
    (let* ((html (org-web-tools--get-url url))
           (title (org-web-tools--html-title html))
           (file-path (file-name-concat ps/dir-downloads (file-name-with-extension (org-hugo-slug title) "webm"))))
      (if title
          (org-link-make-string (concat "file:" file-path) title)
        (user-error "HTML page at URL has no title"))))

  (defun ps/org-web-tools--get-first-url ()
    "Return URL in clipboard, or first URL in the `kill-ring', or nil
if none."
    (current-kill 0) ; added so that clipboard is pushed to kill ring; it won't otherwise
    (cl-loop for item in (append (list (gui-get-selection 'CLIPBOARD))
                                 kill-ring)
             when (and item (string-match (rx bol "http" (optional "s") "://") item))
             return item))

  (advice-add 'org-web-tools--get-first-url :override #'ps/org-web-tools--get-first-url)

  :general
  ("s-A-k" 'org-web-tools-insert-link-for-url))

(use-package request
  :defer 15)

(defvar ps/mullvad-servers
  '(("London" . "gb4-wireguard")
    ("Madrid" . "es-mad-wg-101")
    ("Malmö" . "se1-wireguard")
    ("Frankfurt" . "de5-wireguard")
    ("New York" . "us276-wireguard")
    ("São Paulo" . "br1-wireguard")
    ("Switzerland" . "ch5-wireguard"))
  "Specify the optimal Mullvad VPN servers for each city.")

(defvar ps/mullvad-websites
  '(("Library Genesis" . "Malmö")
    ("HathiTrust" . "New York")
    ("Criterion Channel" . "New York")
    ("Pirate Bay" . "Malmö")
    ("Wise" . "Madrid"))
  "For each website that requires a VPN connection, specify the
optimal server city.")

(defun ps/mullvad-connect-to-server (server)
  "Prompt the user to select from a list of servers and connection
durations, and connect to the server for that duration.

The list of servers is defined in `ps/mullvad-servers'."
  (interactive
   (list
    (completing-read
     "Select server: "
     ps/mullvad-servers)))
  (let* ((duration (call-interactively 'ps/mullvad-disconnect-after))
         (server (cdr (assoc server ps/mullvad-servers)))
         (connection (replace-regexp-in-string
                      "Setting location constraint to \\(.*\\)\n.*\n.*" "\\1"
                      (shell-command-to-string (format
                                                "mullvad relay set hostname %s; mullvad connect"
                                                server)))))
    (if duration
        (message (format "Connected to server %s. Disconnecting in %s minutes." connection duration))
      (message (format "Connected to server %s." connection)))))

(defun ps/mullvad-connect-to-website (website)
  "Prompt the user to select from a list of websites and connection
durations, set optimal VPN server for it, and connect to it for
that duration."
  (interactive
   (list
    (completing-read
     "Select website: "
     ps/mullvad-websites)))
  (let* ((duration (call-interactively 'ps/mullvad-disconnect-after))
         (city (cdr (assoc website ps/mullvad-websites)))
         (server (cdr (assoc city ps/mullvad-servers)))
         (connection (replace-regexp-in-string
                      "Setting location constraint to \\(.*\\)\n.*\n.*" "\\1"
                      (shell-command-to-string (format
                                                "mullvad relay set hostname %s; mullvad connect"
                                                server)))))
    (message "Connected to Mullvad server %s." connection
                     (when duration (message " Disconnecting in %s minute(s)." duration)))))

(defun ps/mullvad-disconnect ()
  "Disconnect from server."
  (interactive)
  (shell-command "mullvad disconnect")
  (message "Disconnected from Mullvad server."))

(defun ps/mullvad-disconnect-after (duration)
  "End connection to Mullvad VPN server after DURATION minutes."
  (interactive
   (list (completing-read
          "Select duration (minutes): "
          '("1" "5" "10" "30" "60" "120" "custom" "unlimited"))))
  (when (equal duration "custom")
    (setq duration (read-string "Enter duration (minutes): ")))
  (unless (equal duration "unlimited")
    ;; If a previous timer is running, cancel it.
    (cancel-function-timers #'ps/mullvad-disconnect)
    ;; Now run a new timer.
    (run-with-timer
     (* (string-to-number duration) 60)
     nil
     #'ps/mullvad-disconnect)
    duration))

(use-package mpv)

(use-package youtube-dl
  :custom
  (youtube-dl-program "yt-dlp")
  (youtube-dl-arguments '(
                          "--no-mtime"
                          "--restrict-filenames"
                          "--write-subs"
                          "--write-auto-subs"
                          ;; the below arguments aren't working
                          ;; "--sub-langs 'en-orig,en,fr,de,it,la,pt,es,sv,ca,ru'"
                          ;; "--skip=translated_subs"
                          ))
  (youtube-dl-directory ps/dir-downloads)

  :config
  ;; TODO:
  ;; - make it work even when point is at start of heading
  ;; - archive subitltes
  (defun ps/org-archive-watched-video ()
    "Archive TODO and associated file in video watchlist."
    (interactive)
    (when (ps/org-link-get-url-at-point)
      (let ((filename (s-replace "file:" "" (ps/org-link-get-url-at-point))))
        (rename-file
         filename
         (expand-file-name
          (file-name-nondirectory filename)
          (file-name-as-directory (expand-file-name
                                   "archive"
                                   (file-name-directory filename))))))
      (org-archive-to-archive-sibling))))

(use-package mentor
  :disabled
  :custom
  (mentor-rtorrent-download-directory ps/dir-downloads)
  (mentor-rtorrent-external-rpc "~/.rtorrent-rpc.socket")
  :general
  (mentor-mode-map
   "SPC" 'mentor-download-load-magnet-link-or-url
   "<return>" 'mentor-download-load-torrent))

(use-package espotify
  :disabled
  :if (equal (system-name) ps/computer-hostname-pablo)
  :after auth-source-pass
  :defer 20
  :custom
  (espotify-service-name "spotify")
  (espotify-use-system-bus-p nil)
  (espotify-client-id (auth-source-pass-get "id" "auth-sources/spotify"))
  (espotify-client-secret (auth-source-pass-get 'secret "auth-sources/spotify")))

(use-package smudge
  :disabled
  :after auth-source-pass
  ;; :defer 600
  :custom
  (smudge-oauth2-client-id (auth-source-pass-get "id" "auth-sources/spotify"))
  (smudge-oauth2-client-secret (auth-source-pass-get 'secret "auth-sources/spotify"))
  (smudge-player-status-format "[%a - %t ◷ %l]")
  (smudge-api-search-limit 100) ; values >100 result in a 400 error
  (smudge-status-location nil)
  :config
  ;; should renew credentials and store with `pass'
  (defun ps/smudge ()
    "Turn on global smudge remote mode and go to my playlists."
    (interactive)
    (global-smudge-remote-mode 1)
    (smudge-my-playlists))
  (defun ps/smudge-track-load-more ()
    "Move point to end of playlist buffer and load more tracks."
    (interactive)
    (end-of-buffer)
    (smudge-track-load-more))
  :general
  ("A-y" 'ps/smudge)
  (smudge-track-search-mode-map
   "RET" 'smudge-track-select
   "b" 'smudge-track-album-select
   "d" 'smudge-select-device
   "m" 'smudge-my-playlists
   "t" 'smudge-track-search
   "r" 'smudge-recently-played
   "s" 'smudge-track-search
   "t" 'smudge-track-artist-select
   "y" 'smudge-playlist-search
   "." 'ps/smudge-track-load-more)
  (smudge-playlist-search-mode-map
   "RET" 'smudge-track-select
   "b" 'smudge-track-album-select
   "d" 'smudge-select-device
   "m" 'smudge-my-playlists
   "r" 'smudge-recently-played
   "s" 'smudge-track-search
   "t" 'smudge-track-artist-select
   "y" 'smudge-playlist-search
   "." 'ps/smudge-track-load-more))

(use-package read-aloud
  :defer 20
  :config
  (setq read-aloud-engine "say")

  :general
  ("A-C-r" 'read-aloud-this)) ; remember that `A-C-d' starts/stops dictation

(use-package emms
  :disabled
  :demand t
  :custom
  (emms-source-file-default-directory ps/dir-music-tango)
  (emms-playlist-buffer-name "*Music*")
  (emms-info-asynchronously t)
  (emms-info-functions '(emms-info-libtag) "make sure libtag is the only thing delivering metadata")
  (emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find "~1 order of magnitude faster; requires GNU find: `brew install findutils'")
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  (require 'emms-info-metaflac)
  (add-to-list 'emms-info-functions 'emms-info-libtag)
  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1))

(use-feature plstore
  :init
  (setq-default plstore-cache-passphrase-for-symmetric-encryption t))

(use-feature epg-config
  :custom
  (epg-pinentry-mode 'loopback)) ; use minibuffer for password entry

(use-feature auth-source
  :defer 5
  :preface
  (eval-when-compile
    (defvar auth-sources))
  :custom
  (auth-source-debug t) ; uncomment for debugging
  (auth-sources '(macos-keychain-internet macos-keychain-generic)))

(use-package emacs-oauth2-auto
  :straight (emacs-oauth2-auto
             :host github
                   :repo "telotortium/emacs-oauth2-auto")

  )

(use-package pass
  ;; :demand t
  :defer 6
  :config
  (defun ps/pass-open-at-point ()
    "Open the URL associated with the entry at point and its password
to the clipboard."
    (interactive)
    (when (eq major-mode 'pass-mode)
      (pass-copy)
      (pass--with-closest-entry entry
        (let* ((inhibit-message t)
               (parsed-entries (password-store-parse-entry entry))
               (field password-store-url-field))
          (unless (assoc field parsed-entries)
            (user-error "Field `%s' not in  %s" field entry))
          (browse-url (password-store-get-field entry field))))))

  (defun ps/pass-quit ()
    "Kill the buffer quitting the window."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pass-view-mode)
          (quit-window t))
        (kill-buffer buf))))
  (advice-add 'pass-quit :override #'ps/pass-quit)

  (defun ps/pass-edit ()
    "Edit the entry at point, without confirmation."
    (interactive)
    (pass--with-closest-entry entry
      (password-store-edit entry)))

  (defun ps/pass-git-sync ()
    "Synchronize with remote repository."
    (interactive)
    ;; (shell-command "pass git config pull.rebase false")
    (shell-command "pass git pull; pass git push"))

  (defun ps/pass-magit-pull-tlon ()
    "Pull changes from remote `tlon' repo."
    (interactive)
    (let ((default-directory ps/dir-dropbox-tlon-pass))
      (magit-pull-from-pushremote nil)))

  (defun ps/pass-magit-push-tlon ()
    "Push local changes to remote `tlon' repo."
    (interactive)
    (let ((default-directory ps/dir-dropbox-tlon-pass))
      (call-interactively #'magit-push-current-to-pushremote)))

  (defun ps/pass-magit-merge-tlon ()
    "Merge local changes into remote `tlon' repo."
    (interactive)
    (let ((default-directory ps/dir-dropbox-tlon-pass))
      (magit-merge-plain (magit-branch-or-commit-at-point))))

  (defun ps/pass-fix-pinentry ()
    "Fix periodic pinentry problem."
    (interactive)
    (shell-command "pkill gpg-agent; gpg-agent --pinentry-program=/usr/bin/pinentry-gtk-2 --daemon"))

  :general
  ("A-H-o" 'pass)
  (pass-mode-map
   "c" 'pass-copy
   "d" 'pass-kill
   "<return>" 'ps/pass-edit
   "SPC" 'ps/pass-open-at-point
   "e" 'ps/pass-edit
   "y" 'ps/pass-git-sync)
  (pass-view-mode-map
   "s-c" 'pass-view-toggle-password
   "s-s" 'server-edit))

(use-feature auth-source-pass
  :demand t
  :after (auth-source pass)
  :custom
  (auth-source-debug t)
  (auth-source-do-cache nil)
  :config
  (auth-source-pass-enable))

(use-package alert
  ;; :defer 10
  :demand t
  :custom
  (alert-fade-time 2)

  :config
  (if (eq system-type 'darwin)
      (setq alert-default-style 'osx-notifier)
    (setq alert-default-style 'notifications))


  ;; This function has to be loaded manually, for some reason.
  (defun alert-osx-notifier-notify (info)
    (apply #'call-process "osascript" nil nil nil "-e"
           (list (format "display notification %S with title %S"
                         (alert-encode-string (plist-get info :message))
                         (alert-encode-string (plist-get info :title)))))
    (alert-message-notify info))

  (defun ps/alert-dismiss-osx-notification ()
    "docstring"
    (interactive)
    (let ((shell-command-buffer-name "*alert-dismiss-osx-notification*"))
      (shell-command "osascript ~/Library/Scripts/notifications.applescript")
      (kill-buffer shell-command-buffer-name)))

  (dolist (user (list "leo" "fede"))
    ;; is there a more elegant way of declaring these variables?
    (let ((hostname (symbol-value (intern (concat "ps/computer-hostname-" user))))
          (file (symbol-value (intern (concat "ps/file-tlon-tareas-" user)))))
      (when (equal (system-name) hostname)
        (file-notify-add-watch
         file
         '(change)
         (lambda (event)
           (alert "Changes to `tareas.org' have been detected."))))))

  :general
  ("H-\"" 'ps/alert-dismiss-osx-notification))

(use-feature calc
  :general
  ("A-c" 'calc
   "A-M-c" 'quick-calc)
  ((calc-mode-map calc-alg-map)
   "C-k" nil))

(use-package constants)

(use-feature tetris
  :general
  (tetris-mode-map
  "k" 'tetris-rotate-prev
  "l" 'tetris-move-down
  "j" 'tetris-move-left
  ";" 'tetris-move-right))

;; github.com/Gleek/emacs.d/blob/761dad7e97accd6d1537a201c5d447a6be1d155c/core/core-util.el#L31
(defun ps/random-alnum (&optional length)
  (let ((times (or length 5))
        (random ""))
    (setq-local random "")
    (dotimes (_ times)
      (setq random (concat random (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
             (i (% (abs (random)) (length alnum))))
                       (substring alnum i (1+ i))))))
    random))

(use-package unpackaged
  :straight (unpackaged :host github :repo "alphapapa/unpackaged.el")
  :commands (unpackaged/org-forward-to-entry-content))

(defun ps/eject-external-hard-drive ()
  "Eject external hard drive. Assumes only one external hard drive
is connected."
  (interactive)
  (let ((list-external
         (shell-command-to-string
          "diskutil list external")))
    (string-match
     "/dev/disk\\([[:digit:]]\\) (external, physical):"
     list-external)
    (shell-command
     (format
      "diskutil eject disk%s"
      (match-string 1 list-external)))))

(use-feature midnight
  :if (equal (system-name) ps/computer-hostname-pablo)
  :defer 600
  :custom
  (clean-buffer-list-kill-never-buffer-names
   '("*mu4e-headers*" " *mu4e-update*")) ; not sure if the space is part of the buffer name
  (clean-buffer-list-kill-never-regexps
   '("\\` \\*Minibuf-.*\\*\\'" "^untitled.*"))
  (clean-buffer-list-delay-general 2) ; kill buffers unused for more than three days

  :config
  (midnight-mode)
  (midnight-delay-set 'midnight-delay "4:30am")

  :hook
  (midnight-mode . ps/save-all-buffers)
  (midnight-hook . ps/ledger-update-commodities)
  (midnight-hook . ps/magit-stage-commit-and-push-all-repos)
  (midnight-hook . clean-buffer-list)
  (midnight-hook . org-roam-db-sync)
  (midnight-hook . ps/org-roam-update-id-locations)
  (midnight-hook . org-gcal-sync)
  (midnight-hook . ps/pass-git-sync)
  ;; (midnight-hook . straight-prune-build)
  )

(use-package keycast
  :config
  (with-eval-after-load 'keycast
    (define-minor-mode keycast-mode
      "Show current command and its key binding in the mode line."
      :global t
      (if keycast-mode
          (add-hook 'pre-command-hook 'keycast--update t)
        (remove-hook 'pre-command-hook 'keycast--update)))
    (add-to-list 'global-mode-string '("" mode-line-keycast))))

(use-package keyfreq
  :disabled
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-feature custom
  :custom
  (custom-safe-themes t)
  (custom-file (file-name-concat ps/dir-downloads "delete.me") "move unintended customizations to a garbage file"))

(defun ps/count-lines-with-expression (s exp)
  "Count the number of lines in the string S that contain the regular expression EXP."
  (let ((count 0))
    (mapc (lambda (line)
            (when (string-match-p exp line)
              (setq count (+ 1 count))))
          (split-string s "\n"))
    count))

(defun ps/productivity-of-the-day ()
  (seq-reduce
   (lambda (acc it)
     (let* ((folder (file-name-directory it))
            (file (file-name-nondirectory it))
            (base-cmd (concat "cd "
                              folder
                              "; git log --since=midnight -p "
                              file
                              "| grep TODO"))
            (changed (shell-command-to-string base-cmd))
            (added (count-lines-with-expression changed "^\\+"))
            (removed (count-lines-with-expression changed "^\\-")))
       (cons (+ (car acc) added)
             (- (cdr acc) removed))))
   org-agenda-files
   '(0 . 0)))

(defun ps/tlon-meeting-with-fede ()
  (interactive)
  (ps/tlon--meeting-with
   "CE0C7638-97F1-4509-8212-5B77F4A4AF29"
   "56CBB3F8-8E75-4298-99B3-899365EB75E0"
   "AAB63566-B9AD-4BA3-96E9-0F3F0A26E2B1"))

(defun ps/tlon-meeting-with-leo ()
  (interactive)
  (ps/tlon--meeting-with
   "76A01EAA-74BC-41FC-9050-E6BDC0D56029"
   "51610BEB-7583-4C84-8FC2-A3B28CA79FAB"
   "8B2F18B4-A309-4F29-A5E6-CD40E010970D"))

(defun ps/tlon--meeting-with (tareas-id meetings-id pending-id)
  ;; "[person] > Meetings > Pending for next meeting" org heading in `work-dashboard.org'
  (ps/org-id-goto pending-id)
  (let ((contents (ps/org-get-heading-contents)))
    (ps/org-clear-heading-contents)
    ;; "[person] > Meetings" org heading in `work-dashboard.org'
    (ps/org-id-goto meetings-id)
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (org-insert-heading)
    (ps/org-time-stamp-inactive-current-time)
    (unless (string= contents "")
      (insert (concat "\nTo discuss:\n" contents "\n"))))
  (forward-line)
  (ps/org-narrow-to-entry-and-children)
  (ps/window-split-if-unsplit)
  (ps/switch-to-last-window)
  ;; "Tareas" org heading in `tareas.org'
  (ps/org-id-goto tareas-id)
  (ps/org-narrow-to-entry-and-children)
  (ps/switch-to-last-window))

(general-define-key
 "C-H-M-S-c" 'ps/org-copy-heading-contents
 "C-H-M-S-g" 'orgmdb-fill-movie-properties
 "C-H-M-S-n" 'ps/create-new-wiki-entry
 ;; "C-H-M-S-i" 'ps/org-switch-to-id ; delete
 "C-H-M-S-w" 'ps/org-wiki-add-datestamp-to-heading-from-clipboard)
