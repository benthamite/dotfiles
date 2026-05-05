;;; codex-extras-test.el --- Tests for codex-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in codex-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-extras)

;;;; Theme sync

(ert-deftest codex-extras-test-sync-theme-updates-existing-tui-section ()
  "Persist theme changes to an existing Codex `[tui]' section."
  (let* ((dir (make-temp-file "codex-theme" t))
         (config (expand-file-name "config.toml" dir))
         (codex-hooks-config-path config))
    (unwind-protect
        (progn
          (with-temp-file config
            (insert "model = \"gpt-5.5\"\n\n[tui]\ntheme = \"light\"\n"))
          (should (codex-extras--sync-theme "dark"))
          (should (string-match-p
                   "^\\[tui\\]\ntheme = \"dark\""
                   (with-temp-buffer
                     (insert-file-contents config)
                     (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest codex-extras-test-sync-theme-adds-tui-section ()
  "Create a Codex `[tui]' section when the config has none."
  (let* ((dir (make-temp-file "codex-theme" t))
         (config (expand-file-name "config.toml" dir))
         (codex-hooks-config-path config))
    (unwind-protect
        (progn
          (with-temp-file config
            (insert "model = \"gpt-5.5\"\n"))
          (should (codex-extras--sync-theme "light"))
          (should (string-match-p
                   "\\[tui\\]\ntheme = \"light\""
                   (with-temp-buffer
                     (insert-file-contents config)
                     (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest codex-extras-test-sync-theme-skips-unchanged-config ()
  "Avoid rewriting Codex config when the theme already matches."
  (let* ((dir (make-temp-file "codex-theme" t))
         (config (expand-file-name "config.toml" dir))
         (codex-hooks-config-path config))
    (unwind-protect
        (progn
          (with-temp-file config
            (insert "[tui]\ntheme = \"dark\"\n"))
          (should-not (codex-extras--sync-theme "dark")))
      (delete-directory dir t))))

(ert-deftest codex-extras-test-sync-theme-to-config-allows-legacy-call ()
  "Accept the old no-argument theme config writer call."
  (let* ((dir (make-temp-file "codex-theme" t))
         (config (expand-file-name "config.toml" dir))
         (codex-hooks-config-path config))
    (unwind-protect
        (progn
          (with-temp-file config
            (insert "[tui]\ntheme = \"light\"\n"))
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (_frame param)
                       (when (eq param 'background-mode) 'dark))))
            (should (codex-extras--sync-theme-to-config)))
          (should (string-match-p
                   "^\\[tui\\]\ntheme = \"dark\""
                   (with-temp-buffer
                     (insert-file-contents config)
                     (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest codex-extras-test-cleanup-obsolete-theme-sync ()
  "Remove stale Codex theme hooks, timers, and functions."
  (let ((enable-theme-functions
         (cons 'codex-extras-sync-theme enable-theme-functions))
        (codex-start-hook
         (cons 'codex-extras-sync-theme codex-start-hook))
        (timer (run-at-time 9999 nil 'codex-extras--do-sync-theme)))
    (unwind-protect
        (progn
          (fset 'codex-extras-sync-theme #'ignore)
          (fset 'codex-extras--do-sync-theme #'ignore)
          (set 'codex-extras--sync-theme-timer timer)
          (codex-extras--cleanup-obsolete-theme-sync)
          (should-not (memq 'codex-extras-sync-theme enable-theme-functions))
          (should-not (memq 'codex-extras-sync-theme codex-start-hook))
          (should-not (symbol-value 'codex-extras--sync-theme-timer))
          (should-not (fboundp 'codex-extras-sync-theme))
          (should-not (fboundp 'codex-extras--do-sync-theme)))
      (cancel-timer timer)
      (when (fboundp 'codex-extras-sync-theme)
        (fmakunbound 'codex-extras-sync-theme))
      (when (fboundp 'codex-extras--do-sync-theme)
        (fmakunbound 'codex-extras--do-sync-theme))
      (when (boundp 'codex-extras--sync-theme-timer)
        (makunbound 'codex-extras--sync-theme-timer)))))

(ert-deftest codex-extras-test-sync-theme-before-start ()
  "Run shared theme sync before starting a Codex process."
  (let ((called nil))
    (cl-letf (((symbol-function 'ai-extras-sync-theme-now)
               (lambda (&rest _) (setq called t))))
      (should-not (codex-extras--sync-theme-before-start "buf" "/tmp"))
      (should called))))

;;;; Skill runner

(ert-deftest codex-extras-test-parse-skill-frontmatter-argument-metadata ()
  "Parse Codex skill argument metadata with the shared parser."
  (let ((file (make-temp-file "codex-skill" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\n")
            (insert "name: proofread\n")
            (insert "description: Proofread a file\n")
            (insert "argument-hint: FILE\n")
            (insert "argument-source: references/*.org\n")
            (insert "---\n"))
          (let ((meta (codex-extras--parse-skill-frontmatter file)))
            (should (equal (plist-get meta :name) "proofread"))
            (should (equal (plist-get meta :argument-hint) "FILE"))
            (should (equal (plist-get meta :argument-source)
                           "references/*.org"))))
      (delete-file file))))

(ert-deftest codex-extras-test-run-skill-prefers-current-codex-buffer ()
  "Send skills to the current Codex buffer when already in one."
  (let (sent-buffer sent-command displayed)
    (with-temp-buffer
      (let ((target (current-buffer)))
        (cl-letf (((symbol-function 'codex--buffer-p)
                   (lambda (buffer) (eq buffer target)))
                  ((symbol-function 'codex--get-or-prompt-for-buffer)
                   (lambda () (error "should not prompt")))
                  ((symbol-function 'codex--do-send-command)
                   (lambda (command)
                     (setq sent-buffer (current-buffer)
                           sent-command command)))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _args) (setq displayed buffer))))
          (codex-extras-run-skill "proofread" "file.org")
          (should (eq sent-buffer target))
          (should (eq displayed target))
          (should (equal sent-command "/proofread file.org")))))))

(provide 'codex-extras-test)
;;; codex-extras-test.el ends here
