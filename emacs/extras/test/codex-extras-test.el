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

(ert-deftest codex-extras-test-sync-theme-before-start ()
  "Run shared theme sync before starting a Codex process."
  (let ((called nil))
    (cl-letf (((symbol-function 'ai-extras-sync-theme-now)
               (lambda (&rest _) (setq called t))))
      (should-not (codex-extras--sync-theme-before-start "buf" "/tmp"))
      (should called))))

(provide 'codex-extras-test)
;;; codex-extras-test.el ends here
