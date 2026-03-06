;;; vc-extras-test.el --- Tests for vc-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in vc-extras.el.

;;; Code:

(require 'ert)
(require 'vc-extras)

;;;; vc-extras-is-git-dir-p

(ert-deftest vc-extras-test-is-git-dir-p-real-git-repo ()
  "Returns non-nil for a real git repository."
  (let ((dir (make-temp-file "vc-extras-test-" t)))
    (unwind-protect
        (progn
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init"))
          (should (vc-extras-is-git-dir-p dir)))
      (delete-directory dir t))))

(ert-deftest vc-extras-test-is-git-dir-p-not-git ()
  "Returns nil for a directory that is not a git repository."
  (let ((dir (make-temp-file "vc-extras-test-" t)))
    (unwind-protect
        (should-not (vc-extras-is-git-dir-p dir))
      (delete-directory dir t))))

;;;; vc-extras-has-submodules-p

(ert-deftest vc-extras-test-has-submodules-p-with-gitmodules ()
  "Returns non-nil when .gitmodules exists."
  (let ((dir (make-temp-file "vc-extras-test-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".gitmodules" dir)
            (insert "[submodule \"sub\"]\n\tpath = sub\n\turl = https://example.com/sub.git\n"))
          (should (vc-extras-has-submodules-p dir)))
      (delete-directory dir t))))

(ert-deftest vc-extras-test-has-submodules-p-without-gitmodules ()
  "Returns nil when .gitmodules does not exist."
  (let ((dir (make-temp-file "vc-extras-test-" t)))
    (unwind-protect
        (should-not (vc-extras-has-submodules-p dir))
      (delete-directory dir t))))

;;;; vc-extras-get-github-remote

(ert-deftest vc-extras-test-get-github-remote-with-account ()
  "Returns correct HTTPS URL when account is provided."
  (should (equal (vc-extras-get-github-remote "my-repo" "my-user")
                 "https://github.com/my-user/my-repo.git")))

(ert-deftest vc-extras-test-get-github-remote-format ()
  "Returns URL in the expected format."
  (let ((url (vc-extras-get-github-remote "dotfiles" "benthamite")))
    (should (string-prefix-p "https://github.com/" url))
    (should (string-suffix-p ".git" url))
    (should (string-match-p "benthamite/dotfiles" url))))

;;;; vc-extras-strip-diff-markers

(ert-deftest vc-extras-test-strip-diff-markers-plus-lines ()
  "Removes leading `+' from added lines."
  (with-temp-buffer
    (insert "+added line 1\n+added line 2\n")
    (vc-extras-strip-diff-markers (point-min) (point-max))
    (should (equal (buffer-string) "added line 1\nadded line 2\n"))))

(ert-deftest vc-extras-test-strip-diff-markers-minus-lines ()
  "Removes leading `-' from removed lines."
  (with-temp-buffer
    (insert "-removed line 1\n-removed line 2\n")
    (vc-extras-strip-diff-markers (point-min) (point-max))
    (should (equal (buffer-string) "removed line 1\nremoved line 2\n"))))

(ert-deftest vc-extras-test-strip-diff-markers-mixed ()
  "Removes markers from mixed added/removed/context lines."
  (with-temp-buffer
    (insert "+added\n-removed\n context\n")
    (vc-extras-strip-diff-markers (point-min) (point-max))
    (should (equal (buffer-string) "added\nremoved\n context\n"))))

(ert-deftest vc-extras-test-strip-diff-markers-no-markers ()
  "Leaves lines without diff markers unchanged."
  (with-temp-buffer
    (insert "no marker\nanother line\n")
    (vc-extras-strip-diff-markers (point-min) (point-max))
    (should (equal (buffer-string) "no marker\nanother line\n"))))

(ert-deftest vc-extras-test-strip-diff-markers-region ()
  "Only strips markers within the specified region."
  (with-temp-buffer
    (insert "+first\n+second\n+third\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((start (point)))
      (forward-line 1)
      (vc-extras-strip-diff-markers start (point)))
    (should (equal (buffer-string) "+first\nsecond\n+third\n"))))

(ert-deftest vc-extras-test-strip-diff-markers-empty-buffer ()
  "Handles empty buffer without error."
  (with-temp-buffer
    (vc-extras-strip-diff-markers (point-min) (point-max))
    (should (equal (buffer-string) ""))))

;;;; vc-extras-get-account-prop

(ert-deftest vc-extras-test-get-account-prop-returns-dir ()
  "Returns the :dir property for a known account."
  (let ((vc-extras-profiles
         '((:name personal :account "testuser" :dir "/tmp/repos"))))
    (should (equal (vc-extras-get-account-prop "testuser" :dir)
                   "/tmp/repos"))))

(ert-deftest vc-extras-test-get-account-prop-returns-name ()
  "Returns the :name property for a known account."
  (let ((vc-extras-profiles
         '((:name personal :account "testuser" :dir "/tmp/repos"))))
    (should (equal (vc-extras-get-account-prop "testuser" :name)
                   'personal))))

(ert-deftest vc-extras-test-get-account-prop-unknown-account ()
  "Returns nil for an unknown account."
  (let ((vc-extras-profiles
         '((:name personal :account "testuser" :dir "/tmp/repos"))))
    (should-not (vc-extras-get-account-prop "nonexistent" :dir))))

;;;; vc-extras-get-repo-dir

(ert-deftest vc-extras-test-get-repo-dir-default ()
  "Returns the repo directory under the account dir."
  (let ((vc-extras-profiles
         '((:name personal :account "testuser" :dir "/tmp/repos"))))
    (let ((result (vc-extras-get-repo-dir "my-project" "testuser")))
      (should (string-match-p "my-project" result))
      (should (string-prefix-p "/tmp/repos" result)))))

(ert-deftest vc-extras-test-get-repo-dir-with-git ()
  "Returns the .git subdirectory when GIT is `git'."
  (let ((vc-extras-profiles
         '((:name personal :account "testuser" :dir "/tmp/repos"))))
    (let ((result (vc-extras-get-repo-dir "my-project" "testuser" 'git)))
      (should (string-match-p "\\.git/" result)))))

;;;; vc-extras--get-submodule-paths-from-gitmodules

(ert-deftest vc-extras-test-get-submodule-paths-from-gitmodules ()
  "Parses .gitmodules and returns submodule paths."
  (let ((dir (make-temp-file "vc-extras-test-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".gitmodules" dir)
            (insert "[submodule \"libfoo\"]\n\tpath = lib/foo\n\turl = https://example.com/foo.git\n")
            (insert "[submodule \"libbar\"]\n\tpath = lib/bar\n\turl = https://example.com/bar.git\n"))
          (let ((paths (vc-extras--get-submodule-paths-from-gitmodules dir)))
            (should (equal paths '("lib/foo" "lib/bar")))))
      (delete-directory dir t))))

(ert-deftest vc-extras-test-get-submodule-paths-from-gitmodules-no-file ()
  "Returns nil when .gitmodules does not exist."
  (let ((dir (make-temp-file "vc-extras-test-" t)))
    (unwind-protect
        (should-not (vc-extras--get-submodule-paths-from-gitmodules dir))
      (delete-directory dir t))))

;;;; vc-extras-resolve-repo-dir

(ert-deftest vc-extras-test-resolve-repo-dir-with-account ()
  "Resolves repo dir using account when account is provided."
  (let ((vc-extras-profiles
         '((:name personal :account "testuser" :dir "/tmp/repos"))))
    (let ((result (vc-extras-resolve-repo-dir "my-repo" "testuser" nil)))
      (should (string-match-p "/tmp/repos.*my-repo" result)))))

(ert-deftest vc-extras-test-resolve-repo-dir-from-candidates ()
  "Resolves repo dir from candidates when account is nil."
  (let ((candidates '(("my-repo" . "/tmp/repos/my-repo"))))
    (should (equal (vc-extras-resolve-repo-dir "my-repo" nil candidates)
                   "/tmp/repos/my-repo"))))

(ert-deftest vc-extras-test-resolve-repo-dir-not-found ()
  "Signals error when repo is not found in candidates."
  (let ((candidates '(("other-repo" . "/tmp/repos/other-repo"))))
    (should-error (vc-extras-resolve-repo-dir "my-repo" nil candidates)
                  :type 'user-error)))

(provide 'vc-extras-test)
;;; vc-extras-test.el ends here
