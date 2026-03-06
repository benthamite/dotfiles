;;; magit-extra-test.el --- Tests for magit-extra -*- lexical-binding: t -*-

;; Tests for pure helper functions in magit-extra.el.

;;; Code:

(require 'ert)
(require 'magit-extra)

;;;; magit-extras-parse-url

(ert-deftest magit-extra-test-parse-url-ssh-to-https ()
  "Converts an SSH git URL to HTTPS."
  (should (equal (magit-extras-parse-url "git@github.com:user/repo.git")
                 "https://github.com/user/repo")))

(ert-deftest magit-extra-test-parse-url-ssh-bitbucket ()
  "Converts a Bitbucket SSH URL to HTTPS."
  (should (equal (magit-extras-parse-url "git@bitbucket.org:team/project.git")
                 "https://bitbucket.org/team/project")))

(ert-deftest magit-extra-test-parse-url-ssh-gitlab ()
  "Converts a GitLab SSH URL to HTTPS."
  (should (equal (magit-extras-parse-url "git@gitlab.com:group/repo.git")
                 "https://gitlab.com/group/repo")))

(ert-deftest magit-extra-test-parse-url-https-passthrough ()
  "Returns HTTPS URLs unchanged."
  (should (equal (magit-extras-parse-url "https://github.com/user/repo.git")
                 "https://github.com/user/repo.git")))

(ert-deftest magit-extra-test-parse-url-http-passthrough ()
  "Returns HTTP URLs unchanged."
  (should (equal (magit-extras-parse-url "http://github.com/user/repo.git")
                 "http://github.com/user/repo.git")))

;;;; magit-extras-get-commit-file

(ert-deftest magit-extra-test-get-commit-file-relative ()
  "Extracts the relative file path from a commit buffer."
  (with-temp-buffer
    (insert "# Changes to be committed:\n#\tmodified:   src/main.el\n")
    (let ((file (magit-extras-get-commit-file)))
      (should (equal file "src/main.el")))))

(ert-deftest magit-extra-test-get-commit-file-sans-dir ()
  "Extracts just the filename without directory."
  (with-temp-buffer
    (insert "# Changes to be committed:\n#\tmodified:   src/main.el\n")
    (let ((file (magit-extras-get-commit-file 'sans-dir)))
      (should (equal file "main.el")))))

(ert-deftest magit-extra-test-get-commit-file-no-staged ()
  "Signals error when no staged file is found."
  (with-temp-buffer
    (insert "# nothing to commit\n")
    (should-error (magit-extras-get-commit-file)
                  :type 'user-error)))

(provide 'magit-extra-test)
;;; magit-extra-test.el ends here
