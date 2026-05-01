;;; magit-extra-test.el --- Tests for magit-extra -*- lexical-binding: t -*-

;; Tests for pure helper functions in magit-extra.el.

;;; Code:

(require 'cl-lib)
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
  "Returns the first staged file relative to the repository root."
  (cl-letf (((symbol-function 'magit-staged-files)
             (lambda () '("src/main.el"))))
    (should (equal (magit-extras-get-commit-file) "src/main.el"))))

(ert-deftest magit-extra-test-get-commit-file-sans-dir ()
  "Returns just the filename without directory when PATH is `sans-dir'."
  (cl-letf (((symbol-function 'magit-staged-files)
             (lambda () '("src/main.el"))))
    (should (equal (magit-extras-get-commit-file 'sans-dir) "main.el"))))

(ert-deftest magit-extra-test-get-commit-file-full ()
  "Returns the full path when PATH is `full'."
  (cl-letf (((symbol-function 'magit-staged-files)
             (lambda () '("src/main.el")))
            ((symbol-function 'magit-toplevel)
             (lambda () "/repo")))
    (should (equal (magit-extras-get-commit-file 'full) "/repo/src/main.el"))))

(ert-deftest magit-extra-test-get-commit-file-no-staged ()
  "Signals a user-error when no file is staged."
  (cl-letf (((symbol-function 'magit-staged-files) (lambda () nil)))
    (should-error (magit-extras-get-commit-file) :type 'user-error)))

(provide 'magit-extra-test)
;;; magit-extra-test.el ends here
