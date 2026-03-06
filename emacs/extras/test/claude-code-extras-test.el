;;; claude-code-extras-test.el --- Tests for claude-code-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in claude-code-extras.el.

;;; Code:

(require 'ert)
(require 'claude-code-extras)

;;;; Session name extraction

(ert-deftest claude-code-extras-test-session-name-standard ()
  "Extract project name from a standard Claude buffer name."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/path/to/project/:default*")
                 "project")))

(ert-deftest claude-code-extras-test-session-name-named-instance ()
  "Extract project name regardless of instance name."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/repos/my-app/:worktree-1*")
                 "my-app")))

(ert-deftest claude-code-extras-test-session-name-deep-path ()
  "Extract project name from a deeply nested path."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/My Drive/repos/org/subdir/:main*")
                 "subdir")))

(ert-deftest claude-code-extras-test-session-name-non-matching ()
  "Return buffer name unchanged when it does not match the pattern."
  (should (equal (claude-code-extras--session-name "*scratch*")
                 "*scratch*")))

(ert-deftest claude-code-extras-test-session-name-no-trailing-star ()
  "Return buffer name unchanged when trailing asterisk is missing."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/path/to/project/:default")
                 "*claude:~/path/to/project/:default")))

;;;; Sanitize buffer name

(ert-deftest claude-code-extras-test-sanitize-buffer-name-replaces-special ()
  "Non-alphanumeric characters (except _ and -) are replaced with underscores."
  (with-temp-buffer
    (rename-buffer "*claude:~/foo/bar/:default*" t)
    (should (equal (claude-code-extras--sanitize-buffer-name)
                   "_claude___foo_bar__default_"))))

(ert-deftest claude-code-extras-test-sanitize-buffer-name-preserves-safe ()
  "Alphanumeric characters, underscores, and hyphens are preserved."
  (with-temp-buffer
    (rename-buffer "hello_world-123" t)
    (should (equal (claude-code-extras--sanitize-buffer-name)
                   "hello_world-123"))))

(ert-deftest claude-code-extras-test-sanitize-buffer-name-spaces ()
  "Spaces are replaced with underscores."
  (with-temp-buffer
    (rename-buffer "my buffer name" t)
    (should (equal (claude-code-extras--sanitize-buffer-name)
                   "my_buffer_name"))))

;;;; Find duplicate names

(ert-deftest claude-code-extras-test-find-duplicate-names-no-duplicates ()
  "Return nil when all names are unique."
  (should (null (claude-code-extras--find-duplicate-names
                 '("alpha" "beta" "gamma")))))

(ert-deftest claude-code-extras-test-find-duplicate-names-one-duplicate ()
  "Return a list containing the duplicated name."
  (should (equal (claude-code-extras--find-duplicate-names
                  '("alpha" "beta" "alpha"))
                 '("alpha"))))

(ert-deftest claude-code-extras-test-find-duplicate-names-multiple-duplicates ()
  "Return all names that appear more than once."
  (let ((result (claude-code-extras--find-duplicate-names
                 '("alpha" "beta" "alpha" "beta" "gamma"))))
    (should (member "alpha" result))
    (should (member "beta" result))
    (should (= (length result) 2))))

(ert-deftest claude-code-extras-test-find-duplicate-names-empty ()
  "Return nil for an empty list."
  (should (null (claude-code-extras--find-duplicate-names '()))))

(ert-deftest claude-code-extras-test-find-duplicate-names-triple ()
  "A name appearing three times is reported once."
  (let ((result (claude-code-extras--find-duplicate-names
                 '("x" "x" "x"))))
    (should (equal result '("x")))))

;;;; Deduplicate names

(ert-deftest claude-code-extras-test-deduplicate-names-no-duplicates ()
  "Return names unchanged when all are unique."
  (should (equal (claude-code-extras--deduplicate-names
                  '("alpha" "beta" "gamma"))
                 '("alpha" "beta" "gamma"))))

(ert-deftest claude-code-extras-test-deduplicate-names-with-duplicates ()
  "First occurrence keeps its name; subsequent get numeric suffixes."
  (should (equal (claude-code-extras--deduplicate-names
                  '("proj" "proj" "proj"))
                 '("proj" "proj (2)" "proj (3)"))))

(ert-deftest claude-code-extras-test-deduplicate-names-mixed ()
  "Only duplicate names get suffixes; unique names are unchanged."
  (should (equal (claude-code-extras--deduplicate-names
                  '("alpha" "beta" "alpha" "gamma" "beta"))
                 '("alpha" "beta" "alpha (2)" "gamma" "beta (2)"))))

(ert-deftest claude-code-extras-test-deduplicate-names-empty ()
  "Return nil for an empty list."
  (should (null (claude-code-extras--deduplicate-names '()))))

(ert-deftest claude-code-extras-test-deduplicate-names-single ()
  "A single-element list is returned unchanged."
  (should (equal (claude-code-extras--deduplicate-names '("only"))
                 '("only"))))

;;;; Emacs theme detection

(ert-deftest claude-code-extras-test-emacs-theme-dark ()
  "Return \"dark\" when frame background mode is dark."
  (let ((frame-params '((background-mode . dark))))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame param) (alist-get param frame-params))))
      (should (equal (claude-code-extras--emacs-theme) "dark")))))

(ert-deftest claude-code-extras-test-emacs-theme-light ()
  "Return \"light\" when frame background mode is light."
  (let ((frame-params '((background-mode . light))))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame param) (alist-get param frame-params))))
      (should (equal (claude-code-extras--emacs-theme) "light")))))

;;;; Batch format prompt

(ert-deftest claude-code-extras-test-batch-format-prompt-title-only ()
  "Return title alone when body is empty."
  (should (equal (claude-code-extras--batch-format-prompt
                  '(:title "Fix the bug" :body ""))
                 "Fix the bug")))

(ert-deftest claude-code-extras-test-batch-format-prompt-title-and-body ()
  "Return title and body separated by blank line."
  (should (equal (claude-code-extras--batch-format-prompt
                  '(:title "Fix the bug" :body "See error in log"))
                 "Fix the bug\n\nSee error in log")))

(ert-deftest claude-code-extras-test-batch-format-prompt-nil-body ()
  "Return title alone when body is nil."
  (should (equal (claude-code-extras--batch-format-prompt
                  '(:title "Refactor module" :body nil))
                 "Refactor module")))

(provide 'claude-code-extras-test)
;;; claude-code-extras-test.el ends here
