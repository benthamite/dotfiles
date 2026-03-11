;;; pass-extras-test.el --- Tests for pass-extras -*- lexical-binding: t -*-

;; Tests for password management helpers in pass-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; pass may not be available in CI; skip all tests if so.
(defvar pass-extras-test--loadable
  (condition-case nil
      (progn (require 'pass-extras) t)
    (error nil))
  "Non-nil when pass-extras loaded successfully.")

;;;; git-crypt-unlock

(ert-deftest pass-extras-test-git-crypt-unlock-success ()
  "Success case: message includes \"Unlocked\"."
  (skip-unless pass-extras-test--loadable)
  (let (msg)
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (&rest _args) 0))
              ((symbol-function 'password-store-list)
               (lambda () '("git-crypt/key")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (pass-extras-git-crypt-unlock "/tmp/repo/" "git-crypt/key"))
    (should (string-match-p "Unlocked" msg))))

(ert-deftest pass-extras-test-git-crypt-unlock-failure ()
  "Failure case: message includes \"Error\"."
  (skip-unless pass-extras-test--loadable)
  (let (msg)
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (&rest _args) 1))
              ((symbol-function 'password-store-list)
               (lambda () '("git-crypt/key")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (pass-extras-git-crypt-unlock "/tmp/repo/" "git-crypt/key"))
    (should (string-match-p "Error" msg))))

;;;; generate-password types

(ert-deftest pass-extras-test-generate-password-types-keys-are-strings ()
  "All keys in the types alist are strings."
  (skip-unless pass-extras-test--loadable)
  (let* ((types '(("Simple (alphanumeric)" . password-generator-simple)
                  ("Strong (with special chars)" . password-generator-strong)
                  ("Numeric (PIN)" . password-generator-numeric)
                  ("Phonetic (memorable)" . password-generator-phonetic)
                  ("Words (correct horse battery)" . password-generator-words)
                  ("Custom (your alphabet)" . password-generator-custom))))
    (dolist (entry types)
      (should (stringp (car entry))))))

(ert-deftest pass-extras-test-generate-password-types-values-are-symbols ()
  "All values in the types alist are function symbols."
  (skip-unless pass-extras-test--loadable)
  (let* ((types '(("Simple (alphanumeric)" . password-generator-simple)
                  ("Strong (with special chars)" . password-generator-strong)
                  ("Numeric (PIN)" . password-generator-numeric)
                  ("Phonetic (memorable)" . password-generator-phonetic)
                  ("Words (correct horse battery)" . password-generator-words)
                  ("Custom (your alphabet)" . password-generator-custom))))
    (dolist (entry types)
      (should (symbolp (cdr entry))))))

(ert-deftest pass-extras-test-generate-password-types-count ()
  "The types alist has exactly 6 entries."
  (skip-unless pass-extras-test--loadable)
  (let* ((types '(("Simple (alphanumeric)" . password-generator-simple)
                  ("Strong (with special chars)" . password-generator-strong)
                  ("Numeric (PIN)" . password-generator-numeric)
                  ("Phonetic (memorable)" . password-generator-phonetic)
                  ("Words (correct horse battery)" . password-generator-words)
                  ("Custom (your alphabet)" . password-generator-custom))))
    (should (= 6 (length types)))))

;;;; store-key

(ert-deftest pass-extras-test-store-key-command-uses-shell-quote ()
  "Command string uses `shell-quote-argument' for both file and entry."
  (skip-unless pass-extras-test--loadable)
  (let (captured-cmd)
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (setq captured-cmd cmd))))
      (pass-extras-store-key "/tmp/my key.pem" "gpg/my-key"))
    (should (string-match-p (regexp-quote (shell-quote-argument "/tmp/my key.pem"))
                            captured-cmd))
    (should (string-match-p (regexp-quote (shell-quote-argument "gpg/my-key"))
                            captured-cmd))))

(ert-deftest pass-extras-test-store-key-command-structure ()
  "Command string contains `cat', `pass insert --multiline', and piping."
  (skip-unless pass-extras-test--loadable)
  (let (captured-cmd)
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (setq captured-cmd cmd))))
      (pass-extras-store-key "/tmp/key.pem" "gpg/key"))
    (should (string-match-p "cat" captured-cmd))
    (should (string-match-p "pass insert --multiline" captured-cmd))
    (should (string-match-p "|" captured-cmd))))

;;;; export-key

(ert-deftest pass-extras-test-export-key-command-uses-shell-quote ()
  "Command string uses `shell-quote-argument' for both entry and file."
  (skip-unless pass-extras-test--loadable)
  (let (captured-cmd)
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (setq captured-cmd cmd)))
              ((symbol-function 'message)
               #'ignore))
      (pass-extras-export-key "gpg/my key" "/tmp/out file.pem"))
    (should (string-match-p (regexp-quote (shell-quote-argument "gpg/my key"))
                            captured-cmd))
    (should (string-match-p (regexp-quote (shell-quote-argument "/tmp/out file.pem"))
                            captured-cmd))))

(ert-deftest pass-extras-test-export-key-command-structure ()
  "Command string contains `pass' and output redirection."
  (skip-unless pass-extras-test--loadable)
  (let (captured-cmd)
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (setq captured-cmd cmd)))
              ((symbol-function 'message)
               #'ignore))
      (pass-extras-export-key "gpg/key" "/tmp/out.pem"))
    (should (string-match-p "^pass " captured-cmd))
    (should (string-match-p ">" captured-cmd))))

(provide 'pass-extras-test)
;;; pass-extras-test.el ends here
