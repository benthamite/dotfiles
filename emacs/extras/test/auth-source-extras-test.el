;;; auth-source-extras-test.el --- Tests for auth-source-extras -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'auth-source-extras)

(defconst auth-source-extras-test--vault
  '((((title . "alpha")
      (fields ((id . "credential") (label . "credential") (value . "A-SECRET"))
              ((id . "f0") (label . "cookie") (value . "A-COOKIE")))))
    (((title . "beta")
      (fields ((id . "password") (label . "password") (value . "B-SECRET"))))))
  "Fake Automation vault: parsed `op item get' output for each item.")

(defmacro auth-source-extras-test--with-fake-op (batches-var &rest body)
  "Run BODY against the fake vault, recording fetched title batches in BATCHES-VAR."
  (declare (indent 1))
  `(let ((auth-source-extras--op-cache (make-hash-table :test #'equal))
         (auth-source-extras--op-tokens (make-hash-table :test #'eq))
         (,batches-var nil))
     (cl-letf (((symbol-function 'auth-source-extras--op-keychain-token)
                (lambda (account) (if (eq account 'personal) "ops_fake" 'missing)))
               ((symbol-function 'auth-source-extras--op-items)
                (lambda (_token titles)
                  (push titles ,batches-var)
                  (delq nil (mapcar
                             (lambda (title)
                               (when-let* ((item (seq-find
                                                  (lambda (i) (equal (alist-get 'title (car i)) title))
                                                  auth-source-extras-test--vault)))
                                 (auth-source-extras--op-item-fields (car item))))
                             titles))))
               ((symbol-function 'display-warning) #'ignore))
       ,@body)))

(ert-deftest auth-source-extras-op-get-reads-fields-by-label-and-id ()
  (auth-source-extras-test--with-fake-op batches
    (should (equal (auth-source-extras-op-get "alpha" "credential") "A-SECRET"))
    (should (equal (auth-source-extras-op-get "alpha" "cookie") "A-COOKIE"))
    (should (equal (auth-source-extras-op-get "alpha" "f0") "A-COOKIE"))))

(ert-deftest auth-source-extras-op-get-caches-after-first-fetch ()
  (auth-source-extras-test--with-fake-op batches
    (auth-source-extras-op-get "alpha" "credential")
    (auth-source-extras-op-get "alpha" "cookie")
    (should (equal batches '(("alpha"))))))

(ert-deftest auth-source-extras-op-get-prefetches-in-one-batch ()
  (auth-source-extras-test--with-fake-op batches
    (let ((auth-source-extras-op-prefetch '((personal "alpha" "beta"))))
      (should (equal (auth-source-extras-op-get "alpha" "credential") "A-SECRET"))
      (should (equal (auth-source-extras-op-get "beta" "password") "B-SECRET"))
      (should (equal batches '(("alpha" "beta")))))))

(ert-deftest auth-source-extras-op-get-missing-item-returns-nil-once ()
  (auth-source-extras-test--with-fake-op batches
    (should-not (auth-source-extras-op-get "gamma" "credential"))
    (should-not (auth-source-extras-op-get "gamma" "credential"))
    (should (equal batches '(("gamma"))))))

(ert-deftest auth-source-extras-op-get-missing-token-returns-nil ()
  (auth-source-extras-test--with-fake-op batches
    (should-not (auth-source-extras-op-get "alpha" "credential" 'tlon))
    (should-not batches)))

(ert-deftest auth-source-extras-op-item-fields-keys-by-label-and-id ()
  (should (equal (auth-source-extras--op-item-fields
                  '((title . "t") (fields ((id . "f1") (label . "api") (value . "V")))))
                 '("t" ("api" . "V") ("f1" . "V")))))

(provide 'auth-source-extras-test)
;;; auth-source-extras-test.el ends here
