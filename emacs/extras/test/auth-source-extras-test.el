;;; auth-source-extras-test.el --- Tests for auth-source-extras -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'auth-source-extras)

(defconst auth-source-extras-test--vaults
  '((personal
     ("alpha" ("credential" . "A-SECRET") ("f0" . "A-COOKIE") ("cookie" . "A-COOKIE"))
     ("beta" ("password" . "B-SECRET"))
     ("api.github.com/benthamite^forge" ("password" . "GH-FORGE") ("username" . "benthamite^forge"))
     ("imap.example.com" ("password" . "IMAP") ("username" . "me@example.com")))
    (tlon
     ("api.github.com/worldsaround^forge" ("password" . "GH-TLON"))))
  "Fake automation vaults: account, then (TITLE . FIELDS) items.")

(defmacro auth-source-extras-test--with-fake-op (batches-var &rest body)
  "Run BODY against the fake vaults, recording fetched batches in BATCHES-VAR.
Each recorded batch is (ACCOUNT . TITLES).  The account `broken' has no token."
  (declare (indent 1))
  `(let ((auth-source-extras--op-cache (make-hash-table :test #'equal))
         (auth-source-extras--op-titles (make-hash-table :test #'eq))
         (,batches-var nil))
     (cl-letf (((symbol-function 'auth-source-extras--op-items)
                (lambda (account titles)
                  (push (cons account titles) ,batches-var)
                  (mapcar (lambda (title)
                            (cons title
                                  (if (eq account 'broken)
                                      '(:error . "no token in the Keychain")
                                    (or (cdr (assoc title (alist-get account auth-source-extras-test--vaults)))
                                        '(:error . "isn't an item")))))
                          titles)))
               ((symbol-function 'auth-source-extras--op-list-titles)
                (lambda (account)
                  (mapcar #'car (alist-get account auth-source-extras-test--vaults))))
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
    (should (equal batches '((personal "alpha"))))))

(ert-deftest auth-source-extras-op-get-prefetches-in-one-batch ()
  (auth-source-extras-test--with-fake-op batches
    (let ((auth-source-extras-op-prefetch '((personal "alpha" "beta"))))
      (should (equal (auth-source-extras-op-get "alpha" "credential") "A-SECRET"))
      (should (equal (auth-source-extras-op-get "beta" "password") "B-SECRET"))
      (should (equal batches '((personal "alpha" "beta")))))))

(ert-deftest auth-source-extras-op-get-missing-item-returns-nil-once ()
  (auth-source-extras-test--with-fake-op batches
    (should-not (auth-source-extras-op-get "gamma" "credential"))
    (should-not (auth-source-extras-op-get "gamma" "credential"))
    (should (equal batches '((personal "gamma"))))))

(ert-deftest auth-source-extras-op-get-unreadable-account-returns-nil ()
  (auth-source-extras-test--with-fake-op batches
    (should-not (auth-source-extras-op-get "alpha" "credential" 'broken))))

(ert-deftest auth-source-extras-op-fetch-warns-once-per-reason ()
  (auth-source-extras-test--with-fake-op batches
    (let (warnings
          (auth-source-extras-op-prefetch '((broken "a" "b" "c"))))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (auth-source-extras-op-get "a" "password" 'broken))
      (should (= (length warnings) 1))
      (should (string-match-p "a, b, c" (car warnings))))))

(ert-deftest auth-source-extras-op-item-fields-keys-by-label-and-id ()
  (should (equal (auth-source-extras--op-item-fields
                  '((title . "t") (fields ((id . "f1") (label . "api") (value . "V")))))
                 '("t" ("api" . "V") ("f1" . "V")))))

(ert-deftest auth-source-extras-op-search-finds-host-slash-user ()
  (auth-source-extras-test--with-fake-op batches
    (let ((result (car (auth-source-extras-op-search
                        :host "api.github.com" :user "benthamite^forge" :max 1))))
      (should (equal (plist-get result :user) "benthamite^forge"))
      (should (equal (funcall (plist-get result :secret)) "GH-FORGE")))))

(ert-deftest auth-source-extras-op-search-falls-through-to-second-account ()
  (auth-source-extras-test--with-fake-op batches
    (let ((result (car (auth-source-extras-op-search
                        :host "api.github.com" :user "worldsaround^forge"))))
      (should (equal (funcall (plist-get result :secret)) "GH-TLON")))))

(ert-deftest auth-source-extras-op-search-bare-host-checks-stored-user ()
  (auth-source-extras-test--with-fake-op batches
    (should (auth-source-extras-op-search :host "imap.example.com" :user "me@example.com"))
    (should (auth-source-extras-op-search :host "imap.example.com"))
    (should-not (auth-source-extras-op-search :host "imap.example.com" :user "other"))))

(ert-deftest auth-source-extras-op-search-refuses-create-and-unknown-hosts ()
  (auth-source-extras-test--with-fake-op batches
    (should-not (auth-source-extras-op-search :host "api.github.com"
                                              :user "benthamite^forge" :create t))
    (should-not (auth-source-extras-op-search :host "unknown.example.com"))))

(ert-deftest auth-source-extras-op-backend-is-selected-by-its-symbol ()
  (should (auth-source-extras-op-backend-parse '1password-automation))
  (should-not (auth-source-extras-op-backend-parse 'password-store)))

(provide 'auth-source-extras-test)
;;; auth-source-extras-test.el ends here
