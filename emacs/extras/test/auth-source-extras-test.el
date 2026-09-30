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

(ert-deftest auth-source-extras-op-get-rate-limit-is-retried-on-next-call ()
  (auth-source-extras-test--with-fake-op batches
    (let ((limited t))
      (cl-letf* ((fake (symbol-function 'auth-source-extras--op-items))
                 ((symbol-function 'auth-source-extras--op-items)
                  (lambda (account titles)
                    (if limited
                        (progn (push (cons account titles) batches)
                               (mapcar (lambda (title)
                                         (cons title '(:error . "Too many requests. Please try again later.")))
                                       titles))
                      (funcall fake account titles)))))
        (should-not (auth-source-extras-op-get "alpha" "credential"))
        (should-not (gethash '(personal . "alpha") auth-source-extras--op-cache))
        (setq limited nil)
        (should (equal (auth-source-extras-op-get "alpha" "credential") "A-SECRET"))
        (should (equal batches '((personal "alpha") (personal "alpha"))))))))

(ert-deftest auth-source-extras-op-get-unreadable-account-is-not-cached-as-missing ()
  (auth-source-extras-test--with-fake-op batches
    (auth-source-extras-op-get "alpha" "credential" 'broken)
    (auth-source-extras-op-get "alpha" "credential" 'broken)
    (should (equal batches '((broken "alpha") (broken "alpha"))))))

(ert-deftest auth-source-extras-op-not-found-p-separates-absence-from-transient-failures ()
  (should (auth-source-extras--op-not-found-p
           "\"gamma\" isn't an item in the \"Automation\" vault. Specify the item with its UUID, name, or domain."))
  (should (auth-source-extras--op-not-found-p "no item titled exactly `ea.news'"))
  (dolist (reason '("Too many requests. Please try again later."
                    "timed out"
                    "op-automations: no token in the Keychain for op-service-account/personal-automation"
                    "dial tcp: lookup my.1password.com: no such host"
                    "exit status 1"))
    (should-not (auth-source-extras--op-not-found-p reason))))

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
                 '("t" ("f1" . "V") ("api" . "V")))))

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
    (should (auth-source-extras-op-search :host "api.github.com"
                                          :user "benthamite^forge" :create t))
    (should-not (auth-source-extras-op-search :host "unknown.example.com"))))

(ert-deftest auth-source-extras-op-backend-is-selected-by-its-symbol ()
  (should (auth-source-extras-op-backend-parse '1password-automation))
  (should-not (auth-source-extras-op-backend-parse 'macos-keychain-internet)))

(ert-deftest auth-source-extras-op-vault-uses-account-specific-names ()
  (let ((auth-source-extras-op-account-vaults '((epoch . "Automations"))))
    (should (equal (auth-source-extras--op-vault 'epoch) "Automations"))
    (should (equal (auth-source-extras--op-vault 'personal) "Automation"))))

(ert-deftest auth-source-extras-op-start-passes-the-account-vault ()
  (let ((auth-source-extras-op-account-vaults '((epoch . "Automations")))
        command)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args) (setq command (plist-get args :command)) nil))
              ((symbol-function 'make-pipe-process) #'ignore)
              ((symbol-function 'process-put) #'ignore))
      (auth-source-extras--op-start 'epoch "item"))
    (should (equal (member "--vault" command) '("--vault" "Automations" "--format" "json")))))

(ert-deftest auth-source-extras-op-exact-fields-rejects-a-fuzzy-match ()
  (let ((item '((title . "ea.news-api") (fields ((id . "password") (label . "password") (value . "X"))))))
    (should (equal (auth-source-extras--op-exact-fields "ea.news-api" item)
                   '(("password" . "X") ("password" . "X"))))
    (should (eq (car (auth-source-extras--op-exact-fields "ea.news" item)) :error))))

(ert-deftest auth-source-extras-git-crypt-unlock-uses-and-deletes-key-file ()
  (let (key-file unlocked-with)
    (cl-letf (((symbol-function 'auth-source-extras--op-save-document)
               (lambda (account title file)
                 (should (eq account 'tlon))
                 (should (equal title "repo-key"))
                 (setq key-file file)
                 (should (= (file-modes file) #o600))))
              ((symbol-function 'call-process)
               (lambda (program _in _out _display &rest args)
                 (should (equal program "git-crypt"))
                 (setq unlocked-with args)
                 0))
              ((symbol-function 'message) #'ignore))
      (auth-source-extras-git-crypt-unlock temporary-file-directory "repo-key"))
    (should (equal unlocked-with (list "unlock" key-file)))
    (should-not (file-exists-p key-file))))

(ert-deftest auth-source-extras-git-crypt-unlock-failure-still-deletes-key ()
  (let (key-file)
    (cl-letf (((symbol-function 'auth-source-extras--op-save-document)
               (lambda (_account _title file) (setq key-file file)))
              ((symbol-function 'call-process) (lambda (&rest _) 1)))
      (should-error (auth-source-extras-git-crypt-unlock temporary-file-directory "repo-key")
                    :type 'user-error))
    (should-not (file-exists-p key-file))))

(ert-deftest auth-source-extras-op-search-cached-smtp-survives-failed-listing ()
  (auth-source-extras-test--with-fake-op batches
    (puthash 'personal 'missing auth-source-extras--op-titles)
    (puthash '(personal . "smtp.example.com/me")
             '(("password" . "APP-PASSWORD")) auth-source-extras--op-cache)
    (cl-letf (((symbol-function 'auth-source-extras--op-list-titles)
               (lambda (&rest _) (ert-fail "Cached SMTP must not list any vault"))))
      (let ((result (car (auth-source-extras-op-search
                         :host "smtp.example.com" :port "465" :user "me"))))
        (should (equal (funcall (plist-get result :secret)) "APP-PASSWORD"))))))

(ert-deftest auth-source-extras-op-search-retries-failed-listing ()
  (auth-source-extras-test--with-fake-op batches
    (puthash 'personal 'missing auth-source-extras--op-titles)
    (should (auth-source-extras-op-search :host "imap.example.com" :user "me@example.com"))))

(ert-deftest auth-source-extras-op-search-does-not-list-unneeded-account ()
  (auth-source-extras-test--with-fake-op batches
    (cl-letf (((symbol-function 'auth-source-extras--op-list-titles)
               (lambda (account)
                 (should (eq account 'personal))
                 '("imap.example.com"))))
      (should (auth-source-extras-op-search :host "imap.example.com" :user "me@example.com")))))

(ert-deftest auth-source-extras-op-search-listing-failure-is-not-cached ()
  (auth-source-extras-test--with-fake-op batches
    (cl-letf (((symbol-function 'auth-source-extras--op-list-titles)
               (lambda (_) 'missing)))
      (should-not (auth-source-extras-op-search :host "smtp.example.com" :user "me"))
      (should-not (gethash 'personal auth-source-extras--op-titles)))))

(ert-deftest auth-source-extras-op-search-rejects-empty-password ()
  (auth-source-extras-test--with-fake-op batches
    (dolist (fields '((("password" . "")) (("notesPlain" . "notes"))))
      (puthash '(personal . "imap.example.com") fields auth-source-extras--op-cache)
      (should-not (auth-source-extras--op-search-result
                   '(personal "imap.example.com" "imap.example.com" "465")
                   "me@example.com")))))

(ert-deftest auth-source-extras-op-password-id-outranks-template-labels ()
  (let* ((item '((title . "smtp.example.com/me")
                 (fields ((id . "pop_password") (label . "password") (value . ""))
                         ((id . "other") (label . "password") (value . "WRONG"))
                         ((id . "password") (label . "password") (value . "CORRECT")))))
         (fields (cdr (auth-source-extras--op-item-fields item))))
    (should (equal (cdr (assoc "password" fields)) "CORRECT"))
    (should (equal (cdr (assoc "pop_password" fields)) ""))))

(ert-deftest auth-source-extras-op-search-preserves-known-specificity ()
  (auth-source-extras-test--with-fake-op batches
    (puthash 'personal '("smtp.example.com:465/me" "smtp.example.com")
             auth-source-extras--op-titles)
    (puthash '(personal . "smtp.example.com") '(("password" . "GENERIC"))
             auth-source-extras--op-cache)
    (cl-letf (((symbol-function 'auth-source-extras--op-items)
               (lambda (_account _titles)
                 '(("smtp.example.com:465/me" ("password" . "SPECIFIC"))))))
      (let ((result (car (auth-source-extras-op-search
                         :host "smtp.example.com" :user "me" :port "465"))))
        (should (equal (funcall (plist-get result :secret)) "SPECIFIC"))))))

(ert-deftest auth-source-extras-op-search-unavailable-vault-allows-other-account ()
  (auth-source-extras-test--with-fake-op batches
    (puthash '(tlon . "smtp.example.com/me") '(("password" . "AVAILABLE"))
             auth-source-extras--op-cache)
    (cl-letf (((symbol-function 'auth-source-extras--op-list-titles)
               (lambda (_) 'missing)))
      (should (auth-source-extras-op-search :host "smtp.example.com" :user "me")))))

(provide 'auth-source-extras-test)
;;; auth-source-extras-test.el ends here
