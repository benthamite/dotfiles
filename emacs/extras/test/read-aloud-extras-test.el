;;; read-aloud-extras-test.el --- Tests for read-aloud-extras -*- lexical-binding: t -*-

;; Tests for engine update and rate manipulation functions
;; in read-aloud-extras.el.

;;; Code:

(require 'ert)
(require 'read-aloud-extras)

;;;; update-engine

(ert-deftest read-aloud-extras-test-update-engine-adds-new ()
  "Update-engine inserts a new engine into the engines plist."
  (let ((read-aloud-engines '("existing" (cmd "echo" args ()))))
    (read-aloud-extras-update-engine "test" '(cmd "say" args ("-r 200")))
    (should (plist-get read-aloud-engines "test" 'equal))
    (let ((params (plist-get read-aloud-engines "test" 'equal)))
      (should (equal (plist-get params 'cmd) "say")))))

(ert-deftest read-aloud-extras-test-update-engine-overwrites ()
  "Update-engine overwrites an existing engine's parameters."
  (let ((read-aloud-engines '("say" (cmd "say" args ("-r 100")))))
    (read-aloud-extras-update-engine "say" '(cmd "say" args ("-r 200")))
    (let ((params (plist-get read-aloud-engines "say" 'equal)))
      (should (equal (plist-get params 'args) '("-r 200"))))))

;;;; update-say-engine

(ert-deftest read-aloud-extras-test-update-say-engine-formats-correctly ()
  "Update-say-engine formats voice and rate into say engine args."
  (let ((read-aloud-engines '("say" (cmd "say" args ())))
        (read-aloud-extras-voice "Samantha")
        (read-aloud-extras-rate 250))
    (read-aloud-extras-update-say-engine)
    (let ((params (plist-get read-aloud-engines "say" 'equal)))
      (should (member "-v Samantha" (plist-get params 'args)))
      (should (member "-r 250" (plist-get params 'args))))))

(provide 'read-aloud-extras-test)
;;; read-aloud-extras-test.el ends here
