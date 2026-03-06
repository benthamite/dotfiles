;;; anki-editor-extras-test.el --- Tests for anki-editor-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in anki-editor-extras.el.

;;; Code:

(require 'ert)
(require 'anki-editor-extras)

;;;; Ankify film directors

(ert-deftest anki-editor-extras-test-ankify-film-directors-single ()
  "Return a single director's last name."
  (should (equal (anki-editor-extras--ankify-film-directors "Kubrick, Stanley")
                 "Kubrick")))

(ert-deftest anki-editor-extras-test-ankify-film-directors-two ()
  "Join two directors with ampersand."
  (should (equal (anki-editor-extras--ankify-film-directors
                  "Coen, Joel; Coen, Ethan")
                 "Coen & Coen")))

(ert-deftest anki-editor-extras-test-ankify-film-directors-three ()
  "Three directors: comma-separated with ampersand before last."
  (should (equal (anki-editor-extras--ankify-film-directors
                  "Spielberg, Steven; Lucas, George; Coppola, Francis")
                 "Spielberg, Lucas & Coppola")))

(ert-deftest anki-editor-extras-test-ankify-film-directors-four ()
  "Four directors: commas for all but last, ampersand before last."
  (should (equal (anki-editor-extras--ankify-film-directors
                  "A, X; B, Y; C, Z; D, W")
                 "A, B, C & D")))

(ert-deftest anki-editor-extras-test-ankify-film-directors-nil ()
  "Return nil for nil input."
  (should (null (anki-editor-extras--ankify-film-directors nil))))

(ert-deftest anki-editor-extras-test-ankify-film-directors-empty ()
  "Return nil for empty string."
  (should (null (anki-editor-extras--ankify-film-directors ""))))

(ert-deftest anki-editor-extras-test-ankify-film-directors-whitespace ()
  "Handle extra whitespace around names."
  (should (equal (anki-editor-extras--ankify-film-directors
                  "  Kubrick , Stanley ;  Tarkovsky , Andrei  ")
                 "Kubrick & Tarkovsky")))

(ert-deftest anki-editor-extras-test-ankify-film-directors-no-comma ()
  "When there is no comma, the entire name is treated as last name."
  (should (equal (anki-editor-extras--ankify-film-directors "Kubrick")
                 "Kubrick")))

;;;; Missing fields error predicate

(ert-deftest anki-editor-extras-test-missing-fields-error-p-match ()
  "Return non-nil for the expected error message."
  (should (anki-editor-extras--missing-fields-error-p
           '(user-error "Cannot map note fields: more than two fields missing"))))

(ert-deftest anki-editor-extras-test-missing-fields-error-p-embedded ()
  "Return non-nil when the message contains the expected substring."
  (should (anki-editor-extras--missing-fields-error-p
           '(user-error "Error: Cannot map note fields: more than two fields missing for Basic"))))

(ert-deftest anki-editor-extras-test-missing-fields-error-p-no-match ()
  "Return nil for an unrelated error message."
  (should-not (anki-editor-extras--missing-fields-error-p
               '(user-error "Some other error"))))

(ert-deftest anki-editor-extras-test-missing-fields-error-p-non-string ()
  "Return nil when the error data is not a string."
  (should-not (anki-editor-extras--missing-fields-error-p
               '(user-error 42))))

(ert-deftest anki-editor-extras-test-missing-fields-error-p-nil-message ()
  "Return nil when the error message is nil."
  (should-not (anki-editor-extras--missing-fields-error-p
               '(user-error nil))))

(provide 'anki-editor-extras-test)
;;; anki-editor-extras-test.el ends here
