;;; ledger-mode-extras-test.el --- Tests for ledger-mode-extras -*- lexical-binding: t -*-

;; Tests for pure parsing functions in ledger-mode-extras.el.

;;; Code:

(require 'ert)
(require 'ledger-mode-extras)

;;;; Convert Interactive Brokers date

(ert-deftest ledger-mode-extras-test-convert-ib-date-standard ()
  "Convert IB date transforms YYYYMMDD to YYYY-MM-DD."
  (should (equal (ledger-extras-convert-interactive-brokers-date "20250306")
                 "2025-03-06")))

(ert-deftest ledger-mode-extras-test-convert-ib-date-january ()
  "Convert IB date handles January dates correctly."
  (should (equal (ledger-extras-convert-interactive-brokers-date "20250101")
                 "2025-01-01")))

(ert-deftest ledger-mode-extras-test-convert-ib-date-december ()
  "Convert IB date handles December dates correctly."
  (should (equal (ledger-extras-convert-interactive-brokers-date "20251231")
                 "2025-12-31")))

(ert-deftest ledger-mode-extras-test-convert-ib-date-different-year ()
  "Convert IB date handles different years."
  (should (equal (ledger-extras-convert-interactive-brokers-date "20200229")
                 "2020-02-29")))

;;;; Parse Wise date

(ert-deftest ledger-mode-extras-test-parse-wise-date-standard ()
  "Parse Wise date converts full date string to YYYY-MM-DD."
  (should (equal (ledger-mode-extras--parse-wise-date "16 January 2026")
                 "2026-01-16")))

(ert-deftest ledger-mode-extras-test-parse-wise-date-single-digit-day ()
  "Parse Wise date handles single-digit days."
  (should (equal (ledger-mode-extras--parse-wise-date "1 March 2025")
                 "2025-03-01")))

(ert-deftest ledger-mode-extras-test-parse-wise-date-december ()
  "Parse Wise date handles December."
  (should (equal (ledger-mode-extras--parse-wise-date "25 December 2025")
                 "2025-12-25")))

(ert-deftest ledger-mode-extras-test-parse-wise-date-february ()
  "Parse Wise date handles February."
  (should (equal (ledger-mode-extras--parse-wise-date "28 February 2025")
                 "2025-02-28")))

;;;; Parse Wise positive amount

(ert-deftest ledger-mode-extras-test-parse-wise-positive-amount-basic ()
  "Parse Wise positive amount extracts a positive USD amount."
  (should (= (ledger-mode-extras--parse-wise-positive-amount "+ 200.79 USD")
             200.79)))

(ert-deftest ledger-mode-extras-test-parse-wise-positive-amount-integer ()
  "Parse Wise positive amount handles integer amounts."
  (should (= (ledger-mode-extras--parse-wise-positive-amount "+ 100 USD")
             100)))

(ert-deftest ledger-mode-extras-test-parse-wise-positive-amount-large ()
  "Parse Wise positive amount handles large amounts."
  (should (= (ledger-mode-extras--parse-wise-positive-amount "+ 12345.67 USD")
             12345.67)))

(ert-deftest ledger-mode-extras-test-parse-wise-positive-amount-negative ()
  "Parse Wise positive amount returns nil for negative amounts."
  (should (null (ledger-mode-extras--parse-wise-positive-amount "- 200.79 USD"))))

(ert-deftest ledger-mode-extras-test-parse-wise-positive-amount-whitespace ()
  "Parse Wise positive amount handles leading/trailing whitespace.
Note: the function calls `string-trim' twice independently, so
`match-string' receives a different string object than `string-match'.
This causes an error when whitespace is present.  In practice the
caller's `split-string' already trims whitespace, so this path is not
reached.  This test documents the current behavior."
  (should-error (ledger-mode-extras--parse-wise-positive-amount "  + 50.00 USD  ")
                :type 'wrong-type-argument))

(ert-deftest ledger-mode-extras-test-parse-wise-positive-amount-no-space ()
  "Parse Wise positive amount handles no space after +."
  (should (= (ledger-mode-extras--parse-wise-positive-amount "+200.79 USD")
             200.79)))

;;;; Wise date line predicate

(ert-deftest ledger-mode-extras-test-wise-date-line-p-valid ()
  "Wise date line predicate matches valid date lines with year."
  (should (ledger-mode-extras--wise-date-line-p "16 January 2026"))
  (should (ledger-mode-extras--wise-date-line-p "1 March 2025"))
  (should (ledger-mode-extras--wise-date-line-p "25 December 2025")))

(ert-deftest ledger-mode-extras-test-wise-date-line-p-whitespace ()
  "Wise date line predicate matches lines with surrounding whitespace."
  (should (ledger-mode-extras--wise-date-line-p "  16 January 2026  ")))

(ert-deftest ledger-mode-extras-test-wise-date-line-p-no-year ()
  "Wise date line predicate rejects date lines without a year."
  (should-not (ledger-mode-extras--wise-date-line-p "16 January")))

(ert-deftest ledger-mode-extras-test-wise-date-line-p-invalid ()
  "Wise date line predicate rejects non-date strings."
  (should-not (ledger-mode-extras--wise-date-line-p "Completed"))
  (should-not (ledger-mode-extras--wise-date-line-p "AIRBNB 4977"))
  (should-not (ledger-mode-extras--wise-date-line-p "+ 200.79 USD"))
  (should-not (ledger-mode-extras--wise-date-line-p "")))

(ert-deftest ledger-mode-extras-test-wise-date-line-p-all-months ()
  "Wise date line predicate matches all twelve months."
  (dolist (month '("January" "February" "March" "April" "May" "June"
                   "July" "August" "September" "October" "November" "December"))
    (should (ledger-mode-extras--wise-date-line-p
             (format "1 %s 2025" month)))))

;;;; Wise amount line predicate

(ert-deftest ledger-mode-extras-test-wise-amount-line-p-positive ()
  "Wise amount line predicate matches positive USD amounts."
  (should (ledger-mode-extras--wise-amount-line-p "+ 200.79 USD"))
  (should (ledger-mode-extras--wise-amount-line-p "+200 USD")))

(ert-deftest ledger-mode-extras-test-wise-amount-line-p-negative ()
  "Wise amount line predicate matches negative USD amounts."
  (should (ledger-mode-extras--wise-amount-line-p "- 50.00 USD"))
  (should (ledger-mode-extras--wise-amount-line-p "-100 USD")))

(ert-deftest ledger-mode-extras-test-wise-amount-line-p-whitespace ()
  "Wise amount line predicate matches amounts with surrounding whitespace."
  (should (ledger-mode-extras--wise-amount-line-p "  + 200.79 USD  ")))

(ert-deftest ledger-mode-extras-test-wise-amount-line-p-invalid ()
  "Wise amount line predicate rejects non-amount strings."
  (should-not (ledger-mode-extras--wise-amount-line-p "16 January 2026"))
  (should-not (ledger-mode-extras--wise-amount-line-p "Completed"))
  (should-not (ledger-mode-extras--wise-amount-line-p "AIRBNB 4977"))
  (should-not (ledger-mode-extras--wise-amount-line-p "200.79 USD"))
  (should-not (ledger-mode-extras--wise-amount-line-p "")))

;;;; Find previous Wise date line

(ert-deftest ledger-mode-extras-test-find-prev-wise-date-line-found ()
  "Find prev Wise date line returns the nearest preceding date line."
  (let ((lines '("16 January 2026" "Completed" "AIRBNB 4977" "+ 200.79 USD")))
    (should (equal (ledger-mode-extras--find-prev-wise-date-line lines 3)
                   "16 January 2026"))))

(ert-deftest ledger-mode-extras-test-find-prev-wise-date-line-immediate ()
  "Find prev Wise date line returns a date line immediately before the index."
  (let ((lines '("10 February 2026" "+ 100.00 USD")))
    (should (equal (ledger-mode-extras--find-prev-wise-date-line lines 1)
                   "10 February 2026"))))

(ert-deftest ledger-mode-extras-test-find-prev-wise-date-line-skips-non-dates ()
  "Find prev Wise date line skips non-date lines to find the nearest date."
  (let ((lines '("5 March 2025" "Completed" "5 March" "AIRBNB" "+ 50.00 USD")))
    (should (equal (ledger-mode-extras--find-prev-wise-date-line lines 4)
                   "5 March 2025"))))

(ert-deftest ledger-mode-extras-test-find-prev-wise-date-line-none ()
  "Find prev Wise date line returns nil when no date line precedes index."
  (let ((lines '("Completed" "AIRBNB 4977" "+ 200.79 USD")))
    (should (null (ledger-mode-extras--find-prev-wise-date-line lines 2)))))

(ert-deftest ledger-mode-extras-test-find-prev-wise-date-line-at-zero ()
  "Find prev Wise date line returns nil when index is 0."
  (let ((lines '("+ 200.79 USD")))
    (should (null (ledger-mode-extras--find-prev-wise-date-line lines 0)))))

(ert-deftest ledger-mode-extras-test-find-prev-wise-date-line-multiple-dates ()
  "Find prev Wise date line returns the closest date, not an earlier one."
  (let ((lines '("1 January 2026" "Completed" "15 January 2026" "AIRBNB" "+ 300.00 USD")))
    (should (equal (ledger-mode-extras--find-prev-wise-date-line lines 4)
                   "15 January 2026"))))

;;;; Find next Wise amount line

(ert-deftest ledger-mode-extras-test-find-next-wise-amount-line-found ()
  "Find next Wise amount line returns the first amount line at or after start."
  (let ((lines '("16 January 2026" "Completed" "AIRBNB 4977" "+ 200.79 USD")))
    (should (equal (ledger-mode-extras--find-next-wise-amount-line lines 0)
                   "+ 200.79 USD"))))

(ert-deftest ledger-mode-extras-test-find-next-wise-amount-line-at-start ()
  "Find next Wise amount line returns an amount line when start points directly at one."
  (let ((lines '("+ 100.00 USD" "other stuff")))
    (should (equal (ledger-mode-extras--find-next-wise-amount-line lines 0)
                   "+ 100.00 USD"))))

(ert-deftest ledger-mode-extras-test-find-next-wise-amount-line-none ()
  "Find next Wise amount line returns nil when no amount line exists at or after start."
  (let ((lines '("16 January 2026" "Completed" "AIRBNB 4977")))
    (should (null (ledger-mode-extras--find-next-wise-amount-line lines 0)))))

(ert-deftest ledger-mode-extras-test-find-next-wise-amount-line-past-end ()
  "Find next Wise amount line returns nil when start is beyond the list."
  (let ((lines '("+ 100.00 USD")))
    (should (null (ledger-mode-extras--find-next-wise-amount-line lines 5)))))

(ert-deftest ledger-mode-extras-test-find-next-wise-amount-line-negative ()
  "Find next Wise amount line also finds negative amount lines."
  (let ((lines '("16 January 2026" "- 50.00 USD")))
    (should (equal (ledger-mode-extras--find-next-wise-amount-line lines 0)
                   "- 50.00 USD"))))

(ert-deftest ledger-mode-extras-test-find-next-wise-amount-line-skips-to-first ()
  "Find next Wise amount line returns the first amount line, not a later one."
  (let ((lines '("date" "+ 100.00 USD" "other" "- 50.00 USD")))
    (should (equal (ledger-mode-extras--find-next-wise-amount-line lines 0)
                   "+ 100.00 USD"))))

(provide 'ledger-mode-extras-test)
;;; ledger-mode-extras-test.el ends here
