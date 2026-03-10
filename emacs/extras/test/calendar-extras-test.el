;;; calendar-extras-test.el --- Tests for calendar-extras -*- lexical-binding: t -*-

;; Tests for date range generation and date computation functions
;; in calendar-extras.el.

;;; Code:

(require 'ert)
(require 'calendar-extras)

;;;; Get dates in range

(ert-deftest calendar-extras-test-get-dates-in-range-single-day ()
  "Get-dates-in-range returns a single date for same start and end."
  (let ((result (calendar-extras-get-dates-in-range "2025-01-15" "2025-01-15")))
    (should (equal result '("2025-01-15")))))

(ert-deftest calendar-extras-test-get-dates-in-range-consecutive-days ()
  "Get-dates-in-range returns consecutive dates."
  (let ((result (calendar-extras-get-dates-in-range "2025-03-01" "2025-03-03")))
    (should (equal result '("2025-03-01" "2025-03-02" "2025-03-03")))))

(ert-deftest calendar-extras-test-get-dates-in-range-month-boundary ()
  "Get-dates-in-range spans month boundaries correctly."
  (let ((result (calendar-extras-get-dates-in-range "2025-01-30" "2025-02-02")))
    (should (equal result '("2025-01-30" "2025-01-31" "2025-02-01" "2025-02-02")))))

(ert-deftest calendar-extras-test-get-dates-in-range-year-boundary ()
  "Get-dates-in-range spans year boundaries correctly."
  (let ((result (calendar-extras-get-dates-in-range "2024-12-30" "2025-01-02")))
    (should (equal result '("2024-12-30" "2024-12-31" "2025-01-01" "2025-01-02")))))

(ert-deftest calendar-extras-test-get-dates-in-range-leap-year ()
  "Get-dates-in-range handles leap year February correctly."
  (let ((result (calendar-extras-get-dates-in-range "2024-02-28" "2024-03-01")))
    (should (equal result '("2024-02-28" "2024-02-29" "2024-03-01")))))

(ert-deftest calendar-extras-test-get-dates-in-range-non-leap-year ()
  "Get-dates-in-range handles non-leap year February correctly."
  (let ((result (calendar-extras-get-dates-in-range "2025-02-27" "2025-03-01")))
    (should (equal result '("2025-02-27" "2025-02-28" "2025-03-01")))))

(ert-deftest calendar-extras-test-get-dates-in-range-week ()
  "Get-dates-in-range returns exactly 7 days for a week span."
  (let ((result (calendar-extras-get-dates-in-range "2025-06-01" "2025-06-07")))
    (should (= (length result) 7))
    (should (equal (car result) "2025-06-01"))
    (should (equal (car (last result)) "2025-06-07"))))

(provide 'calendar-extras-test)
;;; calendar-extras-test.el ends here
