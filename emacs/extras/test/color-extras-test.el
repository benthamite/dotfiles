;;; color-extras-test.el --- Tests for color-extras -*- lexical-binding: t -*-

;; Tests for color conversion, parsing, and formatting functions
;; in color-extras.el.

;;; Code:

(require 'ert)
(require 'color-extras)

;;;; RGB <> HEX conversion

(ert-deftest color-extras-test-rgb-to-hex-black ()
  "RGB-to-hex converts black (0,0,0) correctly."
  (should (equal (color-extras-rgb-to-hex 0 0 0) "#000000")))

(ert-deftest color-extras-test-rgb-to-hex-white ()
  "RGB-to-hex converts white (255,255,255) correctly."
  (should (equal (color-extras-rgb-to-hex 255 255 255) "#ffffff")))

(ert-deftest color-extras-test-rgb-to-hex-primary-colors ()
  "RGB-to-hex converts primary colors correctly."
  (should (equal (color-extras-rgb-to-hex 255 0 0) "#ff0000"))
  (should (equal (color-extras-rgb-to-hex 0 255 0) "#00ff00"))
  (should (equal (color-extras-rgb-to-hex 0 0 255) "#0000ff")))

(ert-deftest color-extras-test-rgb-to-hex-arbitrary ()
  "RGB-to-hex converts arbitrary values correctly."
  (should (equal (color-extras-rgb-to-hex 171 205 239) "#abcdef"))
  (should (equal (color-extras-rgb-to-hex 18 52 86) "#123456")))

(ert-deftest color-extras-test-hex-to-rgb-6-digit ()
  "Hex-to-rgb parses 6-digit hex correctly."
  (let ((rgb (color-extras-hex-to-rgb "#000000")))
    (should (= (nth 0 rgb) 0.0))
    (should (= (nth 1 rgb) 0.0))
    (should (= (nth 2 rgb) 0.0)))
  (let ((rgb (color-extras-hex-to-rgb "#ffffff")))
    (should (= (nth 0 rgb) 1.0))
    (should (= (nth 1 rgb) 1.0))
    (should (= (nth 2 rgb) 1.0)))
  (let ((rgb (color-extras-hex-to-rgb "#ff0000")))
    (should (= (nth 0 rgb) 1.0))
    (should (= (nth 1 rgb) 0.0))
    (should (= (nth 2 rgb) 0.0))))

(ert-deftest color-extras-test-hex-to-rgb-3-digit ()
  "Hex-to-rgb expands 3-digit shorthand correctly."
  (let ((rgb-short (color-extras-hex-to-rgb "#fff"))
        (rgb-long (color-extras-hex-to-rgb "#ffffff")))
    (should (equal rgb-short rgb-long)))
  (let ((rgb-short (color-extras-hex-to-rgb "#f00"))
        (rgb-long (color-extras-hex-to-rgb "#ff0000")))
    (should (equal rgb-short rgb-long))))

(ert-deftest color-extras-test-hex-to-rgb-invalid ()
  "Hex-to-rgb signals error for invalid input."
  (should-error (color-extras-hex-to-rgb "not-a-color"))
  (should-error (color-extras-hex-to-rgb "#12345")))

(ert-deftest color-extras-test-hex-rgb-round-trip ()
  "Hex-to-rgb and rgb-to-hex round-trip correctly."
  (let* ((original "#abcdef")
         (rgb (color-extras-hex-to-rgb original))
         (back (color-extras-rgb-to-hex
                (round (* (nth 0 rgb) 255))
                (round (* (nth 1 rgb) 255))
                (round (* (nth 2 rgb) 255)))))
    (should (equal back original))))

;;;; HSL rescaling

(ert-deftest color-extras-test-hsl-rescale-to-unit ()
  "HSL-rescale divides to unit range correctly."
  (let ((result (color-extras-hsl-rescale 180 50 50 #'/)))
    (should (= (nth 0 result) 0.5))
    (should (= (nth 1 result) 0.5))
    (should (= (nth 2 result) 0.5))))

(ert-deftest color-extras-test-hsl-rescale-from-unit ()
  "HSL-rescale multiplies from unit range correctly."
  (let ((result (color-extras-hsl-rescale 0.5 0.5 0.5 #'*)))
    (should (= (nth 0 result) 180.0))
    (should (= (nth 1 result) 50.0))
    (should (= (nth 2 result) 50.0))))

(ert-deftest color-extras-test-hsl-rescale-zeros ()
  "HSL-rescale handles zero values."
  (let ((result (color-extras-hsl-rescale 0 0 0 #'/)))
    (should (= (nth 0 result) 0.0))
    (should (= (nth 1 result) 0.0))
    (should (= (nth 2 result) 0.0))))

(ert-deftest color-extras-test-hsl-rescale-max-values ()
  "HSL-rescale handles maximum values."
  (let ((result (color-extras-hsl-rescale 360 100 100 #'/)))
    (should (= (nth 0 result) 1.0))
    (should (= (nth 1 result) 1.0))
    (should (= (nth 2 result) 1.0))))

(ert-deftest color-extras-test-hsl-maybe-rescale-nil ()
  "HSL-maybe-rescale returns values unchanged when rescale is nil."
  (let ((result (color-extras-hsl-maybe-rescale 0.5 0.5 0.5 nil #'*)))
    (should (equal result '(0.5 0.5 0.5)))))

(ert-deftest color-extras-test-hsl-maybe-rescale-non-nil ()
  "HSL-maybe-rescale rescales when rescale is non-nil."
  (let ((result (color-extras-hsl-maybe-rescale 180 50 50 t #'/)))
    (should (= (nth 0 result) 0.5))
    (should (= (nth 1 result) 0.5))
    (should (= (nth 2 result) 0.5))))

;;;; HSL formatting

(ert-deftest color-extras-test-hsl-to-string ()
  "HSL-to-string formats HSL values correctly."
  (should (equal (color-extras-hsl-to-string '(180 50 75)) "180, 50%, 75%"))
  (should (equal (color-extras-hsl-to-string '(0 0 0)) "0, 0%, 0%"))
  (should (equal (color-extras-hsl-to-string '(360 100 100)) "360, 100%, 100%")))

(ert-deftest color-extras-test-hsl-to-string-rounds ()
  "HSL-to-string rounds fractional values."
  (should (equal (color-extras-hsl-to-string '(179.6 49.5 75.4)) "180, 50%, 75%")))

(ert-deftest color-extras-test-format-hsl-string ()
  "Format-hsl returns string when string arg is non-nil."
  (should (equal (color-extras-format-hsl '(180 50 75) t) "180, 50%, 75%")))

(ert-deftest color-extras-test-format-hsl-list ()
  "Format-hsl returns list when string arg is nil."
  (should (equal (color-extras-format-hsl '(180 50 75) nil) '(180 50 75))))

;;;; HSL <> HEX conversion

(ert-deftest color-extras-test-hsl-to-hex-primary-red ()
  "HSL-to-hex converts pure red correctly."
  (should (equal (color-extras-hsl-to-hex 0 100 50 t) "#ff0000")))

(ert-deftest color-extras-test-hsl-to-hex-primary-green ()
  "HSL-to-hex converts pure green correctly."
  (should (equal (color-extras-hsl-to-hex 120 100 50 t) "#00ff00")))

(ert-deftest color-extras-test-hsl-to-hex-primary-blue ()
  "HSL-to-hex converts pure blue correctly."
  (should (equal (color-extras-hsl-to-hex 240 100 50 t) "#0000ff")))

(ert-deftest color-extras-test-hsl-to-hex-black ()
  "HSL-to-hex converts black (lightness 0) correctly."
  (should (equal (color-extras-hsl-to-hex 0 0 0 t) "#000000")))

(ert-deftest color-extras-test-hsl-to-hex-white ()
  "HSL-to-hex converts white (lightness 100) correctly."
  (should (equal (color-extras-hsl-to-hex 0 0 100 t) "#ffffff")))

(ert-deftest color-extras-test-hex-to-hsl-unscaled ()
  "Hex-to-hsl returns values in 0.0-1.0 range without rescale."
  (let ((hsl (color-extras-hex-to-hsl "#ff0000")))
    (should (= (nth 0 hsl) 0.0))     ; hue
    (should (= (nth 1 hsl) 1.0))     ; saturation
    (should (= (nth 2 hsl) 0.5))))   ; lightness

(ert-deftest color-extras-test-hex-to-hsl-rescaled ()
  "Hex-to-hsl returns human-readable values with rescale."
  (let ((hsl (color-extras-hex-to-hsl "#ff0000" t)))
    (should (= (nth 0 hsl) 0.0))       ; hue 0 degrees
    (should (= (nth 1 hsl) 100.0))     ; 100% saturation
    (should (= (nth 2 hsl) 50.0))))    ; 50% lightness

(ert-deftest color-extras-test-hsl-hex-round-trip ()
  "HSL-to-hex and hex-to-hsl round-trip for primary colors."
  ;; Primary colors round-trip exactly; arbitrary colors may lose
  ;; precision through floating-point HSL conversion.
  (dolist (hex '("#ff0000" "#00ff00" "#0000ff" "#000000" "#ffffff"))
    (let* ((hsl (color-extras-hex-to-hsl hex))
           (back (apply #'color-extras-hsl-to-hex hsl)))
      (should (equal back hex)))))

(ert-deftest color-extras-test-hsl-hex-round-trip-rescaled ()
  "HSL-to-hex and hex-to-hsl round-trip for primary colors with rescaling."
  (dolist (hex '("#ff0000" "#00ff00" "#0000ff" "#000000" "#ffffff"))
    (let* ((hsl (color-extras-hex-to-hsl hex t))
           (back (apply #'color-extras-hsl-to-hex (append hsl '(t)))))
      (should (equal back hex)))))

;;;; Regex patterns

(ert-deftest color-extras-test-hex-pattern-matches-6-digit ()
  "Hex pattern matches 6-digit hex colors."
  (should (string-match color-extras-hex-pattern "#abcdef"))
  (should (string-match color-extras-hex-pattern "#ABCDEF"))
  (should (string-match color-extras-hex-pattern "#123456"))
  (should (string-match color-extras-hex-pattern "abcdef")))

(ert-deftest color-extras-test-hex-pattern-rejects-invalid ()
  "Hex pattern rejects invalid color strings."
  (should-not (string-match color-extras-hex-pattern "#gghhii"))
  (should-not (string-match color-extras-hex-pattern "#12345"))
  (should-not (string-match color-extras-hex-pattern "xyz")))

(ert-deftest color-extras-test-hsl-pattern-matches-valid ()
  "HSL pattern matches valid HSL strings."
  (should (string-match color-extras-hsl-scaled-pattern "180, 50%, 75%"))
  (should (string-match color-extras-hsl-scaled-pattern "0, 0%, 0%"))
  (should (string-match color-extras-hsl-scaled-pattern "360, 100%, 100%"))
  (should (string-match color-extras-hsl-scaled-pattern "180, 50, 75")))

;;;; Parse color

(ert-deftest color-extras-test-parse-color-hex ()
  "Parse-color returns the hex string for hex input."
  (should (equal (color-extras-parse-color "#abcdef") "#abcdef")))

(ert-deftest color-extras-test-parse-color-hsl ()
  "Parse-color returns parsed HSL for HSL input."
  (let ((result (color-extras-parse-color "180, 50%, 75%")))
    (should (listp result))
    (should (= (nth 0 result) 180))
    (should (= (nth 1 result) 50))
    (should (= (nth 2 result) 75))))

(ert-deftest color-extras-test-parse-color-invalid ()
  "Parse-color signals error for invalid input."
  (should-error (color-extras-parse-color "not a color")))

(provide 'color-extras-test)
;;; color-extras-test.el ends here
