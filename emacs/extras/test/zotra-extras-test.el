;;; zotra-extras-test.el --- Tests for zotra-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in zotra-extras.el, primarily the
;; octal sequence replacement logic.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'zotra-extras)

;;;; IMDb fallback

(ert-deftest zotra-extras-test-imdb-id-from-url ()
  "Extract IMDb title IDs from title URLs."
  (should
   (equal (zotra-extras--imdb-id-from-url
           "https://www.imdb.com/title/tt0073679/?ref_=fn_t_1")
          "tt0073679")))

(ert-deftest zotra-extras-test-imdb-id-from-url-rejects-non-title-url ()
  "Return nil when a URL has no IMDb title ID."
  (should-not
   (zotra-extras--imdb-id-from-url "https://www.imdb.com/find/?q=carrie")))

(ert-deftest zotra-extras-test-omdb-item-to-biblatex ()
  "Convert OMDb movie metadata into a populated BibLaTeX movie entry."
  (let* ((item '((Title . "Carrie")
                 (Year . "1976")
                 (Released . "03 Nov 1976")
                 (Runtime . "98 min")
                 (Genre . "Horror, Mystery")
                 (Director . "Brian De Palma")
                 (Writer . "Stephen King, Lawrence D. Cohen")
                 (Plot . "Carrie White, a shy teenager, discovers her powers.")
                 (Language . "English")
                 (Country . "United States")
                 (imdbID . "tt0073679")))
         (entry (zotra-extras--omdb-item-to-biblatex
                 item "https://www.imdb.com/title/tt0073679/?ref_=fn_t_1")))
    (should (string-match-p "@movie{imdb-tt0073679," entry))
    (should (string-match-p "title = {Carrie}" entry))
    (should (string-match-p "date = {1976}" entry))
    (should (string-match-p "director = {Brian De Palma}" entry))
    (should (string-match-p "url = {https://www.imdb.com/title/tt0073679/}" entry))
    (should (string-match-p "keywords = {Horror, Mystery}" entry))))

(ert-deftest zotra-extras-test-add-entry-uses-omdb-for-imdb-url ()
  "Add IMDb URLs through the OMDb fallback instead of `zotra-add-entry'."
  (let ((file (make-temp-file "zotra-imdb-" nil ".bib"))
        (zotra-extras-use-mullvad-p nil)
        (zotra-after-get-bibtex-entry-hook nil)
        (item '((Title . "Carrie")
                (Year . "1976")
                (Director . "Brian De Palma")
                (Writer . "Hervé Villeré")
                (Genre . "Horror, Mystery")
                (Plot . "Carrie White, a shy teenager, discovers her powers.")
                (imdbID . "tt0073679"))))
    (unwind-protect
        (cl-letf (((symbol-function 'zotra-extras--fetch-omdb-item)
                   (lambda (imdb-id)
                     (should (equal imdb-id "tt0073679"))
                     item))
                  ((symbol-function 'zotra-add-entry)
                   (lambda (&rest _)
                     (ert-fail "IMDb URLs should not use zotra-add-entry"))))
          (zotra-extras-add-entry
           "https://www.imdb.com/title/tt0073679/?ref_=fn_t_1" nil file t)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((contents (buffer-string)))
              (should (string-match-p "@movie{imdb-tt0073679," contents))
              (should (string-match-p "title = {Carrie}" contents))
              (should-not (string-match-p "@online" contents))
              (should (equal zotra-extras-most-recent-bibkey
                             "imdb-tt0073679")))))
      (when (file-exists-p file)
        (delete-file file)))))

;;;; fix-octal-sequences

(ert-deftest zotra-extras-test-fix-octal-sequences-latin-accents ()
  "Replace octal sequences for common Latin accented characters."
  (with-temp-buffer
    ;; \303\251 is the octal/UTF-8 byte sequence for \u00e9 (e with acute).
    ;; `unibyte-string' produces raw bytes that, once inserted into a
    ;; multibyte buffer, appear as the two-character sequence that
    ;; `search-forward' in `zotra-extras-fix-octal-sequences' will find.
    (insert (unibyte-string #o303 #o251))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00e9"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-n-tilde ()
  "Replace octal sequences for n with tilde (\u00f1)."
  (with-temp-buffer
    (insert (unibyte-string #o303 #o261))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00f1"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-u-umlaut ()
  "Replace octal sequences for u with umlaut (\u00fc)."
  (with-temp-buffer
    (insert (unibyte-string #o303 #o274))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00fc"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-no-change ()
  "Leave text without octal sequences unchanged."
  (with-temp-buffer
    (insert "plain ASCII text with no octal sequences")
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "plain ASCII text with no octal sequences"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-mixed-content ()
  "Handle buffer with both octal sequences and normal text."
  (with-temp-buffer
    (insert "Author: Mu" (unibyte-string #o303 #o261) "oz")
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "Author: Mu\u00f1oz"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-empty-buffer ()
  "Handle empty buffer without error."
  (with-temp-buffer
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) ""))))

(ert-deftest zotra-extras-test-fix-octal-sequences-multiple-occurrences ()
  "Replace multiple octal sequences in the same buffer."
  (with-temp-buffer
    (insert (unibyte-string #o303 #o251) " and " (unibyte-string #o303 #o251))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00e9 and \u00e9"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-preserves-point ()
  "Point is preserved after fixing octal sequences (via save-excursion)."
  (with-temp-buffer
    (insert "before " (unibyte-string #o303 #o251) " after")
    (goto-char 4)
    (let ((pos (point)))
      (zotra-extras-fix-octal-sequences)
      (should (= (point) pos)))))

(provide 'zotra-extras-test)
;;; zotra-extras-test.el ends here
