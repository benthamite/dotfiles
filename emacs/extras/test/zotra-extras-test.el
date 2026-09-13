;;; zotra-extras-test.el --- Tests for zotra-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in zotra-extras.el, primarily the
;; octal sequence replacement logic.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'zotra-extras)

;;;; Import identity

(require 'bibtex-completion)
(require 'org-ref-bibtex)
(require 'ebib-utils)

(ert-deftest zotra-extras-test-import-uniquifies-against-bibliographies ()
  "Preserve existing records and return the new entry's unique key."
  (let* ((file (make-temp-file "zotra-collision-" nil ".bib"
                               "@misc{SameKey, title={Existing}, file={existing.pdf}}\n"))
         (other (make-temp-file "zotra-other-" nil ".bib"
                                "@misc{SameKeyb, title={Other}}\n"))
         (bibtex-files (list file other))
         (zotra-extras-most-recent-bibkey "stale-key")
         (zotra-extras-most-recent-bibfile nil)
         (zotra-after-get-bibtex-entry-hook
          '(zotra-extras-after-add-process-bibtex))
         (org-ref-clean-bibtex-entry-hook '(orcb-key org-ref-sort-bibtex-entry))
         (kill-ring nil)
         (kill-ring-yank-pointer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'zotra-get-entry-1)
                   (lambda (&rest _)
                     "@article{raw, author={Lloyd, Harry}, title={New}, journal={Journal}, year={2025}}"))
                  ((symbol-function 'bibtex-generate-autokey) (lambda () "SameKey"))
                  ((symbol-function 'tlon-cleanup-eaf-replace-urls) #'ignore))
          (let ((key (zotra-extras-add-entry "10.123/example" nil file t)))
            (with-temp-buffer
              (insert-file-contents file)
              (should (= 1 (how-many "@misc{SameKey," (point-min) (point-max))))
              (should-not (string-match-p "@article{SameKey," (buffer-string)))
              (should (string-match-p "file={existing.pdf}" (buffer-string))))
            (should (equal key "SameKeyc"))
            (should (equal zotra-extras-most-recent-bibkey key))))
      (dolist (path (list file other))
        (when-let ((buffer (find-buffer-visiting path))) (kill-buffer buffer))
        (delete-file path)))))

(ert-deftest zotra-extras-test-cleanup-error-does-not-insert ()
  "A cleanup failure must leave the target unchanged and remain visible."
  (let* ((file (make-temp-file "zotra-failed-cleanup-" nil ".bib"))
         (bibtex-files (list file))
         (zotra-extras-most-recent-bibkey "previous")
         (zotra-after-get-bibtex-entry-hook
          (list (lambda () (error "Cleanup failed")))))
    (unwind-protect
        (cl-letf (((symbol-function 'zotra-get-entry-1)
                   (lambda (&rest _) "@misc{new, title={New}}")))
          (should-error (zotra-extras-add-entry "10.123/example" nil file t)
                        :type 'error)
          (should (equal zotra-extras-most-recent-bibkey "previous"))
          (should (zerop (file-attribute-size (file-attributes file)))))
      (when-let ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest zotra-extras-test-import-reserves-keys-within-result ()
  "Two fetched records sharing a generated key remain distinct."
  (let* ((file (make-temp-file "zotra-multiple-" nil ".bib"))
         (bibtex-files (list file))
         (zotra-after-get-bibtex-entry-hook '(zotra-extras-after-add-process-bibtex))
         (org-ref-clean-bibtex-entry-hook '(orcb-key org-ref-sort-bibtex-entry))
         (kill-ring nil)
         (kill-ring-yank-pointer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'zotra-get-entry-1)
                   (lambda (&rest _)
                     (concat "@misc{one, author={Author}, title={First}, year={2025}}\n"
                             "@misc{two, author={Author}, title={Second}, year={2025}}\n")))
                  ((symbol-function 'bibtex-generate-autokey) (lambda () "Shared"))
                  ((symbol-function 'tlon-cleanup-eaf-replace-urls) #'ignore))
          (should (equal (zotra-extras-add-entry "10.123/example" nil file t)
                         "Sharedb"))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "@misc{Shared," (buffer-string)))
            (should (string-match-p "@misc{Sharedb," (buffer-string)))))
      (when-let ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest zotra-extras-test-import-preserves-concurrent-buffer-edit ()
  "A target edit made during fetching must not be saved or overwritten."
  (let* ((file (make-temp-file "zotra-concurrent-" nil ".bib"))
         (bibtex-files (list file))
         (zotra-after-get-bibtex-entry-hook nil))
    (unwind-protect
        (cl-letf (((symbol-function 'zotra-get-entry-1)
                   (lambda (&rest _)
                     (with-current-buffer (find-file-noselect file)
                       (insert "% User edit\n"))
                     "@misc{new, title={New}}")))
          (should-error (zotra-extras-add-entry "10.123/example" nil file t)
                        :type 'user-error)
          (should (zerop (file-attribute-size (file-attributes file))))
          (with-current-buffer (find-buffer-visiting file)
            (should (equal (buffer-string) "% User edit\n"))
            (should (buffer-modified-p))))
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest zotra-extras-test-challenge-metadata-does-not-insert ()
  "Reject the observed HAL challenge without touching the bibliography."
  (let* ((original "@misc{existing,title={Existing}}\n")
         (file (make-temp-file "zotra-challenge-" nil ".bib" original))
         (bibtex-files (list file))
         (zotra-extras-most-recent-bibkey "previous")
         (response "@online{2026MakingSureYoure,title={Making sure you're not a bot!},url={https://theses.hal.science/tel-04576792v1},urldate={2026-09-13},timestamp={2026-09-13 17:02:47 (GMT)}}"))
    (unwind-protect
        (cl-letf (((symbol-function 'zotra-get-entry-1)
                   (lambda (&rest _) response))
                  ((symbol-function 'zotra-extras--process-biblatex-entry)
                   (lambda (_) (ert-fail "Challenge reached cleanup")))
                  ((symbol-function 'zotra-extras-open-in-ebib)
                   (lambda (_) (ert-fail "Challenge opened Ebib"))))
          (dolist (prefix '("" "@article{valid,author={Author},title={Valid}}\n"))
            (setq response (concat prefix response))
            (should-error
             (zotra-extras-add-entry "https://theses.hal.science/tel-04576792v1" nil file t)
             :type 'user-error))
          (should (equal zotra-extras-most-recent-bibkey "previous"))
          (should-not (find-buffer-visiting file))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) original))))
      (when-let ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest zotra-extras-test-challenge-metadata-allows-works-about-bots ()
  "Do not mistake ordinary works or attributed titles for challenges."
  (dolist (entry '("@online{a,title={Making sure you're not a bot!},author={Smith, Alex}}"
                   "@article{b,title={Making sure you're not a bot!}}"
                   "@online{c,title={Making sure you're not a bot! A study}}"
                   "@online{d,title={How bots work}}"))
    (should-not (zotra-extras--reject-challenge-metadata entry))))

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
        (bibtex-biblatex-entry-alist
         (cons '("Movie" "Film" (("title")) nil (("timestamp")))
               bibtex-biblatex-entry-alist))
        (zotra-extras-use-mullvad-p nil)
        (zotra-after-get-bibtex-entry-hook
         '(zotra-extras-test--add-fixed-timestamp))
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
                     (ert-fail "IMDb URLs should not use zotra-add-entry")))
                  ((symbol-function 'zotra-extras-test--add-fixed-timestamp)
                   (lambda ()
                     (forward-line 1)
                     (insert "  timestamp = {fixed timestamp},\n"))))
          (zotra-extras-add-entry
           "https://www.imdb.com/title/tt0073679/?ref_=fn_t_1" nil file t)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((contents (buffer-string)))
              (should (string-match-p "@movie{imdb-tt0073679," contents))
              (should (string-match-p "title = {Carrie}" contents))
              (should (string-match-p
                       "timestamp = {fixed timestamp}" contents))
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
