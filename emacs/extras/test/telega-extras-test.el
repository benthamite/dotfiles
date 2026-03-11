;;; telega-extras-test.el --- Tests for telega-extras -*- lexical-binding: t -*-

;; Tests for pure logic functions in telega-extras.el.
;; Cannot load telega-extras in CI (requires telega), so test pure
;; logic directly by defining equivalent functions inline.

;;; Code:

(require 'ert)

;;;; Dired attach function

;; Extracted from telega-extras-dired-attach-func for standalone testing.
(defun telega-extras-test--dired-attach-func (file)
  "Identify msg type for FILE.  Extracted from telega-extras.el for testing."
  (let ((file-ext (file-name-extension file)))
    (cond ((member file-ext '("mp3" "flac"))
           'telega-chatbuf-attach-audio)
          ((member file-ext '("mp4" "mkv"))
           'telega-chatbuf-attach-video)
          ((image-supported-file-p file)
           'telega-chatbuf-attach-photo)
          (t
           'telega-chatbuf-attach-file))))

(ert-deftest telega-extras-test-dired-attach-func-audio-mp3 ()
  "An mp3 file is classified as audio."
  (should (eq (telega-extras-test--dired-attach-func "song.mp3")
              'telega-chatbuf-attach-audio)))

(ert-deftest telega-extras-test-dired-attach-func-audio-flac ()
  "A flac file is classified as audio."
  (should (eq (telega-extras-test--dired-attach-func "track.flac")
              'telega-chatbuf-attach-audio)))

(ert-deftest telega-extras-test-dired-attach-func-video-mp4 ()
  "An mp4 file is classified as video."
  (should (eq (telega-extras-test--dired-attach-func "clip.mp4")
              'telega-chatbuf-attach-video)))

(ert-deftest telega-extras-test-dired-attach-func-video-mkv ()
  "An mkv file is classified as video."
  (should (eq (telega-extras-test--dired-attach-func "movie.mkv")
              'telega-chatbuf-attach-video)))

(ert-deftest telega-extras-test-dired-attach-func-photo-png ()
  "A png file is classified as photo."
  (should (eq (telega-extras-test--dired-attach-func "image.png")
              'telega-chatbuf-attach-photo)))

(ert-deftest telega-extras-test-dired-attach-func-photo-jpg ()
  "A jpg file is classified as photo."
  (should (eq (telega-extras-test--dired-attach-func "photo.jpg")
              'telega-chatbuf-attach-photo)))

(ert-deftest telega-extras-test-dired-attach-func-generic-txt ()
  "A txt file falls through to generic file attachment."
  (should (eq (telega-extras-test--dired-attach-func "readme.txt")
              'telega-chatbuf-attach-file)))

(ert-deftest telega-extras-test-dired-attach-func-generic-pdf ()
  "A pdf file falls through to generic file attachment."
  (should (eq (telega-extras-test--dired-attach-func "document.pdf")
              'telega-chatbuf-attach-file)))

(ert-deftest telega-extras-test-dired-attach-func-no-extension ()
  "A file with no extension falls through to generic file attachment."
  (should (eq (telega-extras-test--dired-attach-func "Makefile")
              'telega-chatbuf-attach-file)))

;;;; Cancel timer when active

;; Extracted from telega-extras-cancel-timer-when-active for standalone testing.
(defun telega-extras-test--cancel-timer-when-active (timer-name)
  "Cancel TIMER-NAME when active."
  (let ((timer (symbol-value timer-name)))
    (when timer
      (cancel-timer timer)
      (set timer-name nil))))

(ert-deftest telega-extras-test-cancel-timer-when-active-cancels ()
  "An active timer is cancelled and its variable set to nil."
  (defvar telega-extras-test--dummy-timer nil)
  (setq telega-extras-test--dummy-timer (run-with-timer 9999 nil #'ignore))
  (should (timerp telega-extras-test--dummy-timer))
  (telega-extras-test--cancel-timer-when-active 'telega-extras-test--dummy-timer)
  (should (null telega-extras-test--dummy-timer)))

(ert-deftest telega-extras-test-cancel-timer-when-active-nil ()
  "Cancelling a nil timer does not signal an error."
  (defvar telega-extras-test--nil-timer nil)
  (setq telega-extras-test--nil-timer nil)
  (telega-extras-test--cancel-timer-when-active 'telega-extras-test--nil-timer)
  (should (null telega-extras-test--nil-timer)))

(provide 'telega-extras-test)
;;; telega-extras-test.el ends here
