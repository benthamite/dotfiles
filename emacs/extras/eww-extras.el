;;; eww-extras.el --- Extensions for eww -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/eww-extras.el
;; Version: 0.2
;; Package-Requires: ((el-patch "1.1") (paths "0.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for `eww'.

;;; Code:

(require 'el-patch)
(require 'eww)
(require 'paths)

;;;; Patches

;; Upstream bug: `eww-score-readability' does not exclude `style' tags, so CSS
;; text inside <style> elements gets counted as readable content.  This causes
;; CSS-heavy nodes (e.g. Wikipedia's `div.navbox-styles') to win the readability
;; contest, producing an empty render.
(el-patch-defun eww-score-readability (node)
  (let ((score -1))
    (cond
     ((memq (dom-tag node) (el-patch-swap '(script head comment)
					  '(script head comment style)))
      (setq score -2))
     ((eq (dom-tag node) 'meta)
      (setq score -1))
     ((eq (dom-tag node) 'img)
      (setq score 2))
     ((eq (dom-tag node) 'a)
      (setq score (- (length (split-string (dom-text node))))))
     (t
      (dolist (elem (dom-children node))
	(cond
         ((stringp elem)
          (setq score (+ score (length (split-string elem)))))
         ((consp elem)
	  (setq score (+ score
			 (or (cdr (assoc :eww-readability-score (cdr elem)))
			     (eww-score-readability elem)))))))))
    ;; Cache the score of the node to avoid recomputing all the time.
    (dom-set-attribute node :eww-readability-score score)
    score))

;; Upstream `eww-highest-readability' replaces the current best node with a
;; deeper child whenever the child scores higher and has more than 100 words.
;; On pages where navigation-heavy elements (navboxes, TOC, categories) drag
;; down the full content node's score, a small subsection—such as the lead
;; paragraph—can win the readability contest.  We raise the threshold to
;; max(100, 1/3 of the current best node's word count), so a child must
;; contain a substantial fraction of the parent's text to replace it.
(el-patch-defun eww-highest-readability (node)
  (let ((result node)
	(el-patch-add (result-words (length (split-string (dom-texts node)))))
	highest)
    (dolist (elem (dom-non-text-children node))
      (when (> (or (dom-attr
		    (setq highest (eww-highest-readability elem))
		    :eww-readability-score)
		   most-negative-fixnum)
	       (or (dom-attr result :eww-readability-score)
		   most-negative-fixnum))
        ;; We set a lower bound to how long we accept that the
        ;; readable portion of the page is going to be.
        (when (> (length (split-string (dom-texts highest)))
		 (el-patch-swap 100
				(max 100 (/ result-words 3))))
	  (el-patch-add
	   (setq result-words (length (split-string (dom-texts highest)))))
          (setq result highest))))
    result))

;;;; Variables

;;;;; Subtitles

(defconst eww-extras-download-subtitles
  "yt-dlp --skip-download --write-auto-subs '%s'"
  "Command to download subtitles from a URL in `srt' format.
The placeholders are replaced by the URL.")

;;;; User options

(defgroup eww-extras ()
  "Extensions for `eww'."
  :group 'eww)

(defcustom eww-extras-readable-exceptions '()
  "List of URLs for which `eww-readable' should not be used by default."
  :type '(repeat string)
  :group 'eww-extras)

(defcustom eww-extras-readable-exceptions-file
  (file-name-concat paths-dir-dotemacs "etc/eww-readable-exceptions.txt")
  "File containing the URLs for which `eww-readable' should not be used by default."
  :type 'file
  :group 'eww-extras)

;;;; Variables

(defconst eww-extras-url-renderer-program
  (file-name-concat paths-dir-dotemacs
                    "extras/scripts/eww-extras-renderer/run.sh")
  "Program used to render URLs to PDF or HTML through headless Chrome.")

;;;; Functions

;;;;; Core

;;;###autoload
(defun eww-extras-browse-file (&optional file)
  "Browse File in `eww'.
If FILE is nil, use the file at point, the file visited by the current buffer,
or prompt the user for a file."
  (interactive)
  (let ((file (or file
		  (thing-at-point 'filename)
		  (buffer-file-name)
		  (read-file-name "File: " nil nil t))))
    (if (file-exists-p file)
	(eww-open-file file)
      (user-error "No file found at point"))))

(defvar ebib--cur-db)
(autoload 'simple-extras-get-url "simple-extras")
(autoload 'ebib-db-get-filename "ebib-db")
(autoload 'org-web-tools-extras-org-title-for-url "org-web-tools-extras")
(declare-function simple-extras-slugify "simple-extras")
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function ebib-extras-get-field "ebib-extras")
;;;###autoload
(defun eww-extras-url-to-file (type &optional url callback key)
  "Generate file of TYPE for URL and run CALLBACK function.
CALLBACK is a function called when the process concludes.  The
function takes two arguments: the file to attach and the BibTeX
key of the entry from which this function was called, if any.
KEY is an optional BibTeX key; when non-nil it is used as the
filename stem and passed to the callback sentinel, bypassing the
buffer-derived lookup."
  (let* ((url (simple-extras-get-url url))
         (bibtex-key (or key
                         (pcase major-mode
                           ('bibtex-mode (bibtex-extras-get-key))
                           ((or 'ebib-entry-mode 'ebib-index-mode)
                            (ebib-extras-get-field "=key=")))))
         (title (pcase major-mode
                  ((or 'bibtex-mode 'ebib-entry-mode 'ebib-index-mode) bibtex-key)
                  (_ (pcase type
                       ("pdf" (buffer-name))
                       ("html" (simple-extras-slugify (org-web-tools-extras-org-title-for-url url)))))))
         (file-name (file-name-with-extension title type))
         (output-file (file-name-concat paths-dir-downloads file-name))
	 (process-buffer
	  (generate-new-buffer (format " *eww-extras download %s*" type)))
	 (process (make-process
		   :name (format "url-to-%s" type)
		   :buffer process-buffer
		   :stderr process-buffer
		   :command (eww-extras-url-to-file-make-command url output-file type))))
    (message "Getting %s file…" type)
    (set-process-sentinel process
			  (eww-extras-url-to-file-sentinel
			   callback output-file bibtex-key process-buffer))))

(defun eww-extras-url-to-file-sentinel
    (callback output-file bibtex-key &optional process-buffer)
  "Create a process sentinel for URL-to-file operations.

CALLBACK is a function to be called upon successful file download.
OUTPUT-FILE is the path of the file being downloaded.
BIBTEX-KEY is the BibTeX key associated with the download, if any.
PROCESS-BUFFER is the private diagnostics buffer for this render.

The returned sentinel function takes two arguments:
PROC, the process object, and EVENT, a string describing the process status."
  (lambda (proc event)
    (let* ((exit-status (process-exit-status proc))
           (file-ok (and (file-exists-p output-file)
                         (file-regular-p output-file)
                         (> (file-attribute-size (file-attributes output-file)) 0)))
           (diagnostic
            (when (buffer-live-p process-buffer)
              (with-current-buffer process-buffer
                (car (last (split-string (string-trim (buffer-string))
                                         "\n" t)))))))
      (unwind-protect
          (cond
           ((and (eq exit-status 0) file-ok)
            (eww-extras-run-callback callback output-file bibtex-key))
           ((eq exit-status 0)
            (user-error "Process exited successfully but %s is empty or missing"
                        (file-name-nondirectory output-file)))
           (t
            (user-error "Could not get file (status %s): %s"
                        exit-status (or diagnostic event))))
        (when (buffer-live-p process-buffer)
          (kill-buffer process-buffer))))))

(defun eww-extras-url-to-file-make-command (url output-file type)
  "Make command to generate OUTPUT-FILE of TYPE from URL."
  (unless (member type '("pdf" "html"))
    (user-error "Invalid type: %s" type))
  (list eww-extras-url-renderer-program
        "render"
        "--type" type
        "--url" url
        "--output" output-file
        "--chrome-program" browse-url-chrome-program))

(defun eww-extras-run-callback (callback file key)
  "When CALLBACK is non-nil, run it with FILE and KEY as arguments.
FILE is the file to attach and KEY is the BibTeX key of the associated entry."
  (when callback
    (funcall callback file key)))

;;;;; URL to HTML, PDF

(defun eww-extras-url-to-html (&optional url callback)
  "Generate HTML of URL, then run CALLBACK function."
  (interactive)
  (eww-extras-url-to-file "html" url callback))

(defun eww-extras-url-to-pdf (&optional url callback)
  "Generate PDF of URL, then run CALLBACK function."
  (interactive)
  (eww-extras-url-to-file "pdf" url callback))

;;;;; readable

(defun eww-extras-readable-autoview ()
  "Display the \"readable\" parts of the current web page by default.
The exceptions are listed in `eww-extras-readable-exceptions'."
  (let ((current-url (eww-current-url)))
    ;; Only proceed if current-url is a valid string
    (when (stringp current-url)
      (let ((exception
             (catch 'exception
               (dolist (url eww-extras-readable-exceptions)
                 (when (string-match-p url current-url)
                   (throw 'exception t))))))
        (unless (or exception
                    ;; if `:source' is nil, `eww-readable' will throw an error
                    (not (plist-get eww-data :source)))
          (eww-readable)
          ;; The readability heuristic can pick a non-visible node (e.g. a div
          ;; containing only <style> elements), producing an empty buffer.  It
          ;; can also pick a node covering only a small fraction of the article
          ;; (e.g. the lead section of a Wikipedia page when navigation-heavy
          ;; elements drag down the full content node's score).  Fall back to
          ;; the full page in either case.
          (when (let ((source-len (length (plist-get eww-data :source))))
                  (or (zerop (buffer-size))
                      (and (> source-len 0)
                           (< (/ (float (buffer-size)) source-len) 0.02))))
            (eww-readable -1)))))))

(add-hook 'eww-after-render-hook #'eww-extras-readable-autoview)

(autoload 'ffap-url-p "ffap")
(autoload 'browse-url-extras-write-url-to-file "browse-url-extras")
(defun eww-extras-add-domain-to-readable-exceptions ()
  "Prompt for a URL and add its domain to the list of `eww-readable' exceptions.
If buffer is visiting a URL or if there is a URL in the kill ring, use its
domain as the initial prompt input."
  (interactive)
  (let* ((url (or (eww-current-url) (ffap-url-p (current-kill 0))))
	 (domain (when url (url-domain (url-generic-parse-url url))))
	 (file eww-extras-readable-exceptions-file)
	 (selection (read-string (format "Add to `%s': " (file-name-nondirectory file)) domain)))
    (browse-url-extras-write-url-to-file selection file)
    (eww-extras-set-readable-exceptions-from-file)
    (eww-reload)))

(defun eww-extras-set-readable-exceptions-from-file ()
  "Set `eww-readable' exceptions from file of exception URLs."
  (when (file-exists-p eww-extras-readable-exceptions-file)
    (with-temp-buffer
      (insert-file-contents eww-extras-readable-exceptions-file)
      (setq eww-extras-readable-exceptions
	    (mapcar #'regexp-quote (split-string (buffer-string) "\n" t))))))

(eww-extras-set-readable-exceptions-from-file)

;;;;; Vimium-like navigation

;; The following four commands copied from
;; github.com/gopar/.emacs.d#eww
(defun eww-extras-edit-current-url (&optional arg)
  "Edit current URL or new search.
With prefix ARG is passed, open in new EWW buffer."
  (interactive)
  (let* ((url (eww-copy-page-url))
	 (uris (eww-suggested-uris)))
    (setq url (read-string "Edit URL or new search: " url 'eww-promt-history uris))
    (setq url (eww--dwim-expand-url url))
    (eww url (if arg 4 nil))))

(defun eww-extras-open-with-recent-kill-ring (&optional arg)
  "Open current EWW with most recent item in kill ring.
With prefix ARG is passed, open in new EWW buffer."
  (interactive "P")
  (if arg
      (with-current-buffer
	  (if (derived-mode-p 'eww-mode) (clone-buffer)
	    (generate-new-buffer "*eww*"))
	(eww-mode)
	(eww (current-kill 0)))
    (eww (current-kill 0))))

(defun eww-extras-go-up-url-hierarchy ()
  "Go up the URL hierarchy."
  (interactive)
  (let* ((url (url-generic-parse-url (eww-current-url)))
         (filepath (url-filename url))
         (paths (split-string filepath "/" t))
         (new-path (mapconcat #'identity (butlast paths 1) "/"))
         (new-url nil))
    (setq new-url (url-parse-make-urlobj
                   (url-type url)
                   (url-user url)
                   (url-password url)
                   (url-host url)
                   (url-port url)
                   new-path
                   (url-target url)
                   nil
                   (url-fullness url)))
    (eww-browse-url (url-recreate-url new-url))))

(defun eww-extras-go-to-root-url-hierarchy ()
  "Go to root of current URL hierarchy."
  (interactive)
  (let* ((url (url-generic-parse-url (eww-current-url)))
	 (new-url nil))
    (setq new-url (url-parse-make-urlobj
		   (url-type url)
		   (url-user url)
		   (url-password url)
		   (url-host url)
		   (url-port url)
		   ""
		   (url-target url)
		   nil
		   (url-fullness url)))
    (eww-browse-url (url-recreate-url new-url))))

;;;;; open with

;;;###autoload
(defun eww-extras-open-with-xwidget ()
  "Open URL in xwidget-webkit."
  (interactive)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not in eww mode"))
  (if-let ((url (eww-current-url)))
      (xwidget-webkit-browse-url url)
    (user-error "No URL at point")))

(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-uri "xwidget.c")
;;;###autoload
(defun eww-extras-open-with-eww ()
  "Open URL in `eww'."
  (interactive)
  (unless (derived-mode-p 'xwidget-webkit-mode)
    (user-error "Not in eww mode"))
  (if-let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
      (eww-browse-url url)
    (user-error "No URL at point")))

;;;;; Misc

(declare-function elfeed-tube-fetch "elfeed-tube")
(declare-function macos-open-app "macos")
(declare-function macos-app-is-open-p "macos")
;;;###autoload
(defun eww-extras-browse-youtube (url &optional player)
  "For YouTube URLs, show its transcript and open video with PLAYER.
If PLAYER is nil, default to `mpv'."
  (when (string-match "youtube.com" url)
    (let ((player (or player "mpv")))
      (kill-buffer)
      (unless (macos-app-is-open-p player)
	(macos-open-app player 'background))
      (elfeed-tube-fetch url))))

(declare-function zotra-extras-add-entry "zotra-extras")
(defun eww-extras-add-entry ()
  "Add current URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (zotra-extras-add-entry (plist-get eww-data :url))))

(provide 'eww-extras)

;;; eww-extras.el ends here
