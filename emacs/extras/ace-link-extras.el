;;; ace-link-extras.el --- Extensions for ace-link -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ace-link-extras.el
;; Version: 0.2
;; Package-Requires: ((ace-link "0.4.0"))

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

;; Extensions for `ace-link'.

;;; Code:

(require 'ace-link)

;;;; Functions

(defvar mm-text-html-renderer)
;;;###autoload
(defun ace-link-extras-mu4e ()
  "Open a visible link in an `mu4e-view-mode' buffer.
Unlike `ace-link-mu4e', this function is sensitive to the value of
`mm-text-html-renderer'.  Specifically, it will use `ace-link--w3m-collect' to
collect the URLs if its value is `w3m'.  Otherwise it will use the normal
`ace-link-mu4e'.

We do not use `ace-link-w3m' because we do not want to browse the URL with
`w3m', which we only use to render HTML email messages.  Instead, we rely on
`ace-link-extras-w3m-get-url' to get the URL and then open it with
`browse-url' (which will browse it with `eww', `chrome' or `firefox', as
specified by `browse-url-handlers')."
  (interactive)
  (if (eq mm-text-html-renderer 'w3m)
      (when-let* ((url (ace-link-extras-w3m-get-url)))
	(browse-url url))
    (ace-link-mu4e)))

(defun ace-link-extras-w3m-get-url ()
  "Return the selected URL in a `w3m-mode' buffer."
  (let ((pt (avy-with ace-link-w3m
              (avy-process
               (mapcar #'cdr (ace-link--w3m-collect))
               #'avy--overlay-pre))))
    (when pt
      (get-text-property (point) 'w3m-href-anchor))))

;;;###autoload
(defun ace-link-extras-eww-externally ()
  "Browse URL using `browse-url-secondary-browser-function'."
  (interactive)
  (ace-link-eww '(4)))

;;;###autoload
(defun ace-link-extras-eww-new-buffer ()
  "Browse URL in new buffer."
  (interactive)
  (ace-link-eww '(16)))

;;;###autoload
(defun ace-link-extras-org-roam ()
  "Open a visible link in an `org-roam-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-org
		      (avy-process
		       (mapcar #'cdr (ace-link--org-collect))
		       (avy--style-fn avy-style)))))
    (ace-link-extras--org-roam-action pt)))

(autoload 'org-roam-preview-visit "org-roam-mode")
(defun ace-link-extras--org-roam-action (pt)
  "Visit the link at PT in an `org-roam-mode' buffer."
  (when (numberp pt)
    (goto-char pt)
    (call-interactively #'org-roam-preview-visit)))

;;;;; Patched functions

;; Upstream `ace-link--mu4e-action' calls `mu4e~view-browse-url-from-binding',
;; which was renamed to `mu4e--view-browse-url-from-binding' in mu4e 1.12.
(declare-function shr-browse-url "shr")
(declare-function mu4e--view-browse-url-from-binding "ext:mu4e-view")

(defun ace-link--mu4e-action (pt)
  "Open link at PT in a `mu4e-view' buffer.
Replaces the upstream definition to use the current mu4e function names."
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (cond ((get-text-property (point) 'shr-url)
           (shr-browse-url))
          ((get-text-property (point) 'mu4e-url)
           (mu4e--view-browse-url-from-binding)))))

;;;;; Slack

;;;###autoload
(defun ace-link-extras-slack ()
  "Open a visible link or clickable element in a slack buffer.
Collects URLs, channel links, user mentions, thread status lines,
and message entries, then jumps to and activates the selected one."
  (interactive)
  (let ((pt (avy-with ace-link-extras-slack
              (avy-process
               (mapcar #'cdr (ace-link-extras--slack-collect))
               (avy--style-fn avy-style)))))
    (ace-link-extras--slack-action pt)))

(defun ace-link-extras--slack-collect ()
  "Collect clickable elements in the visible portion of a slack buffer.
Returns an alist of (LABEL . POSITION) for each clickable element."
  (let ((wstart (window-start))
        (wend (window-end (selected-window) t))
        candidates
        collected-positions)
    (save-excursion
      ;; Collect lui buttons (URLs)
      (let ((pos wstart))
        (while (and (numberp pos) (< pos wend))
          (when-let ((btn (button-at pos)))
            (let ((start (button-start btn))
                  (end (button-end btn)))
              (when (and (<= wstart start) (<= end wend))
                (push (cons (buffer-substring-no-properties start end) start)
                      candidates)
                (push start collected-positions))
              (setq pos end)))
          (let ((next (next-button pos)))
            (setq pos (when next (button-start next))))))
      ;; Collect elements with local-map (user mentions, etc.)
      (let ((pos wstart))
        (while (and pos (< pos wend))
          (let ((map (get-text-property pos 'local-map)))
            (if map
                (let ((start pos)
                      (end (or (next-single-property-change pos 'local-map nil wend)
                               wend)))
                  (unless (button-at start)
                    (let ((label (string-trim
                                  (buffer-substring-no-properties
                                   start (min end (+ start 40))))))
                      (unless (string-empty-p label)
                        (push (cons label start) candidates)
                        (push start collected-positions))))
                  (setq pos end))
              (setq pos (next-single-property-change pos 'local-map nil wend))))))
      ;; Collect elements with keymap (channel links, thread status, etc.)
      (let ((pos wstart))
        (while (and pos (< pos wend))
          (let ((km (get-text-property pos 'keymap)))
            (if km
                (let ((start pos)
                      (end (or (next-single-property-change pos 'keymap nil wend)
                               wend)))
                  (unless (or (member start collected-positions)
                              (get-text-property start 'local-map)
                              (button-at start))
                    (let ((label (string-trim
                                  (buffer-substring-no-properties
                                   start (min end (+ start 50))))))
                      (unless (string-empty-p label)
                        (push (cons label start) candidates)
                        (push start collected-positions))))
                  (setq pos end))
              (setq pos (next-single-property-change pos 'keymap nil wend))))))
      ;; Collect message entries (ts property, for opening messages)
      ;; Skip past header line (image + name + timestamp padding) to body text
      (let ((pos wstart)
            last-ts)
        (while (and pos (< pos wend))
          (let ((ts (get-text-property pos 'ts)))
            (if (and ts (not (equal ts last-ts)))
                (let ((end (or (next-single-property-change pos 'ts nil wend)
                               wend))
                      (body-pos pos))
                  ;; Skip to end of first line (header + timestamp padding)
                  (while (and (< body-pos end)
                              (not (eq (char-after body-pos) ?\n)))
                    (setq body-pos (1+ body-pos)))
                  ;; Skip newlines and whitespace to find body text
                  (while (and (< body-pos end)
                              (memq (char-after body-pos) '(?\n ?\s ?\t)))
                    (setq body-pos (1+ body-pos)))
                  (when (and (< body-pos end)
                             (not (member body-pos collected-positions))
                             (not (button-at body-pos))
                             (not (get-text-property body-pos 'local-map))
                             (not (get-text-property body-pos 'keymap)))
                    (let ((label (string-trim
                                  (buffer-substring-no-properties
                                   body-pos (min end (+ body-pos 50))))))
                      (unless (string-empty-p label)
                        (push (cons label body-pos) candidates))))
                  (setq last-ts ts
                        pos end))
              (setq pos (next-single-property-change pos 'ts nil wend)))))))
    (nreverse candidates)))

(defun ace-link-extras--slack-action (pt)
  "Activate the clickable element at PT in a slack buffer."
  (when (numberp pt)
    (goto-char pt)
    (cond
     ;; lui button (URL)
     ((button-at pt)
      (push-button))
     ;; Element with local-map (user mentions, etc.)
     ((when-let ((map (get-text-property pt 'local-map))
                 (binding (or (lookup-key map (kbd "RET"))
                              (lookup-key map [mouse-1]))))
        (if (commandp binding)
            (call-interactively binding)
          (funcall binding))))
     ;; Element with keymap (channel links, thread status, etc.)
     ;; Some keymap actions (e.g. thread status) assume a channel buffer;
     ;; fall back to opening the message in the activity feed.
     ((when-let ((km (get-text-property pt 'keymap))
                 (binding (or (lookup-key km (kbd "RET"))
                              (lookup-key km [mouse-1]))))
        (condition-case nil
            (if (commandp binding)
                (call-interactively binding)
              (funcall binding))
          (cl-no-applicable-method
           (when (fboundp 'slack-activity-feed-open-message)
             (call-interactively #'slack-activity-feed-open-message))))))
     ;; Message entry (ts property) — open via mode's RET command
     ((get-text-property pt 'ts)
      (let ((binding (key-binding (kbd "RET"))))
        (when (commandp binding)
          (call-interactively binding))))
     (t (message "No action found at point")))))

(provide 'ace-link-extras)
;;; ace-link-extras.el ends here
