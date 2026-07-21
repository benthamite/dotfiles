;;; org-extras-test.el --- Tests for org-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in org-extras.el including line
;; counting, babel evaluation, link manipulation, and string operations.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-extras)
(require 'org-clock)

(defvar buffer-face-mode-hook)

;;;; count-lines-with-expression

(ert-deftest org-extras-test-count-lines-with-expression-basic-match ()
  "Count lines matching a simple regexp in a multiline string."
  (should (= (org-extras-count-lines-with-expression
              "foo\nbar\nfoo again" "foo")
             2)))

(ert-deftest org-extras-test-count-lines-with-expression-no-match ()
  "Return 0 when no lines match the regexp."
  (should (= (org-extras-count-lines-with-expression
              "foo\nbar\nbaz" "qux")
             0)))

(ert-deftest org-extras-test-count-lines-with-expression-all-match ()
  "Count all lines when every line matches."
  (should (= (org-extras-count-lines-with-expression
              "abc\nabc\nabc" "abc")
             3)))

(ert-deftest org-extras-test-count-lines-with-expression-empty-string ()
  "Handle empty string input (one empty line from split)."
  (should (= (org-extras-count-lines-with-expression "" "foo")
             0)))

(ert-deftest org-extras-test-count-lines-with-expression-single-line ()
  "Handle single-line string with match."
  (should (= (org-extras-count-lines-with-expression "hello" "hello")
             1)))

(ert-deftest org-extras-test-count-lines-with-expression-regexp ()
  "Match with a real regexp pattern, not just a literal string."
  (should (= (org-extras-count-lines-with-expression
              "abc123\ndef456\nghi" "[0-9]+")
             2)))

(ert-deftest org-extras-test-count-lines-with-expression-partial-match ()
  "Regexp matches partial line content."
  (should (= (org-extras-count-lines-with-expression
              "foobar\nbarbaz\nqux" "bar")
             2)))

;;;; confirm-babel-evaluate

(ert-deftest org-extras-test-confirm-babel-evaluate-safe-language ()
  "Return non-nil for languages in the safe list."
  (let ((org-extras-confirm-babel-evaluate-languages '("emacs-lisp" "python")))
    (should (org-extras-confirm-babel-evaluate "emacs-lisp" nil))
    (should (org-extras-confirm-babel-evaluate "python" nil))))

(ert-deftest org-extras-test-confirm-babel-evaluate-unsafe-language ()
  "Return nil for languages not in the safe list."
  (let ((org-extras-confirm-babel-evaluate-languages '("emacs-lisp" "python")))
    (should-not (org-extras-confirm-babel-evaluate "shell" nil))
    (should-not (org-extras-confirm-babel-evaluate "ruby" nil))))

(ert-deftest org-extras-test-confirm-babel-evaluate-empty-list ()
  "Return nil for any language when the safe list is empty."
  (let ((org-extras-confirm-babel-evaluate-languages nil))
    (should-not (org-extras-confirm-babel-evaluate "emacs-lisp" nil))))

(ert-deftest org-extras-test-confirm-babel-evaluate-ignores-body ()
  "The second argument (body) is ignored; result depends only on language."
  (let ((org-extras-confirm-babel-evaluate-languages '("python")))
    (should (org-extras-confirm-babel-evaluate "python" "print('hello')"))
    (should (org-extras-confirm-babel-evaluate "python" nil))))

;;;; remove-link

(ert-deftest org-extras-test-remove-link-with-description ()
  "Remove link syntax and keep the description text."
  (with-temp-buffer
    (org-mode)
    (insert "before [[https://example.com][Example]] after")
    (goto-char 10) ; inside the link
    (org-extras-remove-link)
    (should (equal (buffer-string) "before Example after"))))

(ert-deftest org-extras-test-remove-link-without-description ()
  "Remove link syntax and keep the URL when no description is present."
  (with-temp-buffer
    (org-mode)
    (insert "before [[https://example.com]] after")
    (goto-char 10) ; inside the link
    (org-extras-remove-link)
    (should (equal (buffer-string) "before https://example.com after"))))

(ert-deftest org-extras-test-remove-link-not-on-link ()
  "Do nothing when point is not on a link."
  (with-temp-buffer
    (org-mode)
    (insert "no link here")
    (goto-char (point-min))
    (org-extras-remove-link)
    (should (equal (buffer-string) "no link here"))))

;;;; link-get-thing-at-point / link-get-url-at-point / link-get-description-at-point

(ert-deftest org-extras-test-link-get-url-at-point-extracts-url ()
  "Extract URL from an org link at point via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][description]]")
    (goto-char 5) ; inside the link
    (org-extras-link-get-url-at-point)
    (should (equal (current-kill 0) "https://example.com"))))

(ert-deftest org-extras-test-link-get-url-at-point-adds-to-kill-ring ()
  "Verify the extracted URL is added to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][click here]]")
    (goto-char 5)
    (org-extras-link-get-url-at-point)
    (should (equal (current-kill 0) "https://example.com"))))

(ert-deftest org-extras-test-link-get-description-at-point-extracts-description ()
  "Extract description from an org link at point via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][My Description]]")
    (goto-char 5)
    (org-extras-link-get-description-at-point)
    (should (equal (current-kill 0) "My Description"))))

(ert-deftest org-extras-test-link-get-url-at-point-no-link ()
  "Return nil when point is not on an org link."
  (with-temp-buffer
    (org-mode)
    (insert "just plain text")
    (goto-char 5)
    (should-not (org-extras-link-get-url-at-point))))

(ert-deftest org-extras-test-link-get-url-at-point-no-description ()
  "Extract URL from a link without a description via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com]]")
    (goto-char 5)
    (org-extras-link-get-url-at-point)
    (should (equal (current-kill 0) "https://example.com"))))

(ert-deftest org-extras-test-link-get-link-at-point-extracts-full-link ()
  "Extract the full link syntax from an org link at point via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][desc]]")
    (goto-char 5)
    (org-extras-link-get-link-at-point)
    (let ((result (current-kill 0)))
      (should (string-match-p "https://example.com" result))
      (should (string-match-p "desc" result)))))

;;;; get-heading-contents

(ert-deftest org-extras-test-get-heading-contents-returns-body ()
  "Extract body text from an org heading, excluding sub-headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text line one\nBody text line two\n** Sub-heading\nSub content\n")
    (goto-char (point-min))
    (let ((contents (org-extras-get-heading-contents)))
      (should (string-match-p "Body text line one" contents))
      (should-not (string-match-p "Sub content" contents)))))

(ert-deftest org-extras-test-get-heading-contents-empty-heading ()
  "Return empty string for a heading with no body text."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n** Sub-heading\n")
    (goto-char (point-min))
    (let ((contents (org-extras-get-heading-contents)))
      (should (stringp contents))
      (should (string= contents "")))))

(ert-deftest org-extras-test-get-heading-contents-from-inside-body ()
  "Extract body text when point is inside the heading body, not on the heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text here\n** Sub-heading\n")
    (goto-char (point-min))
    (forward-line 1) ; point is on "Body text here"
    (let ((contents (org-extras-get-heading-contents)))
      (should (string-match-p "Body text here" contents)))))

(ert-deftest org-extras-test-get-heading-contents-before-first-heading ()
  "Return a message when point is before the first heading."
  (with-temp-buffer
    (org-mode)
    (insert "Some preamble\n* Heading\nBody\n")
    (goto-char (point-min))
    (should (equal (org-extras-get-heading-contents)
                   "Not in or on an org heading"))))

;;;; org-agenda

(ert-deftest org-extras-test-agenda-switch-does-not-visit-config ()
  "Build the agenda from a non-Org buffer without visiting config."
  (let ((agenda-called nil)
        (config-visited nil)
        (observed-hooks nil)
        (change-major-mode-after-body-hook '(change-hook))
        (find-file-hook '(find-hook))
        (org-mode-hook '(org-hook))
        (outline-mode-hook '(outline-hook))
        (text-mode-hook '(text-hook)))
    (should-not (get-buffer "*Org Agenda(a)*"))
    (cl-letf (((symbol-function 'window-extras-split-if-unsplit) #'ignore)
              ((symbol-function 'winum-select-window-1) #'ignore)
              ((symbol-function 'find-file)
               (lambda (&rest _)
                 (setq config-visited t)))
              ((symbol-function 'org-extras-agenda-toggle-anniversaries) #'ignore)
              ((symbol-function 'org-extras-reset-org-element-caches) #'ignore)
              ((symbol-function 'org-agenda)
               (lambda (&rest _)
                 (setq agenda-called t)
                 (should (eq major-mode 'fundamental-mode))
                 (push (list change-major-mode-after-body-hook
                             find-file-hook
                             org-mode-hook
                             outline-mode-hook
                             text-mode-hook)
                       observed-hooks))))
      (with-temp-buffer
        (should (eq major-mode 'fundamental-mode))
        (org-extras-agenda-switch-to-agenda-current-day))
      (should agenda-called)
      (should-not config-visited)
      (should (equal observed-hooks
                     '(((change-hook) nil (org-hook) (outline-hook) (text-hook))))))))

(ert-deftest org-extras-test-agenda-maintenance-without-agenda-avoids-ui ()
  "Maintain agenda state without creating or displaying an agenda."
  (should-not (get-buffer "*Org Agenda(a)*"))
  (let ((events nil)
        (original-buffer (current-buffer))
        (original-window (selected-window))
        (original-configuration (current-window-configuration)))
    (cl-letf (((symbol-function 'org-extras-with-suppressed-agenda-file-opening-hooks)
               (lambda (fn)
                 (push 'suppressed events)
                 (funcall fn)))
              ((symbol-function 'org-extras--agenda-update-anniversaries)
               (lambda (just-enable no-interactive-fallback)
                 (should just-enable)
                 (should no-interactive-fallback)
                 (push 'anniversaries events)))
              ((symbol-function 'find-file)
               (lambda (&rest _)
                 (ert-fail "Background maintenance visited a file")))
              ((symbol-function 'org-agenda)
               (lambda (&rest _)
                 (ert-fail "Background maintenance created an agenda")))
              ((symbol-function 'switch-to-buffer)
               (lambda (&rest _)
                 (ert-fail "Background maintenance switched buffers")))
              ((symbol-function 'pop-to-buffer)
               (lambda (&rest _)
                 (ert-fail "Background maintenance popped to a buffer")))
              ((symbol-function 'display-buffer)
               (lambda (&rest _)
                 (ert-fail "Background maintenance displayed a buffer")))
              ((symbol-function 'window-extras-split-if-unsplit)
               (lambda (&rest _)
                 (ert-fail "Background maintenance changed windows"))))
      (with-temp-buffer
        (should (eq major-mode 'fundamental-mode))
        (org-extras-agenda-maintain-current-day)
        (should (eq major-mode 'fundamental-mode)))
      (should (equal (nreverse events) '(suppressed anniversaries)))
      (should (eq (current-buffer) original-buffer))
      (should (eq (selected-window) original-window))
      (should (compare-window-configurations
               original-configuration (current-window-configuration))))))

(ert-deftest org-extras-test-agenda-maintenance-refreshes-existing-agenda ()
  "Refresh an existing agenda without displaying it."
  (let ((agenda (get-buffer-create "*Org Agenda(a)*"))
        (events nil)
        (original-buffer (current-buffer))
        (original-window (selected-window))
        (original-configuration (current-window-configuration)))
    (unwind-protect
        (progn
          (with-current-buffer agenda
            (setq-local major-mode 'org-agenda-mode))
          (cl-letf (((symbol-function 'org-extras-with-suppressed-agenda-file-opening-hooks)
                     (lambda (fn)
                       (push 'suppressed events)
                       (funcall fn)))
                    ((symbol-function 'org-extras--agenda-update-anniversaries)
                     (lambda (just-enable no-interactive-fallback)
                       (should just-enable)
                       (should no-interactive-fallback)
                       (push 'anniversaries events)))
                    ((symbol-function 'org-agenda-redo)
                     (lambda ()
                       (should (eq (current-buffer) agenda))
                       (push 'refreshed events)))
                    ((symbol-function 'find-file)
                     (lambda (&rest _)
                       (ert-fail "Background maintenance visited a file")))
                    ((symbol-function 'org-agenda)
                     (lambda (&rest _)
                       (ert-fail "Background maintenance created an agenda")))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (&rest _)
                       (ert-fail "Background maintenance switched buffers")))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _)
                       (ert-fail "Background maintenance popped to a buffer")))
                    ((symbol-function 'display-buffer)
                     (lambda (&rest _)
                       (ert-fail "Background maintenance displayed a buffer"))))
            (org-extras-agenda-maintain-current-day)
            (should (equal (nreverse events)
                           '(suppressed anniversaries refreshed)))
            (should (eq (current-buffer) original-buffer))
            (should (eq (selected-window) original-window))
            (should (compare-window-configurations
                     original-configuration (current-window-configuration)))))
      (when (buffer-live-p agenda)
        (kill-buffer agenda)))))

(ert-deftest org-extras-test-agenda-maintenance-ignores-non-agenda-namesake ()
  "Do not refresh an ordinary buffer that has the agenda buffer name."
  (let ((agenda (get-buffer-create "*Org Agenda(a)*")))
    (unwind-protect
        (cl-letf (((symbol-function 'org-extras-with-suppressed-agenda-file-opening-hooks)
                   (lambda (fn) (funcall fn)))
                  ((symbol-function 'org-extras--agenda-update-anniversaries)
                   #'ignore)
                  ((symbol-function 'org-agenda-redo)
                   (lambda ()
                     (ert-fail "Refreshed a non-agenda buffer"))))
          (with-current-buffer agenda
            (setq-local major-mode 'fundamental-mode))
          (org-extras-agenda-maintain-current-day))
      (when (buffer-live-p agenda)
        (kill-buffer agenda)))))

(ert-deftest org-extras-test-agenda-background-anniversaries-never-jump ()
  "Report a missing anniversary ID without using an interactive jump."
  (cl-letf (((symbol-function 'org-id-find) (lambda (&rest _) nil))
            ((symbol-function 'org-roam-extras-id-goto)
             (lambda (&rest _)
               (ert-fail "Background anniversary maintenance jumped interactively"))))
    (should-error (org-extras--agenda-update-anniversaries t t))))

(ert-deftest org-extras-test-agenda-file-opening-hooks-suppressed ()
  "Suppress interactive setup while preserving unrelated mode hooks."
  (let ((observed-hooks nil)
        (observed-startup-indented 'unset)
        (observed-local-variables 'unset)
        (called nil)
        (change-major-mode-after-body-hook '(change-hook variable-pitch-mode))
        (enable-local-variables t)
        (find-file-hook '(find-hook))
        (org-mode-hook '(org-hook org-indent-mode org-modern-indent-mode
                                  org-tidy-mode))
        (org-startup-indented t)
        (outline-mode-hook '(outline-hook variable-pitch-mode))
        (text-mode-hook '(text-hook jinx-mode))
        (buffer-face-mode-hook '(face-hook org-indent-pixel--maybe-activate)))
    (cl-letf (((symbol-function 'jinx-mode)
               (lambda (&rest _)
                 (push 'jinx-mode called)))
              ((symbol-function 'org-indent-pixel--maybe-activate)
               (lambda (&rest _)
                 (push 'org-indent-pixel--maybe-activate called)))
              ((symbol-function 'org-indent-pixel-mode)
               (lambda (&rest _)
                 (push 'org-indent-pixel-mode called)))
              ((symbol-function 'org-indent-mode)
               (lambda (&rest _)
                 (push 'org-indent-mode called)))
              ((symbol-function 'org-modern-indent-mode)
               (lambda (&rest _)
                 (push 'org-modern-indent-mode called)))
              ((symbol-function 'org-tidy-mode)
               (lambda (&rest _)
                 (push 'org-tidy-mode called)))
              ((symbol-function 'variable-pitch-mode)
               (lambda (&rest _)
                 (push 'variable-pitch-mode called))))
      (org-extras-with-suppressed-agenda-file-opening-hooks
       (lambda ()
         (setq observed-hooks
               (list change-major-mode-after-body-hook
                     find-file-hook
                     org-mode-hook
                     outline-mode-hook
                     text-mode-hook
                     buffer-face-mode-hook))
         (setq observed-startup-indented org-startup-indented)
         (setq observed-local-variables enable-local-variables)
         (jinx-mode 1)
         (org-indent-pixel--maybe-activate)
         (org-indent-pixel-mode 1)
         (org-indent-mode 1)
         (org-modern-indent-mode 1)
         (org-tidy-mode 1)
         (variable-pitch-mode 1))))
    (should (equal observed-hooks
                   '((change-hook)
                     nil
                     (org-hook)
                     (outline-hook)
                     (text-hook)
                     (face-hook))))
    (should-not observed-startup-indented)
    (should-not observed-local-variables)
    (should-not called)
    (should (equal change-major-mode-after-body-hook
                   '(change-hook variable-pitch-mode)))
    (should enable-local-variables)
    (should (equal find-file-hook '(find-hook)))
    (should (equal org-mode-hook
                   '(org-hook org-indent-mode org-modern-indent-mode
                              org-tidy-mode)))
    (should org-startup-indented)
    (should (equal outline-mode-hook '(outline-hook variable-pitch-mode)))
    (should (equal text-mode-hook '(text-hook jinx-mode)))
    (should (equal buffer-face-mode-hook
                   '(face-hook org-indent-pixel--maybe-activate)))))

;;;; Refile cache

(ert-deftest org-extras-test-reset-org-element-caches-resets-file-buffers ()
  "Reset Org element caches for live Org file buffers."
  (let ((buf-a (generate-new-buffer " *org-extras-cache-a*"))
        (buf-b (generate-new-buffer " *org-extras-cache-b*"))
        (reset-buffers nil))
    (unwind-protect
        (cl-letf (((symbol-function 'org-buffer-list)
                   (lambda (&optional scope)
                     (should (eq scope 'files))
                     (list buf-a buf-b)))
                  ((symbol-function 'org-element-cache-reset)
                   (lambda ()
                     (push (current-buffer) reset-buffers))))
          (org-extras-reset-org-element-caches)
          (should (equal (nreverse reset-buffers) (list buf-a buf-b))))
      (when (buffer-live-p buf-a)
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (kill-buffer buf-b)))))

(ert-deftest org-extras-test-refile-regenerate-cache-suppresses-hooks ()
  "Rebuild the refile cache with cache reset and interruptible hooks."
  (let ((cache-cleared nil)
        (events nil)
        (suppressed nil)
        (caches-reset nil)
        (inhibit-quit-seen 'unset))
    (cl-letf (((symbol-function 'org-refile-cache-clear)
               (lambda ()
                 (setq cache-cleared t)
                 (push 'cache-cleared events)))
              ((symbol-function 'org-extras-with-suppressed-agenda-file-opening-hooks)
               (lambda (fn)
                 (setq suppressed t)
                 (push 'suppressed events)
                 (funcall fn)))
              ((symbol-function 'org-extras-reset-org-element-caches)
               (lambda ()
                 (setq caches-reset t)
                 (push 'caches-reset events)))
              ((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (when (eq feature 'files-extras)
                   (push 'files-extras-required events))
                 t))
              ((symbol-function 'org-mode)
               #'ignore)
              ((symbol-function 'org-refile-get-targets)
               (lambda ()
                 (push 'targets events)
                 (setq inhibit-quit-seen inhibit-quit)
                 nil)))
      (let ((inhibit-quit nil))
        (org-extras-refile-regenerate-cache)))
    (should cache-cleared)
    (should caches-reset)
    (should suppressed)
    (should (equal (nreverse events)
                   '(suppressed cache-cleared caches-reset
                     files-extras-required targets)))
    (should-not inhibit-quit-seen)))

(ert-deftest org-extras-test-refile-regenerate-cache-skips-reentrant-run ()
  "Skip overlapping refile cache rebuilds."
  (let ((org-extras-refile-regenerate-cache--running t)
        (cache-cleared nil)
        (targets-called nil)
        (messages nil))
    (cl-letf (((symbol-function 'org-refile-cache-clear)
               (lambda () (setq cache-cleared t)))
              ((symbol-function 'org-refile-get-targets)
               (lambda () (setq targets-called t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (org-extras-refile-regenerate-cache))
    (should-not cache-cleared)
    (should-not targets-called)
    (should (cl-some
             (lambda (msg)
               (string-match-p "already running" msg))
             messages))))

(ert-deftest org-extras-test-refile-regenerate-cache-start-timer-replaces-existing ()
  "Start one owned repeating idle timer for refile cache rebuilds."
  (let ((old-timer (timer-create))
        (new-timer 'new-timer)
        (cancelled nil)
        (timer-args nil)
        (org-extras-refile-regenerate-cache-timer nil))
    (setq org-extras-refile-regenerate-cache-timer old-timer)
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled timer)))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest args)
                 (setq timer-args args)
                 new-timer)))
      (org-extras-refile-regenerate-cache-start-timer 42)
      (should (eq cancelled old-timer))
      (should (equal timer-args
                     (list 42 t #'org-extras-refile-regenerate-cache)))
      (should (eq org-extras-refile-regenerate-cache-timer new-timer)))))

(ert-deftest org-extras-test-anniversary-toggle-suppresses-temp-buffer-hooks ()
  "Suppress presentation hooks only when opening anniversary file internally."
  (let ((observed-hooks nil)
        (org-extras-bbdb-anniversaries-heading "anniversary-id")
        (change-major-mode-after-body-hook '(ignore))
        (find-file-hook '(ignore))
        (org-mode-hook '(ignore))
        (outline-mode-hook '(ignore))
        (text-mode-hook '(ignore)))
    (let ((file (make-temp-file "org-extras-anniversary" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* Birthdays\n:PROPERTIES:\n:ID: anniversary-id\n:END:\n"))
            (let ((real-find-file-noselect (symbol-function 'find-file-noselect)))
              (cl-letf (((symbol-function 'org-id-find) (lambda (_) (cons file 1)))
                        ((symbol-function 'find-file-noselect)
                         (lambda (&rest args)
                           (push (list change-major-mode-after-body-hook
                                       find-file-hook
                                       org-mode-hook
                                       outline-mode-hook
                                       text-mode-hook)
                                 observed-hooks)
                           (apply real-find-file-noselect args))))
                (org-extras-agenda-toggle-anniversaries t)))
            (should (equal observed-hooks
                           '(((ignore) nil (ignore) (ignore) (ignore)))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (string-match-p "%%(org-bbdb-anniversaries-future 1)"
                                      (buffer-string)))))
        (delete-file file)))))

;;;; copy-heading-name

(ert-deftest org-extras-test-copy-heading-name-copies-to-kill-ring ()
  "Copy the current heading name to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "* My Important Heading\nSome body\n")
    (goto-char (point-min))
    (org-extras-copy-heading-name)
    (should (equal (current-kill 0) "My Important Heading"))))

(ert-deftest org-extras-test-copy-heading-name-with-todo ()
  "Copy heading name excluding TODO keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO My Task\nSome body\n")
    (goto-char (point-min))
    (org-extras-copy-heading-name)
    (should (equal (current-kill 0) "My Task"))))

;;;; suppress-track-changes-assertion

(ert-deftest org-extras-test-suppress-track-changes-assertion-normal-function ()
  "Allow normal function calls through without interference."
  (let ((result (org-extras--suppress-track-changes-assertion
                 (lambda (x y) (+ x y))
                 3 4)))
    (should (equal result 7))))

(ert-deftest org-extras-test-suppress-track-changes-assertion-suppresses-assertion ()
  "Suppress cl-assertion-failed errors and return nil."
  (let ((result (org-extras--suppress-track-changes-assertion
                 (lambda ()
                   (signal 'cl-assertion-failed '(nil)))
                 )))
    (should-not result)))

(ert-deftest org-extras-test-suppress-track-changes-assertion-propagates-other-errors ()
  "Do not suppress errors other than cl-assertion-failed."
  (should-error
   (org-extras--suppress-track-changes-assertion
    (lambda () (error "Some other error")))
   :type 'error))

;;;; insert-subheading

(ert-deftest org-extras-test-insert-subheading-creates-child ()
  "Insert a subheading as a child of the current heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nSome body text\n")
    (goto-char (point-min))
    (org-extras-insert-subheading)
    (should (looking-back "^\\*\\* " nil))
    ;; The buffer should have a level-2 heading
    (should (string-match-p "^\\*\\* " (buffer-string)))))

(ert-deftest org-extras-test-insert-subheading-before-existing-children ()
  "Insert a subheading above existing sub-headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nBody text\n** Existing child\nChild body\n")
    (goto-char (point-min))
    (org-extras-insert-subheading)
    ;; Point should be on the newly inserted heading, which is before
    ;; the existing child
    (let ((new-pos (point)))
      (should (string-match-p "^\\*\\* Existing child"
                              (buffer-substring new-pos (point-max)))))))

;;;; copy-heading-contents

(ert-deftest org-extras-test-copy-heading-contents-to-kill-ring ()
  "Copy heading body text to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody line one\nBody line two\n** Sub\n")
    (goto-char (point-min))
    (org-extras-copy-heading-contents)
    (should (string-match-p "Body line one" (current-kill 0)))))

(ert-deftest org-extras-test-copy-heading-contents-empty-heading ()
  "Display message when heading has no body text."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n** Sub\n")
    (goto-char (point-min))
    (should (equal (org-extras-copy-heading-contents) "Heading is empty."))))

;;;; count-words

(ert-deftest org-extras-test-count-words-basic ()
  "Count words in a subtree body and return a summary message."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\nOne two three four five\n")
    (goto-char (point-min))
    (let ((result (org-extras-count-words)))
      (should (stringp result))
      (should (string-match-p "5 words" result)))))

(ert-deftest org-extras-test-count-words-with-children ()
  "Count words across subtree including children."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nOne two\n** Child\nThree four five\n")
    (goto-char (point-min))
    (let ((result (org-extras-count-words)))
      (should (stringp result))
      (should (string-match-p "5 words" result)))))

(ert-deftest org-extras-test-count-words-empty-body ()
  "Count zero words in a heading with no body."
  (with-temp-buffer
    (org-mode)
    (insert "* Empty heading\n")
    (goto-char (point-min))
    (let ((result (org-extras-count-words)))
      (should (stringp result))
      (should (string-match-p "0 words" result)))))

(ert-deftest org-extras-test-count-words-copies-to-kill-ring ()
  "Verify that word count number is copied to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nAlpha beta gamma\n")
    (goto-char (point-min))
    (org-extras-count-words)
    (should (equal (current-kill 0) "3"))))

;;;; jump-to-first-heading

(ert-deftest org-extras-test-jump-to-first-heading-basic ()
  "Move point to the first heading in the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "Preamble text\n* First heading\n* Second heading\n")
    (goto-char (point-max))
    (org-extras-jump-to-first-heading)
    (should (org-at-heading-p))
    (should (string-match-p "First heading" (org-get-heading t t t t)))))

(ert-deftest org-extras-test-jump-to-first-heading-widens ()
  "Widen the buffer before jumping to the first heading."
  (with-temp-buffer
    (org-mode)
    (insert "Preamble\n* First heading\nBody\n* Second heading\nBody\n")
    (goto-char (point-min))
    (org-next-visible-heading 2) ; on second heading
    (org-narrow-to-subtree)
    (org-extras-jump-to-first-heading)
    ;; Buffer should be widened and point on first heading
    (should (= (point-min) 1))
    (should (string-match-p "First heading" (org-get-heading t t t t)))))

(ert-deftest org-extras-test-jump-to-first-heading-from-end ()
  "Jump to the first heading from the end of the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "Some preamble\n* First heading\nBody\n* Second heading\n")
    (goto-char (point-max))
    (org-extras-jump-to-first-heading)
    (should (org-at-heading-p))
    (should (string-match-p "First heading" (org-get-heading t t t t)))))

;;;; narrow-to-entry-and-children

(ert-deftest org-extras-test-narrow-to-entry-and-children-basic ()
  "Narrow to the full subtree including children."
  (with-temp-buffer
    (org-mode)
    (insert "* First\nBody 1\n** Child\nChild body\n* Second\nBody 2\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-and-children)
    ;; The narrowed region should include the child but not the second heading
    (should (string-match-p "Child body" (buffer-string)))
    (should-not (string-match-p "Second" (buffer-string)))))

(ert-deftest org-extras-test-narrow-to-entry-and-children-excludes-siblings ()
  "Narrowing excludes sibling headings at the same level."
  (with-temp-buffer
    (org-mode)
    (insert "* H1\nBody A\n** Child A\n* H2\nBody B\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-and-children)
    (should (string-match-p "Body A" (buffer-string)))
    (should (string-match-p "Child A" (buffer-string)))
    (should-not (string-match-p "H2" (buffer-string)))))

;;;; narrow-to-entry-no-children

(ert-deftest org-extras-test-narrow-to-entry-no-children-basic ()
  "Narrow to entry excluding child subtrees."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nParent body\n** Child\nChild body\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-no-children)
    (should (string-match-p "Parent body" (buffer-string)))
    (should-not (string-match-p "Child" (buffer-string)))))

(ert-deftest org-extras-test-narrow-to-entry-no-children-no-body ()
  "Narrow to entry with no body and children."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\nChild body\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-no-children)
    ;; Should only contain the heading line
    (should (string-match-p "Parent" (buffer-string)))
    (should-not (string-match-p "Child" (buffer-string)))))

(ert-deftest org-extras-test-narrow-to-entry-no-children-excludes-siblings ()
  "Narrowing excludes both children and sibling headings."
  (with-temp-buffer
    (org-mode)
    (insert "* H1\nBody\n** Child\n* H2\nOther\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-no-children)
    (should (string-match-p "Body" (buffer-string)))
    (should-not (string-match-p "Child" (buffer-string)))
    (should-not (string-match-p "H2" (buffer-string)))))

;;;; goto-beginning-of-heading-text

(ert-deftest org-extras-test-goto-beginning-of-heading-text-plain ()
  "Position point at the start of heading text (after stars and space)."
  (with-temp-buffer
    (org-mode)
    (insert "* Simple heading\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "Simple heading"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-with-todo ()
  "Position point after TODO keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO My task\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "My task"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-with-priority ()
  "Position point after priority cookie."
  (with-temp-buffer
    (org-mode)
    (insert "* [#A] Important thing\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "Important thing"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-with-todo-and-priority ()
  "Position point after both TODO keyword and priority cookie."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#B] Another task\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "Another task"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-not-on-heading ()
  "Do nothing when point is not on a heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text\n")
    (goto-char (point-min))
    (forward-line 1) ; on body text
    (let ((pos (point)))
      (org-extras-goto-beginning-of-heading-text)
      (should (= (point) pos)))))

;;;; remove-trailing-heading

(ert-deftest org-extras-test-remove-trailing-heading-empty ()
  "Remove an empty heading at the end of the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* Real heading\nBody\n* \n")
    (org-extras-remove-trailing-heading)
    (should-not (string-match-p "^\\* $" (buffer-string)))
    (should (string-match-p "Real heading" (buffer-string)))))

(ert-deftest org-extras-test-remove-trailing-heading-non-empty ()
  "Do not remove a non-empty trailing heading."
  (with-temp-buffer
    (org-mode)
    (insert "* First\nBody\n* Second\nMore body\n")
    (org-extras-remove-trailing-heading)
    (should (string-match-p "Second" (buffer-string)))))

(ert-deftest org-extras-test-remove-trailing-heading-preserves-content ()
  "Preserve all content before the empty trailing heading."
  (with-temp-buffer
    (org-mode)
    (insert "* H1\nBody one\n* H2\nBody two\n* \n")
    (org-extras-remove-trailing-heading)
    (should (string-match-p "Body one" (buffer-string)))
    (should (string-match-p "Body two" (buffer-string)))
    (should (string-match-p "H2" (buffer-string)))))

;;;; clocktable-sorter

(ert-deftest org-extras-test-clocktable-sorter-sorts-descending ()
  "Sort clocktable entries by time in descending order."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("File A" 100 nil)
                       '("File C" 300 nil)
                       '("File B" 200 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (equal (mapcar #'cadr sorted-tables) '(300 200 100)))))

(ert-deftest org-extras-test-clocktable-sorter-already-sorted ()
  "Handle tables that are already in descending order."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("A" 300 nil)
                       '("B" 200 nil)
                       '("C" 100 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (equal (mapcar #'cadr sorted-tables) '(300 200 100)))))

(ert-deftest org-extras-test-clocktable-sorter-single-table ()
  "Handle a single-element table list."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("Only" 42 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (equal (mapcar #'cadr sorted-tables) '(42)))))

(ert-deftest org-extras-test-clocktable-sorter-equal-times ()
  "Handle tables with equal time values."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("A" 100 nil)
                       '("B" 100 nil)
                       '("C" 100 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (= (length sorted-tables) 3))
    (should (cl-every (lambda (t1) (= (cadr t1) 100)) sorted-tables))))

;;;; reset-checkbox-state-subtree

(ert-deftest org-extras-test-reset-checkbox-state-subtree-basic ()
  "Reset checked checkboxes in a subtree to unchecked."
  (with-temp-buffer
    (org-mode)
    (insert "* Task list\n- [X] Item one\n- [X] Item two\n- [ ] Item three\n")
    (goto-char (point-min))
    (org-extras-reset-checkbox-state-subtree)
    (should-not (string-match-p "\\[X\\]" (buffer-string)))
    (should (string-match-p "\\[ \\]" (buffer-string)))))

(ert-deftest org-extras-test-reset-checkbox-state-subtree-preserves-text ()
  "Preserve checkbox text while resetting state."
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n- [X] Do laundry\n- [X] Buy groceries\n")
    (goto-char (point-min))
    (org-extras-reset-checkbox-state-subtree)
    (should (string-match-p "Do laundry" (buffer-string)))
    (should (string-match-p "Buy groceries" (buffer-string)))))

(ert-deftest org-extras-test-reset-checkbox-state-subtree-already-unchecked ()
  "Handle subtree where all checkboxes are already unchecked."
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n- [ ] Item one\n- [ ] Item two\n")
    (goto-char (point-min))
    (org-extras-reset-checkbox-state-subtree)
    (should-not (string-match-p "\\[X\\]" (buffer-string)))
    ;; Still has unchecked checkboxes
    (should (string-match-p "\\[ \\]" (buffer-string)))))

;;;; Citation preview export

(ert-deftest org-extras-test-citation-preview-keys ()
  "Extract unique org-cite keys from Org source."
  (should (equal (org-extras--citation-preview-keys
                  "[cite:@Alpha2020; @Beta2021]\n[cite:@Alpha2020]")
                 '("Alpha2020" "Beta2021"))))

(ert-deftest org-extras-test-citation-preview-use-bibentry-style ()
  "Rewrite org-cite markers to the CSL bibentry style."
  (with-temp-buffer
    (insert "[cite:@Alpha2020]\n[cite/t:@Beta2021]\n")
    (org-extras--citation-preview-use-bibentry-style)
    (should (equal (buffer-string)
                   "[cite/bibentry:@Alpha2020]\n[cite/bibentry:@Beta2021]\n"))))

(ert-deftest org-extras-test-citation-preview-sanitize-entry ()
  "Move non-date date fields to pubstate for CSL parsing."
  (let ((entry "@book{Alpha,\n  date = {forthcoming},\n  title = {A}\n}"))
    (let ((sanitized (org-extras--citation-preview-sanitize-entry entry)))
      (should (string-match-p "pubstate = {forthcoming}" sanitized))
      (should-not (string-match-p "date = {forthcoming}" sanitized)))))

(ert-deftest org-extras-test-citation-preview-preserve-valid-date ()
  "Preserve valid date fields when sanitizing BibLaTeX entries."
  (let ((entry "@book{Alpha,\n  date = {2026-06-07},\n  title = {A}\n}"))
    (should (string-match-p
             "date = {2026-06-07}"
             (org-extras--citation-preview-sanitize-entry entry)))))

(ert-deftest org-extras-test-citation-preview-normalize-output-file ()
  "Normalize citation preview output paths to HTML files."
  (should (string-suffix-p
           "preview.html"
           (org-extras--citation-preview-normalize-output-file "preview.org")))
  (should (string-suffix-p
           "preview.html"
           (org-extras--citation-preview-normalize-output-file "preview.html"))))

(ert-deftest org-extras-test-citation-preview-effective-style ()
  "Ignore the legacy Markdown-oriented citation preview style."
  (let ((org-extras-citation-preview-style "en/long.csl"))
    (should-not (org-extras--citation-preview-effective-style)))
  (let ((org-extras-citation-preview-style "chicago-author-date.csl"))
    (should (equal (org-extras--citation-preview-effective-style)
                   "chicago-author-date.csl"))))

(ert-deftest org-extras-test-citation-preview-check-output-file ()
  "Reject citation preview output paths that overwrite the source file."
  (let ((file (make-temp-file "org-extras-source-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (should-error (org-extras--citation-preview-check-output-file file))
          (kill-buffer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-extras-test-citation-preview-find-entry-in-file ()
  "Find a single BibLaTeX entry by key."
  (let ((file (make-temp-file "org-extras-bib-" nil ".bib")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "@book{Alpha2020,\n  title = {Alpha}\n}\n\n")
            (insert "@book{Beta2021,\n  title = {Beta {Nested}}\n}\n"))
          (should (string-match-p
                   "Beta {Nested}"
                   (org-extras--citation-preview-find-entry-in-file
                    "Beta2021" file))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-extras-test-citation-preview-assert-output ()
  "Reject HTML that still contains unresolved citation output."
  (let ((file (make-temp-file "org-extras-cite-output-" nil ".html")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "<li>(NO_ITEM_DATA:Alpha2020)</li>"))
          (should-error (org-extras--citation-preview-assert-output file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-extras-test-citation-preview-assert-output-markdown ()
  "Reject HTML that still contains Markdown link markup."
  (let ((file (make-temp-file "org-extras-cite-output-" nil ".html")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "<li>[Title](<a href=\"https://example.com\">url</a>)</li>"))
          (should-error (org-extras--citation-preview-assert-output file)))
      (when (file-exists-p file)
        (delete-file file)))))

;;;; EWW copy formatting

(ert-deftest org-extras-test-shr-heading-level ()
  "Detect heading levels from shr faces."
  (should (= (org-extras-shr-heading-level '(shr-h2 bold)) 2))
  (should-not (org-extras-shr-heading-level '(bold italic))))

;;;; Org ID background maintenance

(ert-deftest org-extras-test-id-update-background-binds-debug-on-error ()
  "Disable debugger entry only around the background Org ID update wrapper."
  (let ((called nil)
        (debug-on-error-seen nil))
    (cl-letf (((symbol-function 'org-extras-id-update-id-locations)
               (lambda ()
                 (setq called t)
                 (setq debug-on-error-seen debug-on-error))))
      (let ((debug-on-error t))
        (org-extras-id-update-id-locations-background)))
    (should called)
    (should-not debug-on-error-seen)))

(provide 'org-extras-test)
;;; org-extras-test.el ends here
