;;; ai-extras.el --- Shared extensions for AI coding CLI tools -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ai-extras.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.0") (transient "0.9") (consult "1.0"))

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

;; Shared abstractions for AI coding CLI tool extensions.
;; Provides backend-agnostic session management, notifications, and
;; terminal integration for packages like `claude-code-extras' and
;; `codex-extras'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

;;;; Custom group

(defgroup ai-extras ()
  "Shared extensions for AI coding CLI tools."
  :group 'tools)

;;;; Backend registry

(defvar ai-extras-backends nil
  "Alist of registered AI backends.
Each entry is (SYMBOL . PLIST) where PLIST has keys:
  :buffer-p              function (buffer) -> bool
  :find-all-buffers      function () -> list of buffers
  :find-buffers-for-dir  function (dir) -> list of buffers
  :directory             function (buffer) -> directory string
  :extract-directory     function (buffer-name) -> directory string
  :extract-instance-name function (buffer-name) -> instance or nil
  :send-command          function (cmd &optional buffer)
  :start                 function (arg extra-switches &optional force-prompt force-switch)
  :start-new             function () -> buffer (start a new session)
  :program               string (CLI binary name)
  :send-return           function (&optional buffer)
  :icon                  string (short identifier, e.g. \"CC\" or \"CX\")
  :label                 string (display name, e.g. \"Claude Code\" or \"Codex\")")

(defvar-local ai-extras--backend nil
  "Cached backend symbol for this buffer.")

(defun ai-extras-register-backend (symbol plist)
  "Register SYMBOL as an AI extras backend with PLIST properties."
  (setf (alist-get symbol ai-extras-backends) plist))

(defun ai-extras--detect-backend (&optional buffer)
  "Detect which AI backend BUFFER belongs to.
Try each registered backend's :buffer-p predicate.  Return the
backend symbol or nil."
  (let ((buf (or buffer (current-buffer))))
    (or (buffer-local-value 'ai-extras--backend buf)
        (let ((found (cl-find-if
                      (lambda (entry)
                        (funcall (plist-get (cdr entry) :buffer-p) buf))
                      ai-extras-backends)))
          (when found
            (with-current-buffer buf
              (setq ai-extras--backend (car found)))
            (car found))))))

(defun ai-extras--backend-get (backend key)
  "Get KEY from the registered plist for BACKEND."
  (plist-get (alist-get backend ai-extras-backends) key))

(defun ai-extras--find-all-buffers ()
  "Return all active AI session buffers across all backends."
  (let (result)
    (dolist (entry ai-extras-backends)
      (let ((bufs (funcall (plist-get (cdr entry) :find-all-buffers))))
        (setq result (nconc result bufs))))
    result))

(defun ai-extras--session-name (buffer-name)
  "Extract the project name from BUFFER-NAME.
Given \"*claude:~/path/to/project/:default*\" or
\"*codex:~/path/to/project/:default*\", return \"project\"."
  (if (string-match "/\\([^/]+\\)/:[^*]+\\*\\'" buffer-name)
      (match-string 1 buffer-name)
    buffer-name))

;;;; Customization

(defcustom ai-extras-protect-buffers t
  "When non-nil, prompt for confirmation before killing AI session buffers."
  :type 'boolean
  :group 'ai-extras)

(defcustom ai-extras-alert-style 'both
  "Style of alert when an AI session finishes responding.
Only takes effect when `ai-extras-alert-on-ready' is non-nil."
  :type '(choice (const :tag "Visual notification only" visual)
                 (const :tag "Sound only" sound)
                 (const :tag "Both visual and sound" both))
  :group 'ai-extras)

(defcustom ai-extras-alert-sound "/System/Library/Sounds/Glass.aiff"
  "Path to the sound file played when a session finishes responding."
  :type 'file
  :group 'ai-extras)

(defcustom ai-extras-alert-on-ready nil
  "When non-nil, alert the user when an AI session finishes responding.
Toggle with `ai-extras-toggle-alert'."
  :type 'boolean
  :group 'ai-extras)

(defcustom ai-extras-sigwinch-delay 0.5
  "Delay in seconds before sending SIGWINCH to fix terminal rendering."
  :type 'number
  :group 'ai-extras)

;;;; Faces

(defface ai-extras-waiting
  '((t :inherit warning))
  "Face for sessions waiting for user input in the session switcher."
  :group 'ai-extras)

;;;; State variables

(defconst ai-extras--home-row-keys '("a" "s" "d" "f" "j" "k" "l" ";")
  "Home row keys assigned to AI sessions, in allocation order.")

(defvar ai-extras--session-keys (make-hash-table :test 'eq)
  "Map from live AI session buffer to its assigned home-row key.")

(defvar-local ai-extras--display-name-cache nil
  "Cached display name for the modeline.")

(defvar-local ai-extras--waiting-for-input nil
  "Non-nil when this AI session is waiting for user input.
Set to the time (via `current-time') by the notification handler
and cleared when input is sent.")

;;;; Forward declarations

(defvar eat-terminal)
(declare-function eat-self-input "eat" (n &optional e))
(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-set-scrollback-size "eat" (terminal size))
(declare-function alert "alert")

(declare-function consult--read "consult")
(declare-function consult--prefix-group "consult")
(declare-function consult--lookup-cdr "consult")
(declare-function consult-yasnippet--candidates "consult-yasnippet")
(declare-function consult-yasnippet--annotate "consult-yasnippet")

(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")
(declare-function yas--template-key "yasnippet")
(declare-function yas--all-templates "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-active-snippets "yasnippet")
(declare-function yas--commit-snippet "yasnippet")
(declare-function map-values "map")

(defvar yas-minor-mode)
(defvar yas-prompt-functions)
(defvar yas--tables)

;;;; Home-row session keys

(defun ai-extras--purge-dead-session-keys ()
  "Remove entries for buffers that are no longer live."
  (let (dead)
    (maphash (lambda (buf _) (unless (buffer-live-p buf) (push buf dead)))
             ai-extras--session-keys)
    (dolist (buf dead)
      (remhash buf ai-extras--session-keys))))

(defun ai-extras--assign-session-key ()
  "Assign a home-row key to the current AI session buffer."
  (when (ai-extras--detect-backend (current-buffer))
    (unless (gethash (current-buffer) ai-extras--session-keys)
      (ai-extras--purge-dead-session-keys)
      (let ((used (hash-table-values ai-extras--session-keys)))
        (when-let* ((key (cl-find-if (lambda (k) (not (member k used)))
                                      ai-extras--home-row-keys)))
          (puthash (current-buffer) key ai-extras--session-keys))))))

(defun ai-extras--release-session-key ()
  "Release the home-row key for the current buffer."
  (remhash (current-buffer) ai-extras--session-keys))

(defun ai-extras--ensure-all-session-keys ()
  "Ensure every active AI session buffer has a home-row key."
  (ai-extras--purge-dead-session-keys)
  (dolist (buf (ai-extras--find-all-buffers))
    (unless (gethash buf ai-extras--session-keys)
      (let ((used (hash-table-values ai-extras--session-keys)))
        (when-let* ((key (cl-find-if (lambda (k) (not (member k used)))
                                      ai-extras--home-row-keys)))
          (puthash buf key ai-extras--session-keys))))))

(defun ai-extras--home-row-key-index (key)
  "Return the index of KEY in `ai-extras--home-row-keys'."
  (or (cl-position key ai-extras--home-row-keys :test #'string=) 99))

;;;; Display names

(defun ai-extras--buffer-session-name (buffer)
  "Return the session name for BUFFER."
  (ai-extras--session-name (buffer-name buffer)))

(defun ai-extras--qualified-session-name (buffer-name)
  "Return a qualified session name from BUFFER-NAME.
Includes instance name when present for disambiguation."
  (let* ((backend (ai-extras--detect-backend (get-buffer buffer-name)))
         (project (ai-extras--session-name buffer-name))
         (instance (when backend
                     (funcall (ai-extras--backend-get backend :extract-instance-name)
                              buffer-name))))
    (if instance
        (format "%s:%s" project instance)
      project)))

(defun ai-extras-display-name (&optional buffer)
  "Return the display name for BUFFER.
Use the project name alone when it is unique among active sessions,
or \"project:instance\" when multiple sessions share the same
project.  Returns the cached value when available."
  (let ((buf (or buffer (current-buffer))))
    (or (buffer-local-value 'ai-extras--display-name-cache buf)
        (ai-extras--compute-display-name buf))))

(defun ai-extras--compute-display-name (buffer)
  "Compute the display name for BUFFER by scanning active sessions."
  (let* ((name (ai-extras--buffer-session-name buffer))
         (backend (ai-extras--detect-backend buffer))
         (all-bufs (if backend
                       (funcall (ai-extras--backend-get backend :find-all-buffers))
                     (ai-extras--find-all-buffers)))
         (others (cl-remove buffer all-bufs))
         (sibling-names (mapcar #'ai-extras--buffer-session-name others)))
    (if (member name sibling-names)
        (ai-extras--qualified-session-name (buffer-name buffer))
      name)))

(defun ai-extras--refresh-display-names ()
  "Recompute and cache display names for all AI session buffers."
  (dolist (buf (ai-extras--find-all-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq ai-extras--display-name-cache
              (ai-extras--compute-display-name buf))))))

;;;; Session switcher

;;;###autoload
(defun ai-extras-start-or-switch ()
  "Start a new AI session or switch to an existing one.
If no sessions are active, prompt for which backend to start.
If sessions exist, show a transient menu with home-row keys."
  (interactive)
  (let ((all-bufs (ai-extras--find-all-buffers)))
    (if (null all-bufs)
        (ai-extras--start-new-session)
      (ai-extras--ensure-all-session-keys)
      (transient-setup 'ai-extras--session-switcher))))

(defun ai-extras--start-new-session ()
  "Start a new session, prompting for backend if multiple are registered."
  (let ((backends ai-extras-backends))
    (cond
     ((null backends) (user-error "No AI backends registered"))
     ((= (length backends) 1)
      (funcall (plist-get (cdar backends) :start-new)))
     (t
      (let* ((names (mapcar (lambda (e)
                              (cons (or (plist-get (cdr e) :label)
                                        (symbol-name (car e)))
                                    (car e)))
                            backends))
             (choice (completing-read "Backend: " (mapcar #'car names) nil t))
             (backend-sym (cdr (assoc choice names))))
        (funcall (ai-extras--backend-get backend-sym :start-new)))))))

(transient-define-prefix ai-extras--session-switcher ()
  "Switch to an AI session or start a new one."
  [["Sessions"
    :class transient-column
    :setup-children ai-extras--session-switcher-children]
   ["Actions"
    ("w" "jump to waiting" ai-extras-jump-to-waiting)
    ("n" "new session" ai-extras--start-new-session)]])

(defun ai-extras--session-switcher-children (_)
  "Build transient suffixes for the session switcher."
  (let (specs)
    (maphash
     (lambda (buf key)
       (when (buffer-live-p buf)
         (let* ((backend (ai-extras--detect-backend buf))
                (icon (when backend
                        (ai-extras--backend-get backend :icon)))
                (name (ai-extras-display-name buf))
                (label (if icon (format "%s %s" icon name) name))
                (waiting (buffer-local-value
                          'ai-extras--waiting-for-input buf))
                (cmd (make-symbol (format "ai-switch-%s" key)))
                (spec (list key label cmd)))
           (when waiting
             (setq spec (append spec (list :face 'ai-extras-waiting))))
           (fset cmd (lambda () (interactive) (switch-to-buffer buf)))
           (push spec specs))))
     ai-extras--session-keys)
    (setq specs
          (sort specs
                (lambda (a b)
                  (< (ai-extras--home-row-key-index (car a))
                     (ai-extras--home-row-key-index (car b))))))
    (transient-parse-suffixes
     'ai-extras--session-switcher
     (apply #'vector specs))))

;;;; Buffer protection

(defun ai-extras-protect-buffer ()
  "Prompt for confirmation before killing AI session buffers.
Returns t if the buffer should be killed, nil otherwise."
  (or (not ai-extras-protect-buffers)
      (not (ai-extras--detect-backend (current-buffer)))
      (not (process-live-p (get-buffer-process (current-buffer))))
      (yes-or-no-p "Kill AI session buffer? ")))

;;;; Alert and notification system

(defun ai-extras-notify (title message)
  "Show notification with TITLE and MESSAGE.
When `ai-extras-alert-on-ready' is non-nil, dispatch to the
configured alert style."
  (message "%s: %s" title message)
  (when ai-extras-alert-on-ready
    (ai-extras--alert-visual title message)
    (ai-extras--alert-sound)))

(defun ai-extras--alert-visual (title message)
  "Show a visual notification with TITLE and MESSAGE."
  (when (memq ai-extras-alert-style '(visual both))
    (require 'alert nil t)
    (alert message :title title)))

(defun ai-extras--alert-sound ()
  "Play the configured alert sound."
  (when (memq ai-extras-alert-style '(sound both))
    (when-let* ((sound ai-extras-alert-sound)
                ((file-exists-p sound)))
      (start-process "ai-extras-alert-sound" nil "afplay" sound))))

(defun ai-extras--clear-waiting-for-input (&rest _)
  "Clear the waiting-for-input flag in the current buffer."
  (when (bound-and-true-p ai-extras--waiting-for-input)
    (setq ai-extras--waiting-for-input nil)))

;;;###autoload
(defun ai-extras-jump-to-waiting ()
  "Switch to the AI session that most recently started waiting for input."
  (interactive)
  (let (best-buf best-time)
    (dolist (buf (ai-extras--find-all-buffers))
      (when (buffer-live-p buf)
        (let ((ts (buffer-local-value 'ai-extras--waiting-for-input buf)))
          (when (and ts (or (null best-time) (time-less-p best-time ts)))
            (setq best-buf buf best-time ts)))))
    (if best-buf
        (switch-to-buffer best-buf)
      (message "No sessions waiting for input"))))

;;;###autoload
(defun ai-extras-toggle-alert ()
  "Toggle OS notifications for AI sessions."
  (interactive)
  (setq ai-extras-alert-on-ready (not ai-extras-alert-on-ready))
  (message "AI alert notifications %s"
           (if ai-extras-alert-on-ready "enabled" "disabled")))

(defun ai-extras-alert-indicator ()
  "Return a bell icon reflecting the current alert state."
  (if ai-extras-alert-on-ready "🔔" "🔕"))

;;;; Scroll to bottom

(defun ai-extras--scroll-to-bottom (buffer)
  "Scroll BUFFER and its windows to the terminal cursor."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (bound-and-true-p eat-terminal)
        (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
          (goto-char cursor-pos)
          (dolist (window (get-buffer-window-list nil nil t))
            (set-window-point window cursor-pos)
            (with-selected-window window
              (goto-char cursor-pos)
              (recenter -1))))))))

;;;; Terminal rendering fix

(defun ai-extras-fix-rendering ()
  "Send SIGWINCH to fix terminal rendering after startup."
  (interactive)
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (ai-extras--send-sigwinch-after-delay (current-buffer))))

(defun ai-extras--send-sigwinch-after-delay (buffer)
  "Send SIGWINCH to the process in BUFFER after a short delay."
  (run-at-time ai-extras-sigwinch-delay nil
               #'ai-extras--send-sigwinch buffer))

(defun ai-extras--send-sigwinch (buffer)
  "Send SIGWINCH to the process in BUFFER."
  (when (buffer-live-p buffer)
    (when-let* ((proc (get-buffer-process buffer)))
      (signal-process proc 'SIGWINCH))))

;;;; Scrollback truncation fix

(defun ai-extras-disable-scrollback-truncation ()
  "Disable eat's default scrollback limit for the current buffer.
Without this, eat truncates terminal output to
`eat-term-scrollback-size' lines, causing older AI session output
to vanish."
  (interactive)
  (when (and (bound-and-true-p eat-terminal)
             (fboundp 'eat-term-set-scrollback-size))
    (eat-term-set-scrollback-size eat-terminal most-positive-fixnum)))

;;;; Snippet insertion

(defun ai-extras--expand-snippet-to-text (template)
  "Expand yasnippet TEMPLATE to plain text in a temporary buffer."
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((yas-prompt-functions '(yas-no-prompt)))
      (yas-expand-snippet (yas--template-content template)
                          nil nil
                          (yas--template-expand-env template)))
    (mapc #'yas--commit-snippet (yas-active-snippets))
    (buffer-string)))

(defun ai-extras--consult-yasnippet (orig-fn arg)
  "In eat-mode buffers, send snippet content via the terminal.
ORIG-FN is `consult-yasnippet'; ARG is the prefix argument."
  (if (not (derived-mode-p 'eat-mode))
      (funcall orig-fn arg)
    (let* ((candidates
            (consult-yasnippet--candidates
             (if arg
                 (progn (require 'map)
                        (yas--all-templates (map-values yas--tables)))
               (yas--all-templates (yas--get-snippet-tables)))))
           (template
            (consult--read
             candidates
             :prompt "Choose a snippet: "
             :annotate (consult-yasnippet--annotate candidates)
             :lookup 'consult--lookup-cdr
             :require-match t
             :group 'consult--prefix-group
             :category 'yasnippet)))
      (when template
        (let* ((expanded (ai-extras--expand-snippet-to-text template))
               (text (replace-regexp-in-string "\n" "\e\r" expanded)))
          (eat-term-send-string eat-terminal text))))))

(advice-add 'consult-yasnippet :around #'ai-extras--consult-yasnippet)

(defun ai-extras--try-expand-snippet-at-prompt ()
  "Try to expand a yasnippet key at the eat terminal prompt.
Search backward from `point-max' for a prompt marker, extract the
user's input, and check whether it ends with a snippet key.  If
found, erase the key and send the expanded text.  Return non-nil
if a snippet was expanded."
  (when (and (derived-mode-p 'eat-mode)
             (bound-and-true-p eat-terminal)
             (bound-and-true-p yas-minor-mode))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^[❯>$][[:space:]]" nil t)
        (let* ((prompt-start (match-end 0))
               (prompt-end (progn (end-of-line) (point)))
               (input (string-trim-right
                       (buffer-substring-no-properties prompt-start prompt-end)))
               (templates (yas--all-templates (yas--get-snippet-tables)))
               (best-match nil)
               (best-key nil))
          (dolist (template templates)
            (let ((key (yas--template-key template)))
              (when (and key
                         (> (length key) 0)
                         (<= (length key) (length input))
                         (string= key (substring input (- (length input) (length key))))
                         (or (null best-key)
                             (> (length key) (length best-key))))
                (setq best-match template
                      best-key key))))
          (when best-match
            (eat-term-send-string eat-terminal
                                  (make-string (length best-key) ?\x7f))
            (let* ((expanded (ai-extras--expand-snippet-to-text best-match))
                   (text (replace-regexp-in-string "\n" "\e\r" expanded)))
              (eat-term-send-string eat-terminal text))
            t))))))

(defun ai-extras-snippet-tab ()
  "Try snippet expansion at prompt, otherwise send TAB to eat."
  (interactive)
  (unless (ai-extras--try-expand-snippet-at-prompt)
    (eat-self-input 1 ?\t)))

(defvar ai-extras--snippet-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'ai-extras-snippet-tab)
    (define-key map [tab] #'ai-extras-snippet-tab)
    map)
  "Keymap for `ai-extras--snippet-keys-mode'.")

(define-minor-mode ai-extras--snippet-keys-mode
  "Minor mode providing yasnippet TAB expansion in AI session buffers."
  :keymap ai-extras--snippet-keys-mode-map)

(defun ai-extras-setup-snippet-keys ()
  "Enable yasnippet TAB expansion in the current AI session buffer."
  (when (and (ai-extras--detect-backend (current-buffer))
             (bound-and-true-p eat-terminal)
             (require 'yasnippet nil t))
    (yas-minor-mode 1)
    (ai-extras--snippet-keys-mode 1)))

;;;; Escape key fix

(defun ai-extras--send-escape-in-current-buffer (orig-fn)
  "When already in an AI buffer, send escape directly without prompting.
ORIG-FN is the original escape command."
  (if (ai-extras--detect-backend (current-buffer))
      (when (bound-and-true-p eat-terminal)
        (eat-term-send-string eat-terminal (kbd "ESC")))
    (funcall orig-fn)))

;;;; Transient boolean infix class

(defclass ai-extras--boolean-variable (transient-lisp-variable)
  ()
  "A `transient-lisp-variable' that toggles a boolean on each press.")

(cl-defmethod transient-infix-read ((obj ai-extras--boolean-variable))
  "Toggle the boolean value of OBJ."
  (not (oref obj value)))

;;;; Provide

(provide 'ai-extras)
;;; ai-extras.el ends here
