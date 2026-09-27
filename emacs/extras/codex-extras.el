;;; codex-extras.el --- Extensions for Codex -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/codex-extras.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (codex "0.1"))

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

;; Private integration between the dotfiles Codex launcher and native sessions.

;;; Code:

(require 'codex)
(require 'cl-lib)
(require 'json)

(defvar-local codex-extras--retained-browser-skill-root nil
  "Immutable Chrome skill root selected for this native server process.")

(defvar-local codex-extras--browser-startup-process nil
  "Server process whose retained browser startup is being prepared.")

(defvar-local codex-extras--browser-startup-state nil
  "Retained browser startup state: nil, pending, ready, or failed.")

;;;###autoload
(defun codex-extras-enable-retained-browser-skills ()
  "Enable the private launcher handshake for native Codex sessions.
Existing sessions are unchanged.  New native servers advertise support
for registering immutable Chrome skills before starting their thread."
  (advice-add 'codex--term-make :around #'codex-extras--make-native-server)
  (advice-add 'codex--app-server-after-initialize
              :around #'codex-extras--prepare-browser-skills)
  (advice-add 'codex--app-server-send-skill-extra-roots
              :around #'codex-extras--merge-browser-skill-root))

(defun codex-extras--make-native-server (original backend &rest arguments)
  "Call ORIGINAL with BACKEND and ARGUMENTS, advertising native support."
  (let ((process-environment (copy-sequence process-environment)))
    (when (eq backend 'app-server)
      (setenv "CODEX_RETAIN_BROWSER_SKILL" "1"))
    (apply original backend arguments)))

(defun codex-extras--prepare-browser-skills (original &rest arguments)
  "Gate ORIGINAL startup with ARGUMENTS on retained skill registration."
  (if (not (member "CODEX_RETAIN_BROWSER_SKILL=1"
                   (plist-get codex--app-server-launch-origin :environment)))
      (apply original arguments)
    (unless (eq codex-extras--browser-startup-process codex--app-server-process)
      (setq codex-extras--browser-startup-process codex--app-server-process
            codex-extras--browser-startup-state nil
            codex-extras--retained-browser-skill-root nil))
    (pcase codex-extras--browser-startup-state
      ('ready (apply original arguments))
      ((or 'pending 'failed) nil)
      (_ (codex-extras--read-browser-startup-config original arguments)))))

(defun codex-extras--read-browser-startup-config (original arguments)
  "Read retained runtime configuration before ORIGINAL with ARGUMENTS."
  (let ((buffer (current-buffer))
        (process codex--app-server-process))
    (setq codex-extras--browser-startup-state 'pending)
    (codex--app-server-send-request
     "config/read"
     `((cwd . ,(codex--app-server-current-cwd)) (includeLayers . :json-false))
     (lambda (result error)
       (when (codex-extras--current-server-p buffer process)
         (with-current-buffer buffer
           (let ((valid
                  (condition-case nil
                      (progn
                        (when (or error (not (assq 'config result)))
                          (error "Startup configuration is unavailable"))
                        (setq codex-extras--retained-browser-skill-root
                              (codex-extras--retained-browser-root
                               (alist-get 'config result)))
                        t)
                    (error (codex-extras--browser-startup-failed) nil))))
             (when valid
               (codex-extras--register-browser-skills
                original arguments buffer process)))))))))

(defun codex-extras--current-server-p (buffer process)
  "Return non-nil when BUFFER still owns the live PROCESS."
  (and (buffer-live-p buffer)
       (process-live-p process)
       (eq process (buffer-local-value 'codex--app-server-process buffer))))

(defun codex-extras--register-browser-skills (original arguments buffer process)
  "Register skills before ORIGINAL with ARGUMENTS in BUFFER for PROCESS."
  (if (not codex-extras--retained-browser-skill-root)
      (progn
        (setq codex-extras--browser-startup-state 'ready)
        (apply original arguments))
    (codex--app-server-send-request
     "skills/extraRoots/set"
     `((extraRoots . ,(vconcat (codex-extras--browser-skill-roots))))
     (lambda (_result error)
       (when (codex-extras--current-server-p buffer process)
         (with-current-buffer buffer
           (if error
               (codex-extras--browser-startup-failed)
             (setq codex-extras--browser-startup-state 'ready)
             (apply original arguments))))))))

(defun codex-extras--browser-startup-failed ()
  "Stop thread startup after a retained browser handshake failure."
  (setq codex-extras--browser-startup-state 'failed)
  (codex--app-server-insert-status
   "Retained browser skill startup failed; thread startup stopped"))

(defun codex-extras--merge-browser-skill-root (original &rest arguments)
  "Call ORIGINAL with ARGUMENTS while retaining the process skill root."
  (let ((codex-skill-extra-roots (codex-extras--browser-skill-roots)))
    (apply original arguments)))

(defun codex-extras--browser-skill-roots ()
  "Return user skill roots together with this process's retained root."
  (delete-dups
   (mapcar #'expand-file-name
           (append codex-skill-extra-roots
                   (when codex-extras--retained-browser-skill-root
                     (list codex-extras--retained-browser-skill-root))))))

(defun codex-extras--retained-browser-root (config)
  "Return CONFIG's retained browser skill root, or nil for ordinary runtimes.
Validate the launcher-selected bundle and require its mutable plugin to be
disabled for this process.  Never expose CONFIG or its MCP environment."
  (let* ((server (alist-get 'node_repl (alist-get 'mcp_servers config)))
         (services-json (alist-get 'NODE_REPL_TRUSTED_SERVICES (alist-get 'env server)))
         (services (and (stringp services-json)
                        (json-parse-string services-json :object-type 'alist)))
         (service (alist-get 'browser services)))
    (when (and (stringp service) (string-match-p "/\\.browser-runtimes/" service))
      (unless (and (file-name-absolute-p service)
                   (string-match-p
                    "/\\.browser-runtimes/[[:xdigit:]]\\{64\\}/assets/scripts/browser-service\\.mjs\\'"
                    service))
        (error "Invalid retained browser service path"))
      (let* ((root (file-name-directory (directory-file-name (file-name-directory service))))
             (skills (expand-file-name "skills" root))
             (manifest (expand-file-name ".codex-plugin/plugin.json" root))
             (plugin (alist-get 'chrome@openai-bundled (alist-get 'plugins config)))
             (metadata (with-temp-buffer
                         (insert-file-contents manifest)
                         (json-parse-buffer :object-type 'alist))))
        (unless (and (assq 'enabled plugin) (null (alist-get 'enabled plugin))
                     (equal (alist-get 'name metadata) "chrome")
                     (file-regular-p service)
                     (file-regular-p (expand-file-name "scripts/browser-client.mjs" root))
                     (file-regular-p (expand-file-name "control-chrome/SKILL.md" skills))
                     (equal (directory-file-name root) (directory-file-name (file-truename root))))
          (error "Retained browser bundle is incomplete or not selected"))
        skills))))

(provide 'codex-extras)
;;; codex-extras.el ends here
