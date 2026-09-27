;;; codex-extras-test.el --- Tests for native browser retention -*- lexical-binding: t -*-

(require 'ert)
(require 'codex-extras)

(defun codex-extras-test--fixture ()
  "Return an owned temporary directory, retained skills root, and config."
  (let* ((temporary (file-truename (make-temp-file "codex-retained-skill-" t)))
         (root (expand-file-name
                (concat ".browser-runtimes/" (make-string 64 ?a) "/assets/") temporary))
         (service (expand-file-name "scripts/browser-service.mjs" root)))
    (dolist (name '("scripts/browser-service.mjs" "scripts/browser-client.mjs"
                    "skills/control-chrome/SKILL.md" ".codex-plugin/plugin.json"))
      (let ((file (expand-file-name name root)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (insert (if (equal name ".codex-plugin/plugin.json")
                      "{\"name\":\"chrome\"}" "fixture")))))
    (list temporary (expand-file-name "skills" root)
          `((mcp_servers
             (node_repl
              (env (NODE_REPL_TRUSTED_SERVICES
                    . ,(json-encode `((browser . ,service)))))))
            (plugins (chrome@openai-bundled (enabled)))))))

(ert-deftest codex-extras-test-marker-only-for-native-child ()
  "The capability is confined to native child process creation."
  (let ((process-environment nil))
    (dolist (backend '(app-server eat vterm))
      (should (equal (codex-extras--make-native-server
                      (lambda (_backend) (getenv "CODEX_RETAIN_BROWSER_SKILL")) backend)
                     (and (eq backend 'app-server) "1"))))
    (should-not (getenv "CODEX_RETAIN_BROWSER_SKILL"))))

(ert-deftest codex-extras-test-unmarked-server-untouched ()
  "Unmarked clients bypass the private config and registration handshake."
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'codex--app-server-send-request)
                 (lambda (&rest _) (ert-fail "Unexpected handshake"))))
        (codex-extras--prepare-browser-skills (lambda () (setq called t)))
        (should called)))))

(ert-deftest codex-extras-test-all-startup-actions-wait-for-roots ()
  "The existing start, resume, fork, and edit dispatch waits for root ACK."
  (pcase-let ((`(,temporary ,root ,config) (codex-extras-test--fixture)))
    (unwind-protect
        (dolist (action '(nil resume resume-session fork fork-session edit-prompt))
          (with-temp-buffer
            (let ((codex--app-server-process 'owned)
                  (codex--app-server-launch-origin
                   '(:environment ("CODEX_RETAIN_BROWSER_SKILL=1")))
                  (codex--app-server-startup-action action)
                  (codex-skill-extra-roots '("/user/skills"))
                  requests started)
              (cl-letf (((symbol-function 'process-live-p) (lambda (p) (eq p 'owned)))
                        ((symbol-function 'codex--app-server-current-cwd) (lambda () "/tmp"))
                        ((symbol-function 'codex--app-server-send-request)
                         (lambda (method params callback) (push (list method params callback) requests)))
                        ((symbol-function 'codex--app-server-send-thread-start)
                         (lambda () (setq started 'start)))
                        ((symbol-function 'codex--app-server-begin-resume)
                         (lambda (method) (setq started method)))
                        ((symbol-function 'codex--app-server-begin-resume-session-id)
                         (lambda (_id &optional method) (setq started (or method "thread/resume"))))
                        ((symbol-function 'codex--app-server-begin-edit-branch)
                         (lambda () (setq started 'edit))))
                (codex-extras--prepare-browser-skills #'codex--app-server-after-initialize)
                (should (equal (caar requests) "config/read"))
                (should-not started)
                (funcall (nth 2 (car requests)) `((config . ,config)) nil)
                (should-not started)
                (should (equal (caar requests) "skills/extraRoots/set"))
                (should (equal (alist-get 'extraRoots (cadar requests))
                               (vector "/user/skills" root)))
                (funcall (nth 2 (car requests)) nil nil)
                (should (equal started (pcase action
                                         ((or 'resume 'resume-session) "thread/resume")
                                         ((or 'fork 'fork-session) "thread/fork")
                                         ('edit-prompt 'edit) (_ 'start))))
                (should (eq codex-extras--browser-startup-state 'ready))))))
      (delete-directory temporary t))))

(ert-deftest codex-extras-test-stale-process-config-ignored ()
  "An obsolete server callback cannot populate the replacement's skill root."
  (with-temp-buffer
    (let ((codex--app-server-process 'first)
          (codex--app-server-launch-origin '(:environment ("CODEX_RETAIN_BROWSER_SKILL=1")))
          callback started)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'codex--app-server-current-cwd) (lambda () "/tmp"))
                ((symbol-function 'codex--app-server-send-request)
                 (lambda (_method _params fn) (setq callback fn))))
        (codex-extras--prepare-browser-skills (lambda () (setq started t)))
        (setq codex--app-server-process 'replacement)
        (funcall callback '((config)) nil)
        (should-not started)
        (should-not codex-extras--retained-browser-skill-root)))))

(ert-deftest codex-extras-test-registration-error-stops-startup ()
  "Failed registration stops startup without printing the error payload."
  (with-temp-buffer
    (let ((codex--app-server-process 'owned)
          (codex-extras--retained-browser-skill-root "/retained/skills")
          callback started status)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'codex--app-server-send-request)
                 (lambda (_method _params fn) (setq callback fn)))
                ((symbol-function 'codex--app-server-insert-status)
                 (lambda (text) (setq status text))))
        (codex-extras--register-browser-skills
         (lambda () (setq started t)) nil (current-buffer) 'owned)
        (funcall callback nil '((message . "PRIVATE ERROR PAYLOAD")))
        (should-not started)
        (should (eq codex-extras--browser-startup-state 'failed))
        (should-not (string-match-p "PRIVATE" status))))))

(ert-deftest codex-extras-test-refresh-preserves-pin-and-user-settings ()
  "Later registration merges current user roots without mutating settings."
  (let ((codex-skill-extra-roots '("/user/skills"))
        (codex-extras--retained-browser-skill-root "/retained/skills"))
    (should (equal (codex-extras--merge-browser-skill-root
                    (lambda () codex-skill-extra-roots))
                   '("/user/skills" "/retained/skills")))
    (should (equal codex-skill-extra-roots '("/user/skills")))))

(ert-deftest codex-extras-test-validation-fails-closed ()
  "Incomplete retained bundles and unsuppressed plugins are rejected."
  (pcase-let ((`(,temporary ,root ,config) (codex-extras-test--fixture)))
    (unwind-protect
        (progn
          (should (equal (codex-extras--retained-browser-root config) root))
          (setf (alist-get 'enabled (alist-get 'chrome@openai-bundled (alist-get 'plugins config))) t)
          (should-error (codex-extras--retained-browser-root config))
          (setf (alist-get 'enabled (alist-get 'chrome@openai-bundled (alist-get 'plugins config))) nil)
          (delete-file (expand-file-name "control-chrome/SKILL.md" root))
          (should-error (codex-extras--retained-browser-root config)))
      (delete-directory temporary t))))

(ert-deftest codex-extras-test-ordinary-disabled-config ()
  "Intentionally disabled Chrome and ordinary runtimes require no pin."
  (should-not (codex-extras--retained-browser-root
               '((plugins (chrome@openai-bundled (enabled)))))))

(ert-deftest codex-extras-test-config-error-stops-startup ()
  "Configuration errors stop the handshake without leaking their payload."
  (with-temp-buffer
    (let ((codex--app-server-process 'owned)
          (codex--app-server-launch-origin '(:environment ("CODEX_RETAIN_BROWSER_SKILL=1")))
          callback started status)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'codex--app-server-current-cwd) (lambda () "/tmp"))
                ((symbol-function 'codex--app-server-send-request)
                 (lambda (_method _params fn) (setq callback fn)))
                ((symbol-function 'codex--app-server-insert-status)
                 (lambda (text) (setq status text))))
        (codex-extras--prepare-browser-skills (lambda () (setq started t)))
        (funcall callback nil '((message . "PRIVATE ERROR PAYLOAD")))
        (should-not started)
        (should (eq codex-extras--browser-startup-state 'failed))
        (should-not (string-match-p "PRIVATE" status))))))

(ert-deftest codex-extras-test-ordinary-startup-error-preserved ()
  "An ordinary startup error is not mislabeled as a browser failure."
  (with-temp-buffer
    (let ((codex--app-server-process 'owned)
          (codex--app-server-launch-origin '(:environment ("CODEX_RETAIN_BROWSER_SKILL=1")))
          callback status)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'codex--app-server-current-cwd) (lambda () "/tmp"))
                ((symbol-function 'codex--app-server-send-request)
                 (lambda (_method _params fn) (setq callback fn)))
                ((symbol-function 'codex--app-server-insert-status)
                 (lambda (text) (setq status text))))
        (codex-extras--prepare-browser-skills (lambda () (error "Ordinary startup failure")))
        (should-error (funcall callback '((config)) nil))
        (should-not status)))))

(ert-deftest codex-extras-test-enable-idempotent ()
  "Repeated setup installs each advice once."
  (unwind-protect
      (progn
        (codex-extras-enable-retained-browser-skills)
        (codex-extras-enable-retained-browser-skills)
        (let ((count 0))
          (advice-mapc (lambda (fn _props)
                         (when (eq fn #'codex-extras--prepare-browser-skills)
                           (cl-incf count)))
                       'codex--app-server-after-initialize)
          (should (= count 1))))
    (advice-remove 'codex--term-make #'codex-extras--make-native-server)
    (advice-remove 'codex--app-server-after-initialize #'codex-extras--prepare-browser-skills)
    (advice-remove 'codex--app-server-send-skill-extra-roots #'codex-extras--merge-browser-skill-root)))

(provide 'codex-extras-test)
;;; codex-extras-test.el ends here
