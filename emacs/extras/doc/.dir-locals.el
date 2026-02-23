((org-mode . ((eval . (add-hook 'after-save-hook
                                (lambda ()
                                  (require 'ox-texinfo)
                                  (let ((inhibit-message t))
                                    (org-texinfo-export-to-texinfo)))
                                nil t)))))
