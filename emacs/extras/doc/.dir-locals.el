((org-mode . ((eval . (add-hook 'after-save-hook
                                #'org-extras-export-manual-to-texinfo
                                nil t)))))
