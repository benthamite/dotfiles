;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((elisp-mode . ((eval . (when (boundp flycheck-disabled-checkers)
			  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))))
