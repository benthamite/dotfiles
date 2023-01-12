;;; Compiled snippets and support files for `snippets'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("README.md" "# Private directory for Yasnippets snippets\n\nThe content of this directory is ignored by Git. This is the default place\nwhere to store your private yasnippets.\n\nThis path will be loaded automatically and used whenever Yasnippets loads.\n" "README.md" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/README.md" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("guk" "(global-unset-key (kbd \"$1\"))" "global-unset-key" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/emacs-lisp-mode/global-unset-key" nil nil)
                       ("gsk" "  (global-set-key (kbd \"$1\") '$2)" "global-set-key" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/emacs-lisp-mode/global-set-key" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("pyi" "src_python{return($0)}" "python-inline" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/python-inline" nil nil)
                       ("org-lib" "[[file:~/Google Drive/Library/.pdf]]" "org-library" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/org-library" nil nil)
                       ("org-img" "#+attr_html: :width 800px\n[[file:~/Google Drive/Pictures/Emacs/.png]]" "org-image" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/org-image" nil nil)
                       ("mpass" "Updating to account for passage of time." "metaculus-time" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-time" "C-s-t" nil)
                       ("msheet" "([[`(current-kill 0)`][source]])" "metaculus-source" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-source" "C-s-e" nil)
                       ("msheet" "[[`(current-kill 0)`][spreadsheet]]" "metaculus-sheet" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-sheet" "C-s-s" nil)
                       ("mst" "Sticking to previous prediction." "metaculus-previous" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-previous" "C-s-p" nil)
                       ("mpost" "/Post-mortem:/ " "metaculus-post" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-post" "C-s-m" nil)
                       ("mfinal" "/Final prediction/: " "metaculus-final" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-final" "C-s-f" nil)
                       ("mdef" "Deferring to the community." "metaculus-defer" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/metaculus-defer" "C-s-u" nil)
                       ("laplace-range" "# check that the formula is correct\n# snippet has to be fixed\n#+begin_src python :results output\nfrom datetime import date\nbegins = date($0,$1,$2)\ntoday = date.today()\nfirstQ = begins - 1 + (sofar + 1)/.75\nthirdQ = begins - 1 + (sofar + 1)/.25\nprint(firstQ.days)\n#+end_src\n" "laplace-range" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/laplace-range" nil nil)
                       ("laplace" "#+begin_src python :results output\nfrom datetime import date\nbegins = date($0,$1,$2)\nends = date($3,$4,$5)\ntoday = date.today()\nsofar = today - begins\nahead = ends - today\nprob_status_quo = (sofar.days + 1) / (sofar.days + ahead.days + 1)\nprint(prob_status_quo)\n#+end_src\n" "laplace" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/laplace" nil nil)
                       ("eal" "[[*${1:$(my/capitalize-first-char yas-text)}][${1:}]] " "ealink" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/ealink" "H-M-e" nil)
                       ("/comm" "#+BEGIN_COMMENT\n${1:}\n#+END_COMMENT\n" "comment-block" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/comment-block" "C-s-c" nil)
                       ("cl_" "#+begin_src clojure :tangle yes\n$0\n#+end_src" "clojure" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/clojure" nil nil)
                       ("bibliography" "** Bibliography\n" "bibliography" nil nil nil "/Users/pablostafforini/.emacs.d/private/snippets/snippets/org-mode/bibliography" "C-s-b" nil)))


;;; Do not edit! File generated at Thu Feb  4 21:08:48 2021
