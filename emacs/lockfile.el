((abbrev-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/abbrev-extras.el") :package
			 "abbrev-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (ace-link :source "elpaca-menu-lock-file" :recipe
	   (:package "ace-link" :repo "abo-abo/ace-link" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "d9bd4a25a02bdfde4ea56247daf3a9ff15632ea4"))
 (ace-link-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/ace-link-extras.el") :package
			   "ace-link-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (activity-watch-mode :source "elpaca-menu-lock-file" :recipe
		      (:package "activity-watch-mode" :fetcher github :repo
				"pauldub/activity-watch-mode" :files
				("*.el" "*.el.in" "dir" "*.info" "*.texi"
				 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				 "doc/*.texinfo" "lisp/*.el" "docs/dir"
				 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
				 (:exclude ".dir-locals.el" "test.el" "tests.el"
					   "*-test.el" "*-tests.el" "LICENSE"
					   "README*" "*-pkg.el"))
				:source "MELPA" :protocol https :inherit t
				:depth treeless :ref
				"19aed6ca81a3b1e549f47867c924d180d8536791"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
		    (:package "aggressive-indent" :repo
			      "Malabarba/aggressive-indent-mode" :fetcher github
			      :files
			      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			       "doc/*.texinfo" "lisp/*.el" "docs/dir"
			       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			       (:exclude ".dir-locals.el" "test.el" "tests.el"
					 "*-test.el" "*-tests.el" "LICENSE"
					 "README*" "*-pkg.el"))
			      :source "MELPA" :protocol https :inherit t :depth
			      treeless :ref
			      "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (aidermacs :source "elpaca-menu-lock-file" :recipe
	    (:package "aidermacs" :fetcher github :repo "MatthewZMD/aidermacs"
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "6b33f2ceb49b1832325e2fbfbbcb72e9df5e9128"))
 (aidermacs-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/aidermacs-extras.el") :package
			    "aidermacs-extras" :ref
			    "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
		("aio.el" "README.md" "UNLICENSE") :source "MELPA" :protocol
		https :inherit t :depth treeless :ref
		"da93523e235529fa97d6f251319d9e1d6fc24a41"))
 (alert :source "elpaca-menu-lock-file" :recipe
	(:package "alert" :fetcher github :repo "jwiegley/alert" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "d17ad05ade019fd3560c78f8ed57a352034797e8"))
 (anaphora :source "elpaca-menu-lock-file" :recipe
	   (:package "anaphora" :repo "rolandwalker/anaphora" :fetcher github
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "a755afa7db7f3fa515f8dd2c0518113be0b027f6"))
 (annas-archive :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth treeless :host
			 github :repo "benthamite/annas-archive" :package
			 "annas-archive" :ref
			 "54eb6b251bc68f302dda0a9cde92a5b435970301"))
 (applescript-mode :source "elpaca-menu-lock-file" :recipe
		   (:package "applescript-mode" :fetcher github :repo
			     "emacsorphanage/applescript-mode" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "67c68afc479581ecb6163371523435f244f5ec92"))
 (async :source "elpaca-menu-lock-file" :recipe
	(:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "dfbd8796ded77e61e3c58c6da083a1753bc7bcb0"))
 (atomic-chrome :source "elpaca-menu-lock-file" :recipe
		(:package "atomic-chrome" :repo "alpha22jp/atomic-chrome"
			  :fetcher github :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "072a137a19d7e6a300ca3e87c0e142a7f4ccb5fb"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (avy-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth nil :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/avy-extras.el") :package "avy-extras" :ref
		      "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (back-button :source "elpaca-menu-lock-file" :recipe
	      (:package "back-button" :repo "rolandwalker/back-button" :fetcher
			github :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "f8783c98a7fefc1d0419959c1b462c7dcadce5a8"))
 (bbdb :source "elpaca-menu-lock-file" :recipe
       (:package "bbdb" :fetcher git :url
		 "https://git.savannah.nongnu.org/git/bbdb.git" :files
		 (:defaults "lisp/*.el") :source "MELPA" :protocol https
		 :inherit t :depth treeless :host github :repo
		 "emacsmirror/bbdb" :pre-build
		 (("./autogen.sh") ("./configure") ("make")) :build
		 (:not elpaca--compile-info) :ref
		 "53e8ba04c47b3542db75b68f9663941daf2e6ca4"))
 (bbdb-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/bbdb-extras.el") :package "bbdb-extras"
		       :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (bbdb-vcard :source "elpaca-menu-lock-file" :recipe
	     (:package "bbdb-vcard" :repo "tohojo/bbdb-vcard" :fetcher github
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "113c66115ce68316e209f51ebce56de8dded3606"))
 (bib :source "elpaca-menu-lock-file" :recipe
      (:source nil :protocol https :inherit t :depth nil :host github :repo
	       "benthamite/bib" :package "bib" :ref
	       "9b927eb75331f07557f395954eba1f29a4c3b4a3"))
 (biblio :source "elpaca-menu-lock-file" :recipe
	 (:package "biblio" :repo "cpitclaudel/biblio.el" :fetcher github :files
		   (:defaults (:exclude "biblio-core.el")) :source "MELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "0314982c0ca03d0f8e0ddbe9fc20588c35021098"))
 (biblio-core :source "elpaca-menu-lock-file" :recipe
	      (:package "biblio-core" :repo "cpitclaudel/biblio.el" :fetcher
			github :files ("biblio-core.el") :source "MELPA"
			:protocol https :inherit t :depth treeless :ref
			"0314982c0ca03d0f8e0ddbe9fc20588c35021098"))
 (bibtex-completion :source "elpaca-menu-lock-file" :recipe
		    (:package "bibtex-completion" :fetcher github :repo
			      "tmalsburg/helm-bibtex" :files
			      ("bibtex-completion.el") :source "MELPA" :protocol
			      https :inherit t :depth treeless :version
			      (lambda (_) "2.0.0") :ref
			      "6064e8625b2958f34d6d40312903a85c173b5261"))
 (bibtex-completion-extras :source "elpaca-menu-lock-file" :recipe
			   (:source nil :protocol https :inherit t :depth nil
				    :host github :repo "benthamite/dotfiles"
				    :files
				    ("emacs/extras/bibtex-completion-extras.el")
				    :package "bibtex-completion-extras" :ref
				    "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (bibtex-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/bibtex-extras.el") :package
			 "bibtex-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (breadcrumb :source "elpaca-menu-lock-file" :recipe
	     (:package "breadcrumb" :repo
		       ("https://github.com/joaotavora/breadcrumb"
			. "breadcrumb")
		       :files ("*" (:exclude ".git")) :source "GNU ELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "da34d030e6d01db2bba45b30080204b23a714c9f"))
 (browse-url-extras :source "elpaca-menu-lock-file" :recipe
		    (:source nil :protocol https :inherit t :depth nil :host
			     github :repo "benthamite/dotfiles" :files
			     ("emacs/extras/browse-url-extras.el") :package
			     "browse-url-extras" :ref
			     "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (calendar-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/calendar-extras.el") :package
			   "calendar-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (calfw :source "elpaca-menu-lock-file" :recipe
	(:package "calfw" :repo "benthamite/emacs-calfw" :fetcher github :files
		  ("calfw.el") :source "MELPA" :protocol https :inherit t :depth
		  treeless :host github :ref
		  "3a4e1edd423636443ab08a386ed33c8ef1f14b4f"))
 (calfw-blocks :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "benthamite/calfw-blocks" :package
			"calfw-blocks" :ref
			"96ba30067a94249ee073e6c1754c6bda696bbd74"))
 (calfw-org :source "elpaca-menu-lock-file" :recipe
	    (:package "calfw-org" :repo "kiwanami/emacs-calfw" :fetcher github
		      :files ("calfw-org.el") :source "MELPA" :protocol https
		      :inherit t :depth treeless :ref
		      "03abce97620a4a7f7ec5f911e669da9031ab9088"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "f925182957ea11119e27c2ac7200cc1fbba5612a"))
 (casual :source "elpaca-menu-lock-file" :recipe
	 (:package "casual" :fetcher github :repo "kickingvegas/casual"
		   :old-names
		   (casual-agenda casual-bookmarks casual-calc casual-dired
				  casual-editkit casual-ibuffer casual-info
				  casual-isearch cc-isearch-menu casual-lib
				  casual-re-builder)
		   :files (:defaults "docs/images") :source "MELPA" :protocol
		   https :inherit t :depth treeless :ref
		   "4f14eac613e45cf2d7f5412971f9aad2e3589e22"))
 (circe :source "elpaca-menu-lock-file" :recipe
	(:package "circe" :repo "emacs-circe/circe" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "a0aada8cbb381228c80d98b81ebdec30012c512e"))
 (citar :source "elpaca-menu-lock-file" :recipe
	(:package "citar" :repo "emacs-citar/citar" :fetcher github :files
		  (:defaults (:exclude "citar-embark.el")) :old-names
		  (bibtex-actions) :source "MELPA" :protocol https :inherit t
		  :depth treeless :host github :includes (citar-org) :ref
		  "5ef097beba280cb0a7e7496a6f6a3b7f612c2fe2"))
 (citar-embark :source "elpaca-menu-lock-file" :recipe
	       (:package "citar-embark" :repo "emacs-citar/citar" :fetcher
			 github :files ("citar-embark.el") :source "MELPA"
			 :protocol https :inherit t :depth treeless :ref
			 "5ef097beba280cb0a7e7496a6f6a3b7f612c2fe2"))
 (citar-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/citar-extras.el") :package "citar-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (citar-org-roam :source "elpaca-menu-lock-file" :recipe
		 (:package "citar-org-roam" :repo "emacs-citar/citar-org-roam"
			   :fetcher github :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el" "tests.el"
				      "*-test.el" "*-tests.el" "LICENSE"
				      "README*" "*-pkg.el"))
			   :source "MELPA" :protocol https :inherit t :depth
			   treeless :host github :ref
			   "9750cfbbf330ab3d5b15066b65bd0a0fe7c296fb"))
 (citeproc :source "elpaca-menu-lock-file" :recipe
	   (:package "citeproc" :fetcher github :repo
		     "andras-simonyi/citeproc-el" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "f36650536b64706f16752c4b004a87da0c98b211"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "clojure-mode" :repo "clojure-emacs/clojure-mode"
			 :fetcher github :files ("clojure-mode.el") :source
			 "MELPA" :protocol https :inherit t :depth treeless :ref
			 "28dc02114ae70db6bb68d537ea77985f272120bc"))
 (closql :source "elpaca-menu-lock-file" :recipe
	 (:package "closql" :fetcher github :repo "magit/closql" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :host github :ref "05a2b048fd4e5c90aa971479cb9e71cf9aeba2bf"))
 (codel :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth treeless :host github
		 :repo "ultronozm/codel.el" :package "codel" :ref
		 "fe53410cf487ae255043a3c29be7a09437b84821"))
 (color-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/color-extras.el") :package "color-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (company :source "elpaca-menu-lock-file" :recipe
	  (:package "company" :fetcher github :repo "company-mode/company-mode"
		    :files
		    (:defaults "icons" ("images/small" "doc/images/small/*.png"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "1924eabfa7438974da0500e85fff5fb32c27282c"))
 (consult :source "elpaca-menu-lock-file" :recipe
	  (:package "consult" :repo "minad/consult" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "d32cccdcb5559b503306291fa9f52df19594d687"))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
	      (:package "consult-dir" :fetcher github :repo
			"karthink/consult-dir" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "4532b8d215d16b0159691ce4dee693e72d71e0ff"))
 (consult-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/consult-extras.el") :package
			  "consult-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (consult-flycheck :source "elpaca-menu-lock-file" :recipe
		   (:package "consult-flycheck" :fetcher github :repo
			     "minad/consult-flycheck" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "86dcb0bf2897be0f2b915ade96b1915ab23c3ee9"))
 (consult-git-log-grep :source "elpaca-menu-lock-file" :recipe
		       (:package "consult-git-log-grep" :fetcher github :repo
				 "ghosty141/consult-git-log-grep" :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				  "*.texinfo" "doc/dir" "doc/*.info"
				  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				  "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el"
					    "tests.el" "*-test.el" "*-tests.el"
					    "LICENSE" "README*" "*-pkg.el"))
				 :source "MELPA" :protocol https :inherit t
				 :depth treeless :ref
				 "5b1669ebaff9a91000ea185264cfcb850885d21f"))
 (consult-todo :source "elpaca-menu-lock-file" :recipe
	       (:package "consult-todo" :fetcher github :repo
			 "eki3z/consult-todo" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :ref
			 "f9ba063a6714cb95ddbd886786ada93771f3c140"))
 (consult-yasnippet :source "elpaca-menu-lock-file" :recipe
		    (:package "consult-yasnippet" :fetcher github :repo
			      "mohkale/consult-yasnippet" :files
			      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			       "doc/*.texinfo" "lisp/*.el" "docs/dir"
			       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			       (:exclude ".dir-locals.el" "test.el" "tests.el"
					 "*-test.el" "*-tests.el" "LICENSE"
					 "README*" "*-pkg.el"))
			      :source "MELPA" :protocol https :inherit t :depth
			      treeless :ref
			      "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
 (copilot :source "elpaca-menu-lock-file" :recipe
	  (:package "copilot" :fetcher github :repo "copilot-emacs/copilot.el"
		    :files ("dist" "*.el") :source "MELPA" :protocol https
		    :inherit t :depth treeless :host github :ref
		    "4f51b3c21c42756d09ee17011201ea7d6e18ff69"))
 (copilot-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/copilot-extras.el") :package
			  "copilot-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (corfu :source "elpaca-menu-lock-file" :recipe
	(:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/*")
		  :fetcher github :source "MELPA" :protocol https :inherit t
		  :depth treeless :includes
		  (corfu-info corfu-echo corfu-history corfu-popupinfo
			      corfu-quick)
		  :ref "5e9a2229322541c1ee28b72128cf7c097b32dd5d"))
 (corfu-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/corfu-extras.el") :package "corfu-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (corg :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth treeless :host github
		:repo "isamert/corg.el" :package "corg" :ref
		"79af293aa95936274d7b3a6ba13d4b66a3a6ee76"))
 (creole :source "elpaca-menu-lock-file" :recipe
	 (:package "creole" :fetcher github :repo "nicferrier/elwikicreole"
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "7d5cffe93857f6c75ca09ac79c0e47b8d4410e53"))
 (crux :source "elpaca-menu-lock-file" :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "3b72275fce66162770b53cf72eb72515c3e68492"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "csv-mode" :repo
		     ("https://github.com/emacsmirror/gnu_elpa" . "csv-mode")
		     :branch "externals/csv-mode" :files ("*" (:exclude ".git"))
		     :source "GNU ELPA" :protocol https :inherit t :depth
		     treeless :ref "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (ct :source "elpaca-menu-lock-file" :recipe
     (:package "ct" :fetcher github :repo "neeasade/ct.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "e3d082136e06c0ec777ab032bec5a785239f412b"))
 (curl-to-elisp :source "elpaca-menu-lock-file" :recipe
		(:package "curl-to-elisp" :fetcher github :repo
			  "xuchunyang/curl-to-elisp" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "63d8d9c6d5efb8af8aa88042bfc0690ba699ef64"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
		 ("dash.el" "dash.texi") :source "MELPA" :protocol https
		 :inherit t :depth treeless :ref
		 "fcb5d831fc08a43f984242c7509870f30983c27c"))
 (db :source "elpaca-menu-lock-file" :recipe
     (:package "db" :fetcher github :repo "nicferrier/emacs-db" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "b3a423fb8e72f9013009cbe033d654df2ce31438"))
 (deferred :source "elpaca-menu-lock-file" :recipe
	   (:package "deferred" :repo "kiwanami/emacs-deferred" :fetcher github
		     :files ("deferred.el") :source "MELPA" :protocol https
		     :inherit t :depth treeless :ref
		     "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (dired-du :source "elpaca-menu-lock-file" :recipe
	   (:package "dired-du" :repo
		     ("https://github.com/emacsmirror/gnu_elpa" . "dired-du")
		     :branch "externals/dired-du" :files ("*" (:exclude ".git"))
		     :source "GNU ELPA" :protocol https :inherit t :depth
		     treeless :ref "f7e1593e94388b0dfb71af8e9a3d5d07edf5a159"))
 (dired-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/dired-extras.el") :package "dired-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (dired-git-info :source "elpaca-menu-lock-file" :recipe
		 (:package "dired-git-info" :repo
			   ("https://github.com/clemera/dired-git-info"
			    . "dired-git-info")
			   :files ("*" (:exclude ".git")) :source "GNU ELPA"
			   :protocol https :inherit t :depth treeless :ref
			   "91d57e3a4c5104c66a3abc18e281ee55e8979176"))
 (dired-hacks :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "Fuco1/dired-hacks" :package "dired-hacks"
		       :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-quick-sort :source "elpaca-menu-lock-file" :recipe
		   (:package "dired-quick-sort" :repo "xuhdev/dired-quick-sort"
			     :fetcher gitlab :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "611acc82919e99ac37ce504934f5e8c605ad7efa"))
 (djvu :source "elpaca-menu-lock-file" :recipe
       (:package "djvu" :repo
		 ("https://github.com/emacsmirror/gnu_elpa" . "djvu") :branch
		 "externals/djvu" :files ("*" (:exclude ".git")) :source
		 "GNU ELPA" :protocol https :inherit t :depth treeless :ref
		 "1251c94f85329de9f957408d405742023f6c50e2"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
		(:package "doom-modeline" :repo "seagle0128/doom-modeline"
			  :fetcher github :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :build (:not elpaca--check-version) :ref
			  "cb703c217e8eb4d6f853da7fca9f1be91d985642"))
 (doom-modeline-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :protocol https :inherit t :depth nil :host
				github :repo "benthamite/dotfiles" :files
				("emacs/extras/doom-modeline-extras.el")
				:package "doom-modeline-extras" :ref
				"805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (dwim-shell-command :source "elpaca-menu-lock-file" :recipe
		     (:package "dwim-shell-command" :fetcher github :repo
			       "xenodium/dwim-shell-command" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi" "docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
					  "*-test.el" "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https :inherit t :depth
			       treeless :host github :ref
			       "4b077432a94873e5f505c8f569743cfd984eebb1"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo "akib/emacs-eat" :files
		("*.el" ("term" "term/*.el") "*.texi" "*.ti"
		 ("terminfo/e" "terminfo/e/*") ("terminfo/65" "terminfo/65/*")
		 ("integration" "integration/*")
		 (:exclude ".dir-locals.el" "*-tests.el"))
		:source "NonGNU ELPA" :protocol https :inherit t :depth treeless
		:host codeberg :ref "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (ebib :source "elpaca-menu-lock-file" :recipe
       (:package "ebib" :fetcher github :repo "joostkremers/ebib" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "01a6b41c1e1034ee094b38d1b072500c5ae3ef67"))
 (ebib-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/ebib-extras.el") :package "ebib-extras"
		       :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (edit-indirect :source "elpaca-menu-lock-file" :recipe
		(:package "edit-indirect" :fetcher github :repo
			  "Fanael/edit-indirect" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "82a28d8a85277cfe453af464603ea330eae41c05"))
 (ein :source "elpaca-menu-lock-file" :recipe
      (:package "ein" :repo "millejoh/emacs-ipython-notebook" :fetcher github
		:files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"271136654631d42105164163fff3d8ceec4c5e40"))
 (el-patch :source "elpaca-menu-lock-file" :recipe
	   (:package "el-patch" :fetcher github :repo "radian-software/el-patch"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "e10392847cb4d9d32d614a5678007aac4c2cca92"))
 (elfeed :source "elpaca-menu-lock-file" :recipe
	 (:package "elfeed" :repo "skeeto/elfeed" :fetcher github :files
		   (:defaults "README.md") :source "MELPA" :protocol https
		   :inherit t :depth treeless :ref
		   "a39fb78e34ee25dc8baea83376f929d7c128344f"))
 (elfeed-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/elfeed-extras.el") :package
			 "elfeed-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (elfeed-org :source "elpaca-menu-lock-file" :recipe
	     (:package "elfeed-org" :repo "remyhonig/elfeed-org" :fetcher github
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "34c0b4d758942822e01a5dbe66b236e49a960583"))
 (elfeed-tube :source "elpaca-menu-lock-file" :recipe
	      (:package "elfeed-tube" :repo "karthink/elfeed-tube" :fetcher
			github :files
			("elfeed-tube.el" "elfeed-tube-utils.el"
			 "elfeed-tube-fill.el")
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "f4a84fa259472b217d7e3ce18c816626d397ba92"))
 (elfeed-tube-mpv :source "elpaca-menu-lock-file" :recipe
		  (:package "elfeed-tube-mpv" :repo "karthink/elfeed-tube"
			    :fetcher github :files ("elfeed-tube-mpv.el")
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :ref
			    "f4a84fa259472b217d7e3ce18c816626d397ba92"))
 (elgantt :source "elpaca-menu-lock-file" :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github
		   :repo "legalnonsense/elgantt" :package "elgantt" :ref
		   "23fe6a3dd4f1a991e077f13869fb960b8b29e183"))
 (elgrep :source "elpaca-menu-lock-file" :recipe
	 (:package "elgrep" :repo "TobiasZawada/elgrep" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "329eaf2e9e994e5535c7f7fe2685ec21d8323384"))
 (elisp-demos :source "elpaca-menu-lock-file" :recipe
	      (:package "elisp-demos" :fetcher github :repo
			"xuchunyang/elisp-demos" :files (:defaults "*.org")
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "1a108d1c5011f9ced58be2ca98bea1fbd4130a2f"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
	     (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github
		       :files (:defaults (:exclude "elisp-refs-bench.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elnode :source "elpaca-menu-lock-file" :recipe
	 (:package "elnode" :fetcher github :repo "jcaw/elnode" :branch "melpa"
		   :files ("default*" "elnode*") :source "MELPA" :protocol https
		   :inherit t :depth treeless :ref
		   "29ef0f51a65a24fca7fdcdb4140d2e4556e4bb29"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
	    "https://github.com/progfolio/elpaca.git" :ref
	    "f0bbcec414ef7ec7b9ac7caee249dafc0b7c3fd4" :files
	    (:defaults "elpaca-test.el" (:exclude "extensions")) :build
	    (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/elpaca-extras.el") :wait t :package
			 "elpaca-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
		     (:package "elpaca-use-package" :wait t :repo
			       "https://github.com/progfolio/elpaca.git" :files
			       ("extensions/elpaca-use-package.el") :main
			       "extensions/elpaca-use-package.el" :build
			       (:not elpaca--compile-info) :source
			       "Elpaca extensions" :protocol https :inherit t
			       :depth treeless :ref
			       "f0bbcec414ef7ec7b9ac7caee249dafc0b7c3fd4"))
 (elpy :source "elpaca-menu-lock-file" :recipe
       (:package "elpy" :fetcher github :repo "jorgenschaefer/elpy" :files
		 ("*.el" "NEWS.rst" "snippets" "elpy") :source "MELPA" :protocol
		 https :inherit t :depth treeless :ref
		 "0b381f55969438ab2ccc2d1a1614045fcf7c9545"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
	  (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
		    (:defaults "README.md" "sqlite") :source "MELPA" :protocol
		    https :inherit t :depth treeless :ref
		    "ced062890061b6e4fbe4d00c0617f7ff84fff25c"))
 (embark :source "elpaca-menu-lock-file" :recipe
	 (:package "embark" :repo "oantolin/embark" :fetcher github :files
		   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "89c7610fa7988b6c93029f87afa4ba6b7768bf81"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
		 (:package "embark-consult" :repo "oantolin/embark" :fetcher
			   github :files ("embark-consult.el") :source "MELPA"
			   :protocol https :inherit t :depth treeless :ref
			   "89c7610fa7988b6c93029f87afa4ba6b7768bf81"))
 (emojify :source "elpaca-menu-lock-file" :recipe
	  (:package "emojify" :fetcher github :repo "iqbalansari/emacs-emojify"
		    :files (:defaults "data" "images") :source "MELPA" :protocol
		    https :inherit t :depth treeless :ref
		    "1b726412f19896abf5e4857d4c32220e33400b55"))
 (empv :source "elpaca-menu-lock-file" :recipe
       (:package "empv" :fetcher github :repo "isamert/empv.el" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless
		 :host github :ref "2bb45a6a3119d2816af351ff097506c6f2801dd6"))
 (emsg-blame :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth treeless :host
		      github :repo "ISouthRain/emsg-blame" :package "emsg-blame"
		      :ref "7b0bdae8398a38b0bdb103f8cdeaaf62053496cb"))
 (engine-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "engine-mode" :repo "hrs/engine-mode" :fetcher github
			:files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "e7f317f1b284853b6df4dfd37ab7715b248e0ebd"))
 (eshell-syntax-highlighting :source "elpaca-menu-lock-file" :recipe
			     (:package "eshell-syntax-highlighting" :fetcher
				       github :repo
				       "akreisher/eshell-syntax-highlighting"
				       :files
				       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
					"*.texinfo" "doc/dir" "doc/*.info"
					"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
					"docs/dir" "docs/*.info" "docs/*.texi"
					"docs/*.texinfo"
					(:exclude ".dir-locals.el" "test.el"
						  "tests.el" "*-test.el"
						  "*-tests.el" "LICENSE"
						  "README*" "*-pkg.el"))
				       :source "MELPA" :protocol https :inherit
				       t :depth treeless :ref
				       "62418fd8b2380114a3f6dad699c1ba45329db1d2"))
 (esxml :source "elpaca-menu-lock-file" :recipe
	(:package "esxml" :fetcher github :repo "tali713/esxml" :files
		  ("esxml.el" "esxml-query.el") :source "MELPA" :protocol https
		  :inherit t :depth treeless :ref
		  "affada143fed7e2da08f2b3d927a027f26ad4a8f"))
 (eww-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth nil :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/eww-extras.el") :package "eww-extras" :ref
		      "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
		(:package "expand-region" :repo "magnars/expand-region.el"
			  :fetcher github :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	      :source "MELPA" :protocol https :inherit t :depth treeless :ref
	      "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (faces-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/faces-extras.el") :package "faces-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (fakir :source "elpaca-menu-lock-file" :recipe
	(:package "fakir" :fetcher github :repo "nicferrier/emacs-fakir" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "1fca406ad7de80fece6319ff75d4230b648534b0"))
 (fatebook :source "elpaca-menu-lock-file" :recipe
	   (:source nil :protocol https :inherit t :depth treeless :repo
		    "sonofhypnos/fatebook.el" :host github :files
		    ("fatebook.el") :package "fatebook" :ref
		    "7b70876ea0de1ee78047600e4dfc07bf8069916f"))
 (files-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/files-extras.el") :package "files-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
	   (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "a4d782e7af12e20037c0cecf0d4386cd2676c085"))
 (flycheck-languagetool :source "elpaca-menu-lock-file" :recipe
			(:package "flycheck-languagetool" :repo
				  "emacs-languagetool/flycheck-languagetool"
				  :fetcher github :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :protocol https :inherit t
				  :depth treeless :ref
				  "bca4b2d91f62f45beb3c5d387b74448ac294b718"))
 (flycheck-ledger :source "elpaca-menu-lock-file" :recipe
		  (:package "flycheck-ledger" :fetcher github :repo
			    "purcell/flycheck-ledger" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			     "doc/*.texinfo" "lisp/*.el" "docs/dir"
			     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el" "tests.el"
				       "*-test.el" "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :ref
			    "48bed9193c8601b142245df03968ae493b7d430c"))
 (forge :source "elpaca-menu-lock-file" :recipe
	(:package "forge" :fetcher github :repo "magit/forge" :files
		  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
		  :protocol https :inherit t :depth treeless :host github
		  :branch "main" :build (:not elpaca--check-version) :ref
		  "ca6c2ae18cbe2c9795832ee9a73fab8b245831d3"))
 (forge-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/forge-extras.el") :package "forge-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (forge-search :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "benthamite/forge-search.el" :branch
			"fix/forge-get-repository" :package "forge-search" :ref
			"dc792fa9cd1d26c194313f888b4c0092b6f42e03"))
 (frame-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/frame-extras.el") :package "frame-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (gdrive :source "elpaca-menu-lock-file" :recipe
	 (:source nil :protocol https :inherit t :depth nil :host github :repo
		  "benthamite/gdrive" :package "gdrive" :ref
		  "a2c99e87097bb2a8317a551a7b9456d6d2a9352c"))
 (gh :source "elpaca-menu-lock-file" :recipe
     (:package "gh" :repo "sigma/gh.el" :fetcher github :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless
	       :version (lambda (_) "2.29") :ref
	       "b5a8d8209340d49ad82dab22d23dae0434499fdf"))
 (ghub :source "elpaca-menu-lock-file" :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
		 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
		 :protocol https :inherit t :depth treeless :host github :build
		 (:not elpaca--check-version) :branch "main" :ref
		 "a3a5dd315a8dad1b1cccfc1ac8502907f17fb9b5"))
 (git-auto-commit-mode :source "elpaca-menu-lock-file" :recipe
		       (:package "git-auto-commit-mode" :fetcher github :repo
				 "ryuslash/git-auto-commit-mode" :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				  "*.texinfo" "doc/dir" "doc/*.info"
				  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				  "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el"
					    "tests.el" "*-test.el" "*-tests.el"
					    "LICENSE" "README*" "*-pkg.el"))
				 :source "MELPA" :protocol https :inherit t
				 :depth treeless :ref
				 "a7b59acea622a737d23c783ce7d212fefb29f7e6"))
 (gntp :source "elpaca-menu-lock-file" :recipe
       (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "767571135e2c0985944017dc59b0be79af222ef5"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
	  (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
		    ("go-mode.el") :source "MELPA" :protocol https :inherit t
		    :depth treeless :ref
		    "0ed3c5227e7f622589f1411b4939c3ee34711ebd"))
 (goldendict-ng :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth treeless :host
			 github :repo "benthamite/goldendict-ng" :package
			 "goldendict-ng" :ref
			 "98e9727a9edeb4ed82c6e6a705fadb65f616ec6b"))
 (goto-last-change :source "elpaca-menu-lock-file" :recipe
		   (:package "goto-last-change" :repo
			     "camdez/goto-last-change.el" :fetcher github :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "58b0928bc255b47aad318cd183a5dce8f62199cc"))
 (gptel :source "elpaca-menu-lock-file" :recipe
	(:package "gptel" :repo "karthink/gptel" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "5897d07a3aa164243ee42f67e4f7eb2bab0748d9"))
 (gptel-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/gptel-extras.el") :package "gptel-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (gptel-plus :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth treeless :host
		      github :repo "benthamite/gptel-plus" :package "gptel-plus"
		      :ref "2697ace508573e8dc092873663c4f221a51d31b7"))
 (gptel-quick :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "karthink/gptel-quick" :package
		       "gptel-quick" :ref
		       "495b5e0b5348dbced1448bd12cbf8847e30b5175"))
 (grammarly :source "elpaca-menu-lock-file" :recipe
	    (:package "grammarly" :repo "emacs-grammarly/grammarly" :fetcher
		      github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "c0550c4d6de606290b8771ef2b2b5fd55932b5a9"))
 (graphql-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "graphql-mode" :repo "davazp/graphql-mode" :fetcher
			 github :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :ref
			 "ee49531935ede7a2c9597713e13a4c9d33ef2220"))
 (grip-mode :source "elpaca-menu-lock-file" :recipe
	    (:package "grip-mode" :repo "seagle0128/grip-mode" :fetcher github
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "c965a344ab950c6b4ef788c7b9e8792a95a139d1"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher
			 github :files (:defaults "NEWS" "logo.svg") :source
			 "MELPA" :protocol https :inherit t :depth treeless :ref
			 "4ce37fcc5f43acfb3966687475dbfb0fb3f97e7e"))
 (helpful :source "elpaca-menu-lock-file" :recipe
	  (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (highlight-indentation :source "elpaca-menu-lock-file" :recipe
			(:package "highlight-indentation" :repo
				  "antonj/Highlight-Indentation-for-Emacs"
				  :fetcher github :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :protocol https :inherit t
				  :depth treeless :ref
				  "d88db4248882da2d4316e76ed673b4ac1fa99ce3"))
 (highlight-parentheses :source "elpaca-menu-lock-file" :recipe
			(:package "highlight-parentheses" :fetcher sourcehut
				  :repo "tsdh/highlight-parentheses.el" :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :protocol https :inherit t
				  :depth treeless :ref
				  "965b18dd69eff4457e17c9e84b3cbfdbfca2ddfb"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
	  (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :build (:not elpaca--check-version) :ref
		    "b8be53068b3469572d66cfedc540f4130901a3da"))
 (hsluv :source "elpaca-menu-lock-file" :recipe
	(:package "hsluv" :fetcher github :repo "hsluv/hsluv-emacs" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "c3bc5228e30d66e7dee9ff1a0694c2b976862fc0"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmlize :source "elpaca-menu-lock-file" :recipe
	  (:package "htmlize" :fetcher github :repo "emacsorphanage/htmlize"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "c9a8196a59973fabb3763b28069af9a4822a5260"))
 (hydra :source "elpaca-menu-lock-file" :recipe
	(:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
		  (:defaults (:exclude "lv.el")) :source "MELPA" :protocol https
		  :inherit t :depth treeless :ref
		  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (init :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth nil :host github :repo
		"benthamite/init" :wait t :package "init" :ref
		"a0c14901d8f5b83a664dbe8c6cd96ed6639d31db"))
 (isearch-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/isearch-extras.el") :package
			  "isearch-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (jeison :source "elpaca-menu-lock-file" :recipe
	 (:package "jeison" :repo "SavchenkoValeriy/jeison" :fetcher github
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "19a51770f24eaa7b538c7be6a8a5c25d154b641f"))
 (jinx :source "elpaca-menu-lock-file" :recipe
       (:package "jinx" :repo "minad/jinx" :files
		 (:defaults "jinx-mod.c" "emacs-module.h") :fetcher github
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "214d98c18c01897eab108a880b18aa3b64632a03"))
 (jinx-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/jinx-extras.el") :package "jinx-extras"
		       :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (js2-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "e0c302872de4d26a9c1614fac8d6b94112b96307"))
 (json-mode :source "elpaca-menu-lock-file" :recipe
	    (:package "json-mode" :fetcher github :repo "json-emacs/json-mode"
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "466d5b563721bbeffac3f610aefaac15a39d90a9"))
 (json-snatcher :source "elpaca-menu-lock-file" :recipe
		(:package "json-snatcher" :fetcher github :repo
			  "Sterlingg/json-snatcher" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (kelly :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth treeless :host github
		 :repo "benthamite/kelly" :package "kelly" :ref
		 "3cbe7393d76e07a3859c58a979cb013dceb4ac2b"))
 (keycast :source "elpaca-menu-lock-file" :recipe
	  (:package "keycast" :fetcher github :repo "tarsius/keycast" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "6570b73c4d726d18d6ee48a46494b6ff35aacea6"))
 (kmacro-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/kmacro-extras.el") :package
			 "kmacro-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (kv :source "elpaca-menu-lock-file" :recipe
     (:package "kv" :fetcher github :repo "nicferrier/emacs-kv" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "721148475bce38a70e0b678ba8aa923652e8900e"))
 (language-detection :source "elpaca-menu-lock-file" :recipe
		     (:package "language-detection" :fetcher github :repo
			       "andreasjansson/language-detection.el" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi" "docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
					  "*-test.el" "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https :inherit t :depth
			       treeless :ref
			       "54a6ecf55304fba7d215ef38a4ec96daff2f35a4"))
 (ledger-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode"
			:files ("ledger-*.el" "doc/*.texi") :old-names
			(ldg-mode) :source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"d9b664820176bf294fbca5ee99c91920862cf37d"))
 (ledger-mode-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :protocol https :inherit t :depth nil :host
			      github :repo "benthamite/dotfiles" :files
			      ("emacs/extras/ledger-mode-extras.el") :package
			      "ledger-mode-extras" :ref
			      "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (lin :source "elpaca-menu-lock-file" :recipe
      (:package "lin" :repo ("https://github.com/protesilaos/lin" . "lin")
		:files ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
		:source "GNU ELPA" :protocol https :inherit t :depth treeless
		:ref "993ed8519715dcd390ebb3c9f983f3c8d2d56de2"))
 (list-utils :source "elpaca-menu-lock-file" :recipe
	     (:package "list-utils" :repo "rolandwalker/list-utils" :fetcher
		       github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "bbea0e7cc7ab7d96e7f062014bde438aa8ffcd43"))
 (llama :source "elpaca-menu-lock-file" :recipe
	(:package "llama" :fetcher github :repo "tarsius/llama" :files
		  ("llama.el" ".dir-locals.el") :source "MELPA" :protocol https
		  :inherit t :depth treeless :ref
		  "0cc2daffded18eea7f00a318cfa3e216977ffe50"))
 (llm :source "elpaca-menu-lock-file" :recipe
      (:package "llm" :repo ("https://github.com/ahyatt/llm" . "llm") :files
		("*" (:exclude ".git")) :source "GNU ELPA" :protocol https
		:inherit t :depth treeless :ref
		"6d6c88c20a2b485ade7bac53bdfb44ed8f91c279"))
 (llm-tool-collection :source "elpaca-menu-lock-file" :recipe
		      (:source nil :protocol https :inherit t :depth treeless
			       :host github :repo "skissue/llm-tool-collection"
			       :package "llm-tool-collection" :ref
			       "6d2765a16dc10af2e1d1911bcabf6d7f287e0434"))
 (log4e :source "elpaca-menu-lock-file" :recipe
	(:package "log4e" :repo "aki2o/log4e" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "6d71462df9bf595d3861bfb328377346aceed422"))
 (logito :source "elpaca-menu-lock-file" :recipe
	 (:package "logito" :repo "sigma/logito" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "d5934ce10ba3a70d3fcfb94d742ce3b9136ce124"))
 (lsp-grammarly :source "elpaca-menu-lock-file" :recipe
		(:package "lsp-grammarly" :repo "emacs-grammarly/lsp-grammarly"
			  :fetcher github :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "58bfced2b54dd2a4ed0552e8675ded68e9fc01f8"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github
		     :files (:defaults "clients/*.*") :source "MELPA" :protocol
		     https :inherit t :depth treeless :ref
		     "1aa47406dc1b171304f29563c9125e5abc761fc0"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el")
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (macos :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth treeless :host github
		 :repo "benthamite/macos" :package "macos" :ref
		 "292e8a9512c8a6f0555b535140b5931f95a4db17"))
 (macrostep :source "elpaca-menu-lock-file" :recipe
	    (:package "macrostep" :fetcher github :repo
		      "emacsorphanage/macrostep" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "d0928626b4711dcf9f8f90439d23701118724199"))
 (magit :source "elpaca-menu-lock-file" :recipe
	(:package "magit" :fetcher github :repo "magit/magit" :files
		  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
		   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
		   (:exclude "lisp/magit-section.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :host github :branch "main" :build
		  (:not elpaca--check-version) :ref
		  "4eac262de56057fdcb215a5f69c4d34e67f628d3"))
 (magit-extra :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/magit-extra.el") :package "magit-extra"
		       :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (magit-gptcommit :source "elpaca-menu-lock-file" :recipe
		  (:package "magit-gptcommit" :fetcher github :repo
			    "douo/magit-gptcommit" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			     "doc/*.texinfo" "lisp/*.el" "docs/dir"
			     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el" "tests.el"
				       "*-test.el" "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :ref
			    "97dfcf33777731ba8d8ad0522c2deb0554a143fe"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
		(:package "magit-section" :fetcher github :repo "magit/magit"
			  :files
			  ("lisp/magit-section.el" "docs/magit-section.texi"
			   "magit-section-pkg.el")
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "4eac262de56057fdcb215a5f69c4d34e67f628d3"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
	     (:package "marginalia" :repo "minad/marginalia" :fetcher github
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "3bb30214eb041428d95cd704adfe9b4b8a8f0c09"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
		(:package "markdown-mode" :fetcher github :repo
			  "jrblevin/markdown-mode" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "fc4fff89bae976ff4a594a538e6ef11820440c1f"))
 (markdown-mode-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :protocol https :inherit t :depth nil :host
				github :repo "benthamite/dotfiles" :files
				("emacs/extras/markdown-mode-extras.el")
				:package "markdown-mode-extras" :ref
				"805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (marshal :source "elpaca-menu-lock-file" :recipe
	  (:package "marshal" :fetcher github :repo "sigma/marshal.el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "bc00044d9073482f589aad959e34d563598f682a"))
 (mcp :source "elpaca-menu-lock-file" :recipe
      (:package "mcp" :fetcher github :repo "lizqwerscott/mcp.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :host
		github :build (:not elpaca--check-version) :ref
		"4708c5849ce4ddb632016eca662a7405bfa642d4"))
 (mediawiki :source "elpaca-menu-lock-file" :recipe
	    (:package "mediawiki" :repo "hexmode/mediawiki-el" :fetcher github
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "5c92f03ced519e59d0b8249b53a3311ea344b2ff"))
 (mercado-libre :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth treeless :host
			 github :repo "benthamite/mercado-libre" :package
			 "mercado-libre" :ref
			 "f70ee561d22452f04aabadd9007aad27ff3485cc"))
 (metaweblog :source "elpaca-menu-lock-file" :recipe
	     (:package "metaweblog" :fetcher github :repo "org2blog/org2blog"
		       :files ("metaweblog.el") :source "MELPA" :protocol https
		       :inherit t :depth treeless :ref
		       "d0168606e60df2267b451dfe92975ad3f5c7919c"))
 (modus-themes :source "elpaca-menu-lock-file" :recipe
	       (:package "modus-themes" :fetcher github :repo
			 "protesilaos/modus-themes" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :host github :ref
			 "f4e67c80c94fcae23f87517902a10eccc8ad99d6"))
 (modus-themes-extras :source "elpaca-menu-lock-file" :recipe
		      (:source nil :protocol https :inherit t :depth nil :host
			       github :repo "benthamite/dotfiles" :files
			       ("emacs/extras/modus-themes-extras.el") :package
			       "modus-themes-extras" :ref
			       "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (moon-reader :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "benthamite/moon-reader" :package
		       "moon-reader" :ref
		       "e10c4189e53952a855a15b0bafe7205465bc423f"))
 (mpv :source "elpaca-menu-lock-file" :recipe
      (:package "mpv" :repo "kljohann/mpv.el" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"62cb8825d525d7c9475dd93d62ba84d419bc4832"))
 (mu4e :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth nil :host github :files
		("mu4e/*.el" "build/mu4e/mu4e-meta.el"
		 "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info")
		:repo "djcb/mu" :main "mu4e/mu4e.el" :pre-build
		(("./autogen.sh") ("ninja" "-C" "build")) :build
		(:not elpaca--compile-info) :package "mu4e" :ref
		"0714873978a2d02079f001f3313120831dc2cf10"))
 (mu4e-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/mu4e-extras.el") :package "mu4e-extras"
		       :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (mullvad :source "elpaca-menu-lock-file" :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github
		   :repo "benthamite/mullvad" :package "mullvad" :ref
		   "445c153793ac5296d08fd6e1886e994ded42f038"))
 (nav-flash :source "elpaca-menu-lock-file" :recipe
	    (:package "nav-flash" :repo "rolandwalker/nav-flash" :fetcher github
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "5d4b48567862f6be0ca973d6b1dca90e4815cb9b"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
	     (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el"
		       :fetcher github :files (:defaults "data") :source "MELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "f3e7ba37642455e5627968b1031faeefbcac1245"))
 (nerd-icons-completion :source "elpaca-menu-lock-file" :recipe
			(:package "nerd-icons-completion" :repo
				  "rainstormstudio/nerd-icons-completion"
				  :fetcher github :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :protocol https :inherit t
				  :depth treeless :ref
				  "5f27607b008a972a27b237d266c8ee7b26fbd721"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
		   (:package "nerd-icons-dired" :repo
			     "rainstormstudio/nerd-icons-dired" :fetcher github
			     :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "5df0dd57022d0b3f565e05c5d9b3a8a723236676"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
	       (:package "no-littering" :fetcher github :repo
			 "emacscollective/no-littering" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :wait t :ref
			 "f12c3d9e44c4be099c226114ac95586f4d50df05"))
 (noflet :source "elpaca-menu-lock-file" :recipe
	 (:package "noflet" :fetcher github :repo "nicferrier/emacs-noflet"
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "7ae84dc3257637af7334101456dafe1759c6b68a"))
 (nov :source "elpaca-menu-lock-file" :recipe
      (:package "nov" :fetcher git :url "https://depp.brause.cc/nov.el.git"
		:files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"933816c190633fa1f2f0667ba105d1311572b3c6"))
 (oauth2-auto :source "elpaca-menu-lock-file" :recipe
	      (:package "oauth2-auto" :fetcher github :repo
			"telotortium/emacs-oauth2-auto" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol ssh :inherit t :depth treeless
			:host github :ref
			"20b3153d9cfb7aafe68a0168647a17373adf5e22"))
 (ob-typescript :source "elpaca-menu-lock-file" :recipe
		(:package "ob-typescript" :repo "lurdan/ob-typescript" :fetcher
			  github :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "5fe1762f8d8692dd5b6f1697bedbbf4cae9ef036"))
 (ol-emacs-slack :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth treeless :host
			  github :repo "ag91/ol-emacs-slack" :package
			  "ol-emacs-slack" :ref
			  "299bd86280179999b049abc7252eb1bffa8a5ddd"))
 (orderless :source "elpaca-menu-lock-file" :recipe
	    (:package "orderless" :repo "oantolin/orderless" :fetcher github
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "31812d9252c6cfa7eae8fa04cd40c8b2081e9936"))
 (orderless-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/orderless-extras.el") :package
			    "orderless-extras" :ref
			    "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-appear :source "elpaca-menu-lock-file" :recipe
	     (:package "org-appear" :fetcher github :repo "awth13/org-appear"
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (org-archive-hierarchically :source "elpaca-menu-lock-file" :recipe
			     (:source nil :protocol https :inherit t :depth
				      treeless :host gitlab :repo
				      "andersjohansson/org-archive-hierarchically"
				      :package "org-archive-hierarchically" :ref
				      "c7ddf3f36570e50d6163e7a4e3099c2c8117f894"))
 (org-autosort :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "yantar92/org-autosort" :package
			"org-autosort" :ref
			"dcb04823a5278c68b1ecfbfa0b3e61ab5710e4d4"))
 (org-clock-convenience :source "elpaca-menu-lock-file" :recipe
			(:package "org-clock-convenience" :fetcher github :repo
				  "dfeich/org-clock-convenience" :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :protocol https :inherit t
				  :depth treeless :ref
				  "42af7c611bcbc818653cd5f5574ef2ff8df0eb22"))
 (org-clock-split :source "elpaca-menu-lock-file" :recipe
		  (:package "org-clock-split" :repo "0robustus1/org-clock-split"
			    :fetcher github :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			     "doc/*.texinfo" "lisp/*.el" "docs/dir"
			     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el" "tests.el"
				       "*-test.el" "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :host github :branch "support-emacs-29.1"
			    :ref "65b7872864038a458990418947cd94b8907f7e38"))
 (org-contacts :source "elpaca-menu-lock-file" :recipe
	       (:package "org-contacts" :fetcher git :url
			 "https://repo.or.cz/org-contacts.git" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :build (:not elpaca--check-version) :ref
			 "cbf1f189381e7fd1727ed7afe0b525a39c806722"))
 (org-contrib :source "elpaca-menu-lock-file" :recipe
	      (:package "org-contrib" :repo
			("https://git.sr.ht/~bzg/org-contrib" . "org-contrib")
			:files (:defaults) :source "Org" :protocol https
			:inherit t :depth treeless :ref
			"f1f6b6ec812803ff99693255555a82960fb3545a"))
 (org-download :source "elpaca-menu-lock-file" :recipe
	       (:package "org-download" :repo "abo-abo/org-download" :fetcher
			 github :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :ref
			 "c8be2611786d1d8d666b7b4f73582de1093f25ac"))
 (org-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth nil :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/org-extras.el") :package "org-extras" :ref
		      "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-gcal :source "elpaca-menu-lock-file" :recipe
	   (:package "org-gcal" :fetcher github :repo "kidd/org-gcal.el" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :host github :build (:not elpaca--check-version) :ref
		     "23561f63b9433353420a4cb113f455f6ca31abe6"))
 (org-gcal-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/org-gcal-extras.el") :package
			   "org-gcal-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-journal :source "elpaca-menu-lock-file" :recipe
	      (:package "org-journal" :fetcher github :repo
			"bastibe/org-journal" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "c72d7c75f8a05d1032250e307d35797ceee7e578"))
 (org-make-toc :source "elpaca-menu-lock-file" :recipe
	       (:package "org-make-toc" :fetcher github :repo
			 "alphapapa/org-make-toc" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :ref
			 "5f0f39b11c091a5abf49ddf78a6f740252920f78"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
	     (:package "org-modern" :repo "minad/org-modern" :fetcher github
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "1723689710715da9134e62ae7e6d41891031813c"))
 (org-modern-indent :source "elpaca-menu-lock-file" :recipe
		    (:source nil :protocol https :inherit t :depth treeless
			     :host github :repo "jdtsmith/org-modern-indent"
			     :package "org-modern-indent" :ref
			     "9973bd3b91e4733a3edd1fca232208c837c05473"))
 (org-msg :source "elpaca-menu-lock-file" :recipe
	  (:package "org-msg" :repo "jeremy-compostella/org-msg" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "59e2042e5f23e25f31c6aef0db1e70c6f54f117d"))
 (org-msg-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/org-msg-extras.el") :package
			  "org-msg-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-noter :source "elpaca-menu-lock-file" :recipe
	    (:package "org-noter" :fetcher github :repo "org-noter/org-noter"
		      :files
		      ("*.el" "modules"
		       (:exclude "*-test-utils.el" "*-devel.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :host github :ref
		      "a28f61238a572dd9f0073c2ee15a951ffe6aaebc"))
 (org-noter-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/org-noter-extras.el") :package
			    "org-noter-extras" :ref
			    "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-pdftools :source "elpaca-menu-lock-file" :recipe
	       (:package "org-pdftools" :fetcher github :repo
			 "fuxialexander/org-pdftools" :files ("org-pdftools.el")
			 :old-names (org-pdfview) :source "MELPA" :protocol
			 https :inherit t :depth treeless :build
			 (:not elpaca--check-version) :ref
			 "2b3357828a4c2dfba8f87c906d64035d8bf221f2"))
 (org-pomodoro :source "elpaca-menu-lock-file" :recipe
	       (:package "org-pomodoro" :fetcher github :repo
			 "marcinkoziej/org-pomodoro" :files
			 (:defaults "resources") :source "MELPA" :protocol https
			 :inherit t :depth treeless :ref
			 "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
 (org-pomodoro-extras :source "elpaca-menu-lock-file" :recipe
		      (:source nil :protocol https :inherit t :depth nil :host
			       github :repo "benthamite/dotfiles" :files
			       ("emacs/extras/org-pomodoro-extras.el") :package
			       "org-pomodoro-extras" :ref
			       "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-ql :source "elpaca-menu-lock-file" :recipe
	 (:package "org-ql" :fetcher github :repo "alphapapa/org-ql" :files
		   (:defaults (:exclude "helm-org-ql.el")) :source "MELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca"))
 (org-ref :source "elpaca-menu-lock-file" :recipe
	  (:package "org-ref" :fetcher github :repo "jkitchin/org-ref" :files
		    (:defaults "org-ref.org" "org-ref.bib" "citeproc") :source
		    "MELPA" :protocol https :inherit t :depth treeless :ref
		    "edbb80863ef63ef52ef04fce3239e063843f8d30"))
 (org-ref-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/org-ref-extras.el") :package
			  "org-ref-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
	   (:package "org-roam" :fetcher github :repo "org-roam/org-roam" :files
		     (:defaults "extensions/*") :source "MELPA" :protocol https
		     :inherit t :depth treeless :ref
		     "89dfaef38b6caa3027f20f96a551dc8f194ac533"))
 (org-roam-bibtex :source "elpaca-menu-lock-file" :recipe
		  (:package "org-roam-bibtex" :fetcher github :repo
			    "org-roam/org-roam-bibtex" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			     "doc/*.texinfo" "lisp/*.el" "docs/dir"
			     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el" "tests.el"
				       "*-test.el" "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :ref
			    "b065198f2c3bc2a47ae520acd2b1e00e7b0171e6"))
 (org-roam-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/org-roam-extras.el") :package
			   "org-roam-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org-roam-ui :source "elpaca-menu-lock-file" :recipe
	      (:package "org-roam-ui" :fetcher github :repo
			"org-roam/org-roam-ui" :files ("*.el" "out") :source
			"MELPA" :protocol https :inherit t :depth treeless :host
			github :branch "main" :ref
			"5ac74960231db0bf7783c2ba7a19a60f582e91ab"))
 (org-super-agenda :source "elpaca-menu-lock-file" :recipe
		   (:package "org-super-agenda" :fetcher github :repo
			     "alphapapa/org-super-agenda" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "fb20ad9c8a9705aa05d40751682beae2d094e0fe"))
 (org-tidy :source "elpaca-menu-lock-file" :recipe
	   (:package "org-tidy" :fetcher github :repo "jxq0/org-tidy" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "0bea3a2ceaa999e0ad195ba525c5c1dcf5fba43b"))
 (org-transclusion :source "elpaca-menu-lock-file" :recipe
		   (:package "org-transclusion" :repo
			     ("https://github.com/nobiot/org-transclusion"
			      . "org-transclusion")
			     :files ("*" (:exclude ".git")) :source "GNU ELPA"
			     :protocol https :inherit t :depth treeless :ref
			     "e9728b0b14b5c2e5d3b68af98f772ed99e136b48"))
 (org-vcard :source "elpaca-menu-lock-file" :recipe
	    (:package "org-vcard" :fetcher github :repo "pinoaffe/org-vcard"
		      :files ("org-vcard.el" "styles") :source "MELPA" :protocol
		      https :inherit t :depth treeless :ref
		      "cca90b1dabe2ec25699628ba704221bea67324dd"))
 (org-web-tools :source "elpaca-menu-lock-file" :recipe
		(:package "org-web-tools" :fetcher github :repo
			  "alphapapa/org-web-tools" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el" "tests.el"
				     "*-test.el" "*-tests.el" "LICENSE"
				     "README*" "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "7a6498f442fc7f29504745649948635c7165d847"))
 (org-web-tools-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :protocol https :inherit t :depth nil :host
				github :repo "benthamite/dotfiles" :files
				("emacs/extras/org-web-tools-extras.el")
				:package "org-web-tools-extras" :ref
				"805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (org2blog :source "elpaca-menu-lock-file" :recipe
	   (:package "org2blog" :fetcher github :repo "org2blog/org2blog" :files
		     (:defaults "README.org" (:exclude "metaweblog.el")) :source
		     "MELPA" :protocol https :inherit t :depth treeless :ref
		     "d0168606e60df2267b451dfe92975ad3f5c7919c"))
 (org2blog-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/org2blog-extras.el") :package
			   "org2blog-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (orgit :source "elpaca-menu-lock-file" :recipe
	(:package "orgit" :fetcher github :repo "magit/orgit" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :build (:not elpaca--check-version) :ref
		  "224350397df0987f8cdc770eb7f4618eca34a727"))
 (orgit-forge :source "elpaca-menu-lock-file" :recipe
	      (:package "orgit-forge" :fetcher github :repo "magit/orgit-forge"
			:files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :build (:not elpaca--check-version) :ref
			"050590fbc79bc3acd0ab87c0257f6719bff1e33a"))
 (orgtbl-edit :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "shankar2k/orgtbl-edit" :package
		       "orgtbl-edit" :ref
		       "178b2ec078e7badfde5143e7a9ff9f9605836d98"))
 (orgtbl-join :source "elpaca-menu-lock-file" :recipe
	      (:package "orgtbl-join" :fetcher github :repo "tbanel/orgtbljoin"
			:files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "d434bd138726e77f34a0e77d5de8416d6322b9f4"))
 (outli :source "elpaca-menu-lock-file" :recipe
	(:package "outli" :fetcher github :repo "jdtsmith/outli" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :host github :ref "6527512470b450b4d1c0d8bba69888de19f0c124"))
 (outline-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/outline-extras.el") :package
			  "outline-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (ov :source "elpaca-menu-lock-file" :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "e2971ad986b6ac441e9849031d34c56c980cf40b"))
 (ox-clip :source "elpaca-menu-lock-file" :recipe
	  (:package "ox-clip" :fetcher github :repo "jkitchin/ox-clip" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "a549cc8e1747beb6b7e567ffac27e31ba45cb8e8"))
 (ox-gfm :source "elpaca-menu-lock-file" :recipe
	 (:package "ox-gfm" :fetcher github :repo "larstvei/ox-gfm" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb"))
 (ox-hugo :source "elpaca-menu-lock-file" :recipe
	  (:package "ox-hugo" :fetcher github :repo "kaushalmodi/ox-hugo" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "e3365cb4e65c1853d8838b863a21546bbd9e0990"))
 (ox-pandoc :source "elpaca-menu-lock-file" :recipe
	    (:package "ox-pandoc" :repo "emacsorphanage/ox-pandoc" :fetcher
		      github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "5766c70b6db5a553829ccdcf52fcf3c6244e443d"))
 (pandoc-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "pandoc-mode" :fetcher github :repo
			"joostkremers/pandoc-mode" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "d7f6fa119bb0e883cfd615009d197e4b87916033"))
 (parsebib :source "elpaca-menu-lock-file" :recipe
	   (:package "parsebib" :fetcher github :repo "joostkremers/parsebib"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "7bfde4e4679413424a9a9af099203d5c23e32cd2"))
 (pass :source "elpaca-menu-lock-file" :recipe
       (:package "pass" :fetcher github :repo "NicolasPetton/pass" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "7651389c52919f5e0e41d9217b29c7166e3a45c2"))
 (pass-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/pass-extras.el") :package "pass-extras"
		       :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (password-generator :source "elpaca-menu-lock-file" :recipe
		     (:package "password-generator" :fetcher github :repo
			       "vandrlexay/emacs-password-genarator" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi" "docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
					  "*-test.el" "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https :inherit t :depth
			       treeless :host github :ref
			       "2d0deb52f2fd978bff9001e155e36ac5bd287d52"))
 (password-store :source "elpaca-menu-lock-file" :recipe
		 (:package "password-store" :fetcher github :repo
			   "zx2c4/password-store" :files ("contrib/emacs/*.el")
			   :source "MELPA" :protocol https :inherit t :depth
			   treeless :ref
			   "3ca13cd8882cae4083c1c478858adbf2e82dd037"))
 (password-store-otp :source "elpaca-menu-lock-file" :recipe
		     (:package "password-store-otp" :repo
			       "volrath/password-store-otp.el" :fetcher github
			       :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi" "docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
					  "*-test.el" "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https :inherit t :depth
			       treeless :version (lambda (_) "0.1.5") :ref
			       "be3a00a981921ed1b2f78012944dc25eb5a0beca"))
 (paths :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth nil :host github :repo
		 "benthamite/dotfiles" :files ("emacs/extras/paths.el") :package
		 "paths" :ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (pcache :source "elpaca-menu-lock-file" :recipe
	 (:package "pcache" :repo "sigma/pcache" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "e287b5d116679f79789ee9ee22ee213dc6cef68c"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
	  (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
	    (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools"
		      :files
		      (:defaults "README" ("build" "Makefile")
				 ("build" "server"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "30b50544e55b8dbf683c2d932d5c33ac73323a16"))
 (pdf-tools-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/pdf-tools-extras.el") :package
			    "pdf-tools-extras" :ref
			    "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (pdf-tools-pages :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth treeless :host
			   github :repo "benthamite/pdf-tools-pages" :package
			   "pdf-tools-pages" :ref
			   "39eed3e9ddbbf4cfb4291497ccc936bf13bc138e"))
 (pdf-view-restore :source "elpaca-menu-lock-file" :recipe
		   (:package "pdf-view-restore" :repo
			     "007kevin/pdf-view-restore" :fetcher github :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			      "doc/*.texinfo" "lisp/*.el" "docs/dir"
			      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el" "tests.el"
					"*-test.el" "*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "5a1947c01a3edecc9e0fe7629041a2f53e0610c9"))
 (peep-dired :source "elpaca-menu-lock-file" :recipe
	     (:package "peep-dired" :repo "asok/peep-dired" :fetcher github
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "1cb81016dd78a7afca48ad1dba6dc7996ec6e167"))
 (persist :source "elpaca-menu-lock-file" :recipe
	  (:package "persist" :repo
		    ("https://github.com/emacsmirror/gnu_elpa" . "persist")
		    :branch "externals/persist" :files ("*" (:exclude ".git"))
		    :source "GNU ELPA" :protocol https :inherit t :depth
		    treeless :ref "bf9a52b8608f00a86cdf5b82207fc7bbc747566c"))
 (persistent-scratch :source "elpaca-menu-lock-file" :recipe
		     (:package "persistent-scratch" :fetcher github :repo
			       "Fanael/persistent-scratch" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi" "docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
					  "*-test.el" "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https :inherit t :depth
			       treeless :ref
			       "5ff41262f158d3eb966826314516f23e0cb86c04"))
 (persistent-soft :source "elpaca-menu-lock-file" :recipe
		  (:package "persistent-soft" :repo
			    "rolandwalker/persistent-soft" :fetcher github
			    :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			     "doc/*.texinfo" "lisp/*.el" "docs/dir"
			     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el" "tests.el"
				       "*-test.el" "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :ref
			    "a1e0ddf2a12a6f18cab565dee250f070384cbe02"))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"6f593663a2c76e582fa2a6db1bb902474ea9b44e"))
 (plz :source "elpaca-menu-lock-file"
   :recipe
   (:package "plz" :repo ("https://github.com/alphapapa/plz.el.git" . "plz")
	     :files ("*" (:exclude ".git" "LICENSE")) :source "GNU ELPA"
	     :protocol https :inherit t :depth treeless :ref
	     "e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e"))
 (plz-event-source :source "elpaca-menu-lock-file" :recipe
		   (:package "plz-event-source" :repo
			     ("https://github.com/r0man/plz-event-source"
			      . "plz-event-source")
			     :files ("*" (:exclude ".git")) :source "GNU ELPA"
			     :protocol https :inherit t :depth treeless :ref
			     "de89214ce14e2b82cbfdc30e1adcf3e77b1f250a"))
 (plz-media-type :source "elpaca-menu-lock-file" :recipe
		 (:package "plz-media-type" :repo
			   ("https://github.com/r0man/plz-media-type"
			    . "plz-media-type")
			   :files ("*" (:exclude ".git")) :source "GNU ELPA"
			   :protocol https :inherit t :depth treeless :ref
			   "b1127982d53affff082447030cda6e8ead3899cb"))
 (polymarket :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth treeless :host
		      github :repo "benthamite/polymarket" :package "polymarket"
		      :ref "dfbb185c08f713f5d86c77e082be0b3da1736794"))
 (polymode :source "elpaca-menu-lock-file" :recipe
	   (:package "polymode" :fetcher github :repo "polymode/polymode" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "25ba9463a443f0e904147138f226284e437248d3"))
 (popper :source "elpaca-menu-lock-file" :recipe
	 (:package "popper" :fetcher github :repo "karthink/popper" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "49f4904480cf4ca5c6db83fcfa9e6ea8d4567d96"))
 (posframe :source "elpaca-menu-lock-file" :recipe
	   (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "12f540c9ad5da09673b2bca1132b41f94c134e82"))
 (powerthesaurus :source "elpaca-menu-lock-file" :recipe
		 (:package "powerthesaurus" :repo
			   "SavchenkoValeriy/emacs-powerthesaurus" :fetcher
			   github :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el" "tests.el"
				      "*-test.el" "*-tests.el" "LICENSE"
				      "README*" "*-pkg.el"))
			   :source "MELPA" :protocol https :inherit t :depth
			   treeless :ref
			   "4b97797cf789aaba411c61a85fe23474ebc5bedc"))
 (pr-review :source "elpaca-menu-lock-file" :recipe
	    (:package "pr-review" :fetcher github :repo
		      "blahgeek/emacs-pr-review" :files (:defaults "graphql")
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "469e2e1f0f111773899627f81ae433f6ec14e5b5"))
 (profiler-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/profiler-extras.el") :package
			   "profiler-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (prot-common :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "protesilaos/dotfiles" :local-repo
		       "prot-common" :main
		       "emacs/.emacs.d/prot-lisp/prot-common.el" :build
		       (:not elpaca--check-version) :files
		       ("emacs/.emacs.d/prot-lisp/prot-common.el") :package
		       "prot-common" :ref
		       "d70319903c40cb456235c7c3500643ad3f997069"))
 (prot-eww :source "elpaca-menu-lock-file" :recipe
	   (:source nil :protocol https :inherit t :depth treeless :host github
		    :repo "protesilaos/dotfiles" :local-repo "prot-eww" :main
		    "emacs/.emacs.d/prot-lisp/prot-eww.el" :build
		    (:not elpaca--check-version) :files
		    ("emacs/.emacs.d/prot-lisp/prot-eww.el") :package "prot-eww"
		    :ref "d70319903c40cb456235c7c3500643ad3f997069"))
 (prot-scratch :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "protesilaos/dotfiles" :local-repo
			"prot-scratch" :main
			"emacs/.emacs.d/prot-lisp/prot-scratch.el" :build
			(:not elpaca--check-version) :files
			("emacs/.emacs.d/prot-lisp/prot-scratch.el") :package
			"prot-scratch" :ref
			"d70319903c40cb456235c7c3500643ad3f997069"))
 (prot-simple :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "protesilaos/dotfiles" :local-repo
		       "prot-simple" :main
		       "emacs/.emacs.d/prot-lisp/prot-simple.el" :build
		       (:not elpaca--check-version) :files
		       ("emacs/.emacs.d/prot-lisp/prot-simple.el") :package
		       "prot-simple" :ref
		       "d70319903c40cb456235c7c3500643ad3f997069"))
 (puni :source "elpaca-menu-lock-file" :recipe
       (:package "puni" :repo "AmaiKinono/puni" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "f430f5b0a14c608176e3376058eb380ab0824621"))
 (pyenv-mode :source "elpaca-menu-lock-file" :recipe
	     (:package "pyenv-mode" :fetcher github :repo
		       "pythonic-emacs/pyenv-mode" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "ba1a8c7bd7d03c006f8411c856990c083c814c9c"))
 (pythonic :source "elpaca-menu-lock-file" :recipe
	   (:package "pythonic" :fetcher github :repo "pythonic-emacs/pythonic"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "5daa1b9f589c2faf0ee8165dbb470a4dc752122f"))
 (pyvenv :source "elpaca-menu-lock-file" :recipe
	 (:package "pyvenv" :fetcher github :repo "jorgenschaefer/pyvenv" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "31ea715f2164dd611e7fc77b26390ef3ca93509b"))
 (queue :source "elpaca-menu-lock-file" :recipe
	(:package "queue" :repo
		  ("https://github.com/emacsmirror/gnu_elpa" . "queue") :branch
		  "externals/queue" :files ("*" (:exclude ".git")) :source
		  "GNU ELPA" :protocol https :inherit t :depth treeless :ref
		  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "rainbow-mode" :repo
			 ("https://github.com/emacsmirror/gnu_elpa"
			  . "rainbow-mode")
			 :branch "externals/rainbow-mode" :files
			 ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
			 https :inherit t :depth treeless :ref
			 "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (read-aloud :source "elpaca-menu-lock-file" :recipe
	     (:package "read-aloud" :repo "gromnitsky/read-aloud.el" :fetcher
		       github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :ref "c662366226abfb07204ab442b4f853ed85438d8a"))
 (read-aloud-extras :source "elpaca-menu-lock-file" :recipe
		    (:source nil :protocol https :inherit t :depth nil :host
			     github :repo "benthamite/dotfiles" :files
			     ("emacs/extras/read-aloud-extras.el") :package
			     "read-aloud-extras" :ref
			     "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (register-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/register-extras.el") :package
			   "register-extras" :ref
			   "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
	     ("request.el") :source "MELPA" :protocol https :inherit t :depth
	     treeless :ref "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (request-deferred :source "elpaca-menu-lock-file" :recipe
		   (:package "request-deferred" :repo "tkf/emacs-request"
			     :fetcher github :files ("request-deferred.el")
			     :source "MELPA" :protocol https :inherit t :depth
			     treeless :ref
			     "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (reveal-in-osx-finder :source "elpaca-menu-lock-file" :recipe
		       (:package "reveal-in-osx-finder" :repo
				 "kaz-yos/reveal-in-osx-finder" :fetcher github
				 :old-names (reveal-in-finder) :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				  "*.texinfo" "doc/dir" "doc/*.info"
				  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				  "docs/dir" "docs/*.info" "docs/*.texi"
				  "docs/*.texinfo"
				  (:exclude ".dir-locals.el" "test.el"
					    "tests.el" "*-test.el" "*-tests.el"
					    "LICENSE" "README*" "*-pkg.el"))
				 :source "MELPA" :protocol https :inherit t
				 :depth treeless :ref
				 "5710e5936e47139a610ec9a06899f72e77ddc7bc"))
 (reverso :source "elpaca-menu-lock-file" :recipe
	  (:package "reverso" :repo "SqrtMinusOne/reverso.el" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :host github :ref "40ed3d83c4f04c39e05d69d84595761ae2956a64"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	      :source "MELPA" :protocol https :inherit t :depth treeless :ref
	      "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (scihub :source "elpaca-menu-lock-file" :recipe
	 (:package "scihub" :fetcher github :repo "benthamite/scihub" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :host github :ref "b5352a0e9a94d246fff02f9c59e8b11abbffd409"))
 (scroll-other-window :source "elpaca-menu-lock-file" :recipe
		      (:source nil :protocol https :inherit t :depth treeless
			       :host github :repo
			       "benthamite/scroll-other-window" :package
			       "scroll-other-window" :ref
			       "98775d18d6e82d0c7b0ce85986165afa2537eb27"))
 (seq :source "elpaca-menu-lock-file" :recipe
      (:package "seq" :repo ("https://github.com/emacsmirror/gnu_elpa" . "seq")
		:branch "externals/seq" :files ("*" (:exclude ".git")) :source
		"GNU ELPA" :protocol https :inherit t :depth treeless :build
		(elpaca--queue-dependencies elpaca--add-info-path
					    elpaca-unload-seq
					    elpaca--activate-package)
		:ref "27a90793a13f149121180e864fa53d68b9eac0b3"))
 (session :source "elpaca-menu-lock-file" :recipe
	  (:package "session" :fetcher github :repo "emacsattic/session" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "3be207c50dfe964de3cbf5cd8fa9b07fc7d2e609"))
 (shr-heading :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "oantolin/emacs-config" :files
		       ("my-lisp/shr-heading.el") :package "shr-heading" :ref
		       "dac275f629189c177f5288e1f94a806bb6101618"))
 (shr-tag-pre-highlight :source "elpaca-menu-lock-file" :recipe
			(:package "shr-tag-pre-highlight" :fetcher github :repo
				  "xuchunyang/shr-tag-pre-highlight.el" :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :protocol https :inherit t
				  :depth treeless :ref
				  "02a93d48f030d71eba460bd09d091baedcad6626"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
	      (:package "shrink-path" :fetcher gitlab :repo
			"bennya/shrink-path.el" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (shut-up
   :source "elpaca-menu-lock-file" :recipe
   (:package "shut-up" :fetcher github :repo "cask/shut-up" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			"*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :source "MELPA" :protocol https :inherit t :depth treeless :ref
	     "ed62a7fefdf04c81346061016f1bc69ca045aaf6"))
 (simple-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/simple-extras.el") :package
			 "simple-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (simple-httpd :source "elpaca-menu-lock-file" :recipe
	       (:package "simple-httpd" :repo "skeeto/emacs-web-server" :fetcher
			 github :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el" "tests.el"
				    "*-test.el" "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t :depth
			 treeless :ref
			 "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (slack :source "elpaca-menu-lock-file" :recipe
	(:package "slack" :fetcher github :repo "emacs-slack/emacs-slack" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "b104bb2f9212e157da01f9161e466b5ed0b151fc"))
 (smartrep :source "elpaca-menu-lock-file" :recipe
	   (:package "smartrep" :repo "myuhe/smartrep.el" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :ref "fdf135e3781b286174b5de4d613f12c318d2023c"))
 (spacious-padding :source "elpaca-menu-lock-file" :recipe
		   (:package "spacious-padding" :repo
			     ("https://github.com/protesilaos/spacious-padding"
			      . "spacious-padding")
			     :files
			     ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
			     :source "GNU ELPA" :protocol https :inherit t
			     :depth treeless :tag "0.3.0" :ref
			     "9d96d301d5bccf192daaf00dba64bca9979dcb5a"))
 (spinner :source "elpaca-menu-lock-file" :recipe
	  (:package "spinner" :repo
		    ("https://github.com/Malabarba/spinner.el" . "spinner")
		    :files ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
		    https :inherit t :depth treeless :ref
		    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
		    (:package "string-inflection" :fetcher github :repo
			      "akicho8/string-inflection" :files
			      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
			       "doc/*.texinfo" "lisp/*.el" "docs/dir"
			       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
			       (:exclude ".dir-locals.el" "test.el" "tests.el"
					 "*-test.el" "*-tests.el" "LICENSE"
					 "README*" "*-pkg.el"))
			      :source "MELPA" :protocol https :inherit t :depth
			      treeless :ref
			      "99607eb77702953c7715ec2ba5a6a840b9bea62b"))
 (subed :source "elpaca-menu-lock-file" :recipe
	(:package "subed" :repo "sachac/subed" :files ("subed/*.el") :source
		  "NonGNU ELPA" :protocol https :inherit t :depth treeless :host
		  github :ref "cfe9a6be651af2dae2e87a06ec7b4996498e92c1"))
 (substitute :source "elpaca-menu-lock-file" :recipe
	     (:package "substitute" :repo "protesilaos/substitute" :files
		       ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
		       :source "GNU ELPA" :protocol https :inherit t :depth
		       treeless :host github :ref
		       "a1afb50cea273514ab6655b136fe11973d35af96"))
 (tab-bar-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/tab-bar-extras.el") :package
			  "tab-bar-extras" :ref
			  "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (tablist :source "elpaca-menu-lock-file" :recipe
	  (:package "tablist" :fetcher github :repo "emacsorphanage/tablist"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (telega :source "elpaca-menu-lock-file" :recipe
	 (:package "telega" :fetcher github :repo "zevlg/telega.el" :files
		   (:defaults "etc" "server" "contrib" "Makefile") :source
		   "MELPA" :protocol https :inherit t :depth treeless :ref
		   "ff06f58364375c96477561f265e3dbf55a8ad231"))
 (telega-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/telega-extras.el") :package
			 "telega-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (tlon :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth nil :host github :repo
		"tlon-team/tlon.el" :package "tlon" :ref
		"6b155034cdfd6dd560189c0832282be5335ae537"))
 (tmr :source "elpaca-menu-lock-file" :recipe
      (:package "tmr" :repo ("https://github.com/protesilaos/tmr" . "tmr")
		:files
		("*" (:exclude ".git" "COPYING" "doclicense.texi" "Makefile"))
		:source "GNU ELPA" :protocol https :inherit t :depth treeless
		:ref "4944017340f32ad540016eb2ee9883c8cfb08406"))
 (tomelr :source "elpaca-menu-lock-file" :recipe
	 (:package "tomelr" :repo
		   ("https://github.com/kaushalmodi/tomelr" . "tomelr") :files
		   ("*" (:exclude ".git" "LICENSE")) :source "GNU ELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "670e0a08f625175fd80137cf69e799619bf8a381"))
 (transient :source "elpaca-menu-lock-file" :recipe
	    (:package "transient" :fetcher github :repo "magit/transient" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :host github :branch "main" :build
		      (:not elpaca--check-version) :ref
		      "aa32e0d66cc389befed7a8e8df9439d92a729daa"))
 (treepy :source "elpaca-menu-lock-file" :recipe
	 (:package "treepy" :repo "volrath/treepy.el" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "651e2634f01f346da9ec8a64613c51f54b444bc3"))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (use-package-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :protocol https :inherit t :depth nil :host
			      github :repo "benthamite/dotfiles" :files
			      ("emacs/extras/use-package-extras.el") :package
			      "use-package-extras" :ref
			      "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (vc-extras :source "elpaca-menu-lock-file" :recipe
	    (:source nil :protocol https :inherit t :depth nil :host github
		     :repo "benthamite/dotfiles" :files
		     ("emacs/extras/vc-extras.el") :package "vc-extras" :ref
		     "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (vertico :source "elpaca-menu-lock-file" :recipe
	  (:package "vertico" :repo "minad/vertico" :files
		    (:defaults "extensions/*") :fetcher github :source "MELPA"
		    :protocol https :inherit t :depth treeless :includes
		    (vertico-indexed vertico-flat vertico-grid vertico-mouse
				     vertico-quick vertico-buffer vertico-repeat
				     vertico-reverse vertico-directory
				     vertico-multiform vertico-unobtrusive)
		    :ref "d8dc49a3520ab908c2b0f62a4a8ae32ca23f2726"))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
		     (:package "visual-fill-column" :fetcher codeberg :repo
			       "joostkremers/visual-fill-column" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				"*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi" "docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el" "tests.el"
					  "*-test.el" "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https :inherit t :depth
			       treeless :ref
			       "e391b52922086ac38397a3325933900b6d90f9f0"))
 (vulpea :source "elpaca-menu-lock-file" :recipe
	 (:package "vulpea" :fetcher github :repo "d12frosted/vulpea" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "1217e4e2ef498da82663a8e23eb62a07b219887c"))
 (vulpea-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/vulpea-extras.el") :package
			 "vulpea-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (vundo :source "elpaca-menu-lock-file" :recipe
	(:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo")
		  :files ("*" (:exclude ".git" "test")) :source "GNU ELPA"
		  :protocol https :inherit t :depth treeless :ref
		  "30f85b4ae1f2a7189d44bb738b49559928d046cb"))
 (w3m :source "elpaca-menu-lock-file" :recipe
      (:package "w3m" :fetcher github :repo "emacs-w3m/emacs-w3m" :files
		(:defaults "icons"
			   (:exclude "octet.el" "mew-w3m.el" "w3m-xmas.el"
				     "doc/*.texi"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"bf3315747a469ef2692ebf57fcd2c4f853392c24"))
 (web :source "elpaca-menu-lock-file" :recipe
      (:package "web" :fetcher github :repo "nicferrier/emacs-web" :files
		("web.el") :source "MELPA" :protocol https :inherit t :depth
		treeless :ref "483188dac4bc6b409b985c9dae45f3324a425efd"))
 (websocket :source "elpaca-menu-lock-file" :recipe
	    (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
	(:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep"
		  :files ("wgrep.el") :source "MELPA" :protocol https :inherit t
		  :depth treeless :ref
		  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (window-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/window-extras.el") :package
			 "window-extras" :ref
			 "805e1294829d8ff6eee854a3e5fe34b057f6e5da"))
 (winum :source "elpaca-menu-lock-file" :recipe
	(:package "winum" :fetcher github :repo "deb0ch/emacs-winum" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			"*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :source "MELPA" :protocol https :inherit t :depth treeless :ref
	     "0a2df46e2831c3b8ddc8f688069f2b4a49898731"))
 (writegood-mode :source "elpaca-menu-lock-file" :recipe
		 (:package "writegood-mode" :repo "bnbeckwith/writegood-mode"
			   :fetcher github :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el" "tests.el"
				      "*-test.el" "*-tests.el" "LICENSE"
				      "README*" "*-pkg.el"))
			   :source "MELPA" :protocol https :inherit t :depth
			   treeless :ref
			   "d54eadeedb8bf3aa0e0a584c0a7373c69644f4b8"))
 (writeroom-mode :source "elpaca-menu-lock-file" :recipe
		 (:package "writeroom-mode" :fetcher github :repo
			   "joostkremers/writeroom-mode" :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el" "tests.el"
				      "*-test.el" "*-tests.el" "LICENSE"
				      "README*" "*-pkg.el"))
			   :source "MELPA" :protocol https :inherit t :depth
			   treeless :ref
			   "cca2b4b3cfcfea1919e1870519d79ed1a69aa5e2"))
 (xml-rpc :source "elpaca-menu-lock-file" :recipe
	  (:package "xml-rpc" :fetcher github :repo "xml-rpc-el/xml-rpc-el"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "edfcd6f93ca28af438f420cea889c5c88be0c145"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless
		 :host github :ref "f99ef76c80e6fc3fcf650c4fe34e10726594a4c4"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
	    (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :ref "d91f878729312a6beed77e6637c60497c5786efa"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
	    (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github
		      :files ("yasnippet.el" "snippets") :source "MELPA"
		      :protocol https :inherit t :depth treeless :ref
		      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
		     (:package "yasnippet-snippets" :repo
			       "AndreaCrotti/yasnippet-snippets" :fetcher github
			       :files ("*.el" "snippets" ".nosearch") :source
			       "MELPA" :protocol https :inherit t :depth
			       treeless :ref
			       "f2fdb1693719ec906c683037c5031ca1f3230d91"))
 (ytdl :source "elpaca-menu-lock-file" :recipe
       (:package "ytdl" :repo "tuedachu/ytdl" :fetcher gitlab :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "309ad5ce95368ad2e35d1c1701a1f3c0043415a3"))
 (zotra :source "elpaca-menu-lock-file" :recipe
	(:package "zotra" :fetcher github :repo "mpedramfar/zotra" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :host github :ref "fe9093b226a1678fc6c2fadd31a09d5a22ecdcf1"))
 (zotra-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/zotra-extras.el") :package "zotra-extras"
			:ref "805e1294829d8ff6eee854a3e5fe34b057f6e5da")))
