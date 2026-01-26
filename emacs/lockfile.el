((abbrev-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/abbrev-extras.el") :package
			 "abbrev-extras" :ref
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"7b67facc657a7388a53ea8bba5d6e7eba20fa3e0"))
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
 (affe :source "elpaca-menu-lock-file" :recipe
       (:package "affe" :repo "minad/affe" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "295e2fb26a2de66e13c0f8414d1ada5b090a1011"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
	      (:package "agent-shell" :fetcher github :repo
			"xenodium/agent-shell" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "562df616a8aa4723462010d3c0994549f9b1d40e"))
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
		      :ref "6d0c41d1cfd24821fb32933edf8c0c2a9bb8c847"))
 (aidermacs-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/aidermacs-extras.el") :package
			    "aidermacs-extras" :ref
			    "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
		("aio.el" "README.md" "UNLICENSE") :source "MELPA" :protocol
		https :inherit t :depth treeless :ref
		"58157e51e7eb7a4b954894ee4182564c507a2f01"))
 (alert :source "elpaca-menu-lock-file" :recipe
	(:package "alert" :fetcher github :repo "jwiegley/alert" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "79f6936ab4d85227530959811143429347a6971b"))
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
 (anki-editor :source "elpaca-menu-lock-file" :recipe
	      (:package "anki-editor" :fetcher github :repo
			"anki-editor/anki-editor" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :host github :ref
			"d50f9e35015df768feeb7efab17f6af6f938ce13"))
 (anki-editor-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :protocol https :inherit t :depth nil :host
			      github :repo "benthamite/dotfiles" :files
			      ("emacs/extras/anki-editor-extras.el") :package
			      "anki-editor-extras" :ref
			      "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (ankiorg :source "elpaca-menu-lock-file" :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github
		   :repo "orgtre/ankiorg" :package "ankiorg" :ref
		   "0a866cf128cb20c23374f92537c3caee50695baa"))
 (annas-archive :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth treeless :host
			 github :repo "benthamite/annas-archive" :package
			 "annas-archive" :ref
			 "4ee85550346317fcf222d067321315e7e4a65d38"))
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
			     "82e5c35d0de9c8db6281aed21105f09acbb69eba"))
 (async :source "elpaca-menu-lock-file" :recipe
	(:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "31cb2fea8f4bc7a593acd76187a89075d8075500"))
 (atomic-chrome :source "elpaca-menu-lock-file" :recipe
		(:package "atomic-chrome" :repo "KarimAziev/atomic-chrome"
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
			  "344247d45ae19b03751344a4c9ee4c163fc82379"))
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
		      "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		       :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
	       "bde22e73a54f6aa6c05b06492d91ca6f1e15abbc"))
 (biblio :source "elpaca-menu-lock-file" :recipe
	 (:package "biblio" :repo "cpitclaudel/biblio.el" :fetcher github :files
		   (:defaults (:exclude "biblio-core.el")) :source "MELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "bb9d6b4b962fb2a4e965d27888268b66d868766b"))
 (biblio-core :source "elpaca-menu-lock-file" :recipe
	      (:package "biblio-core" :repo "cpitclaudel/biblio.el" :fetcher
			github :files ("biblio-core.el") :source "MELPA"
			:protocol https :inherit t :depth treeless :ref
			"bb9d6b4b962fb2a4e965d27888268b66d868766b"))
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
				    "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (bibtex-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/bibtex-extras.el") :package
			 "bibtex-extras" :ref
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (breadcrumb :source "elpaca-menu-lock-file" :recipe
	     (:package "breadcrumb" :repo
		       ("https://github.com/joaotavora/breadcrumb"
			. "breadcrumb")
		       :files ("*" (:exclude ".git")) :source "GNU ELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "1d9dd90f77a594cd50b368e6efc85d44539ec209"))
 (browse-url-extras :source "elpaca-menu-lock-file" :recipe
		    (:source nil :protocol https :inherit t :depth nil :host
			     github :repo "benthamite/dotfiles" :files
			     ("emacs/extras/browse-url-extras.el") :package
			     "browse-url-extras" :ref
			     "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (bug-hunter :source "elpaca-menu-lock-file" :recipe
	     (:package "bug-hunter" :repo
		       ("https://github.com/Malabarba/elisp-bug-hunter"
			. "bug-hunter")
		       :files ("*" (:exclude ".git")) :source "GNU ELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "31a2da8fd5825f0938a1cce976baf39805b13e9f"))
 (calendar-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/calendar-extras.el") :package
			   "calendar-extras" :ref
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (calfw :source "elpaca-menu-lock-file" :recipe
	(:package "calfw" :fetcher github :repo "kiwanami/emacs-calfw" :files
		  ("calfw.el" "calfw-compat.el") :source "MELPA" :protocol https
		  :inherit t :depth treeless :ref
		  "36846cdca91794cf38fa171d5a3ac291d3ebc060"))
 (calfw-blocks :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "benthamite/calfw-blocks" :package
			"calfw-blocks" :ref
			"96ba30067a94249ee073e6c1754c6bda696bbd74"))
 (calfw-org :source "elpaca-menu-lock-file" :recipe
	    (:package "calfw-org" :fetcher github :repo "kiwanami/emacs-calfw"
		      :files ("calfw-org.el" "calfw-compat.el") :source "MELPA"
		      :protocol https :inherit t :depth treeless :ref
		      "36846cdca91794cf38fa171d5a3ac291d3ebc060"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "f8682a046a57525754ebc812ba3ae9c973db083b"))
 (casual :source "elpaca-menu-lock-file" :recipe
	 (:package "casual" :fetcher github :repo "kickingvegas/casual"
		   :old-names
		   (casual-agenda casual-bookmarks casual-calc casual-dired
				  casual-editkit casual-ibuffer casual-info
				  casual-isearch cc-isearch-menu casual-lib
				  casual-re-builder)
		   :files (:defaults "docs/images") :source "MELPA" :protocol
		   https :inherit t :depth treeless :ref
		   "1620d8ac1f555b4b4299fb7a852d59ddeaaae2e4"))
 (circe :source "elpaca-menu-lock-file" :recipe
	(:package "circe" :repo "emacs-circe/circe" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :ref "e909ff49e59c396b19564855a3f282684a4e716e"))
 (citar :source "elpaca-menu-lock-file" :recipe
	(:package "citar" :repo "emacs-citar/citar" :fetcher github :files
		  (:defaults (:exclude "citar-embark.el")) :old-names
		  (bibtex-actions) :source "MELPA" :protocol https :inherit t
		  :depth treeless :host github :includes (citar-org) :ref
		  "dc7018eb36fb3540cb5b7fc526d6747144437eef"))
 (citar-embark :source "elpaca-menu-lock-file" :recipe
	       (:package "citar-embark" :repo "emacs-citar/citar" :fetcher
			 github :files ("citar-embark.el") :source "MELPA"
			 :protocol https :inherit t :depth treeless :ref
			 "dc7018eb36fb3540cb5b7fc526d6747144437eef"))
 (citar-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/citar-extras.el") :package "citar-extras"
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		     :ref "4bde999a41803fe519ea80eab8b813d53503eebd"))
 (claude-code :source "elpaca-menu-lock-file" :recipe
	      (:package "claude-code" :fetcher github :repo
			"stevemolitor/claude-code.el" :files
			("*.el" (:exclude "images/*")) :source "MELPA" :protocol
			https :inherit t :depth treeless :host github :branch
			"main" :ref "4a9914bd4161eb43f489820f9174c62390e5adc8"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "clojure-mode" :repo "clojure-emacs/clojure-mode"
			 :fetcher github :files ("clojure-mode.el") :source
			 "MELPA" :protocol https :inherit t :depth treeless :ref
			 "467922210962061777e00d6cc46f5db2d00fb3ef"))
 (closql :source "elpaca-menu-lock-file" :recipe
	 (:package "closql" :fetcher github :repo "magit/closql" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :host github :ref "947426d0c93e5ad5374c464b2f121c36cdaf2132"))
 (codel :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth treeless :host github
		 :repo "ultronozm/codel.el" :package "codel" :ref
		 "e3409c83c1cd6e68326c1c38e3e68eaf462a245a"))
 (color-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/color-extras.el") :package "color-extras"
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (company :source "elpaca-menu-lock-file" :recipe
	  (:package "company" :fetcher github :repo "company-mode/company-mode"
		    :files
		    (:defaults "icons" ("images/small" "doc/images/small/*.png"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "fad9f207e00a851c0d96dd532c1b175326ac3e3d"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			"*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :source "MELPA" :protocol https :inherit t :depth treeless :ref
	     "0430bd1eb3493ea90d69feb6b7eb7dac3e10d0ba"))
 (consult :source "elpaca-menu-lock-file" :recipe
	  (:package "consult" :repo "minad/consult" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth treeless
		    :ref "8d1a262993f0dd0c5a221d18dcc8265b7c55e865"))
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
			treeless :ref "1497b46d6f48da2d884296a1297e5ace1e050eb5"))
 (consult-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/consult-extras.el") :package
			  "consult-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			     "e3fca5fadfa86cde5b1f2a5fc7c7669fb3423d15"))
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
		    :inherit t :depth treeless :host github :build
		    (:not elpaca--check-version) :ref
		    "7ee4758bb748beac7d29e62de5d2e752ebafb858"))
 (copilot-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/copilot-extras.el") :package
			  "copilot-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (corfu :source "elpaca-menu-lock-file" :recipe
	(:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/*")
		  :fetcher github :source "MELPA" :protocol https :inherit t
		  :depth treeless :includes
		  (corfu-info corfu-echo corfu-history corfu-popupinfo
			      corfu-quick)
		  :ref "4bca44febb6b4b1692019e142cd1504f1d06af3a"))
 (corfu-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/corfu-extras.el") :package "corfu-extras"
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (corg :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth treeless :host github
		:repo "isamert/corg.el" :package "corg" :ref
		"54c0ed1a38a216b05eca3c7d1f00b28847cd5bb1"))
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
	       "66fb78baf83525ca068c3ddd156ef0989a65bf9d"))
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
		 "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
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
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			  "9ac20488c56be0e88611881beb713cc58e02eff3"))
 (doom-modeline-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :protocol https :inherit t :depth nil :host
				github :repo "benthamite/dotfiles" :files
				("emacs/extras/doom-modeline-extras.el")
				:package "doom-modeline-extras" :ref
				"3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			       "1227c014d08c116d8fd446cf5eeb27c566289644"))
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
		 "fb0c0376a069ea720f20e2c06e1692e28f2c4375"))
 (ebib-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/ebib-extras.el") :package "ebib-extras"
		       :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		"8fa836fcd1c22f45d36249b09590b32a890f2b9e"))
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
		     :ref "5adb7097d0ff3d9e004a8bb07c0b25f7ee20ba8a"))
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
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
	      (:package "elfeed-tube" :fetcher github :repo
			"karthink/elfeed-tube" :files
			(:defaults (:exclude "elfeed-tube-mpv.el")) :source
			"MELPA" :protocol https :inherit t :depth treeless :ref
			"99e55ac428dc50bff271575cffddc5060f22087d"))
 (elfeed-tube-mpv :source "elpaca-menu-lock-file" :recipe
		  (:package "elfeed-tube-mpv" :repo "karthink/elfeed-tube"
			    :fetcher github :files ("elfeed-tube-mpv.el")
			    :source "MELPA" :protocol https :inherit t :depth
			    treeless :ref
			    "99e55ac428dc50bff271575cffddc5060f22087d"))
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
	    "1508298c1ed19c81fa4ebc5d22d945322e9e4c52" :files
	    (:defaults "elpaca-test.el" (:exclude "extensions")) :build
	    (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/elpaca-extras.el") :wait t :package
			 "elpaca-extras" :ref
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
		     (:package "elpaca-use-package" :wait t :repo
			       "https://github.com/progfolio/elpaca.git" :files
			       ("extensions/elpaca-use-package.el") :main
			       "extensions/elpaca-use-package.el" :build
			       (:not elpaca--compile-info) :source
			       "Elpaca extensions" :protocol https :inherit t
			       :depth treeless :ref
			       "1508298c1ed19c81fa4ebc5d22d945322e9e4c52"))
 (elpy :source "elpaca-menu-lock-file" :recipe
       (:package "elpy" :fetcher github :repo "jorgenschaefer/elpy" :files
		 ("*.el" "NEWS.rst" "snippets" "elpy") :source "MELPA" :protocol
		 https :inherit t :depth treeless :ref
		 "0b381f55969438ab2ccc2d1a1614045fcf7c9545"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
	  (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
		    (:defaults "README.md" "sqlite") :source "MELPA" :protocol
		    https :inherit t :depth treeless :ref
		    "d654e4fb1d0f5addd998982754519d144df4bd4c"))
 (embark :source "elpaca-menu-lock-file" :recipe
	 (:package "embark" :repo "oantolin/embark" :fetcher github :files
		   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
		 (:package "embark-consult" :repo "oantolin/embark" :fetcher
			   github :files ("embark-consult.el") :source "MELPA"
			   :protocol https :inherit t :depth treeless :ref
			   "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"))
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
		 :host github :ref "7ef178763d3d3044fd030368d66e15a0bbeed160"))
 (emsg-blame :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth treeless :host
		      github :repo "ISouthRain/emsg-blame" :package "emsg-blame"
		      :ref "34f981394aecb3747f0c95c4ae359e8f35c7c677"))
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
		      "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		     :ref "5c24d1b732e86433f34ebf5ec7ca3c985edbc744"))
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
				  "8889414ec50c3c4dfeef601900c63901a55a2237"))
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
 (flymake-mdl :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "MicahElliott/flymake-mdl" :package
		       "flymake-mdl" :ref
		       "97fe83116842944ca780de0e4c5151c0f6dcfc72"))
 (forge :source "elpaca-menu-lock-file" :recipe
	(:package "forge" :fetcher github :repo "magit/forge" :files
		  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
		  :protocol https :inherit t :depth treeless :host github
		  :branch "main" :build (:not elpaca--check-version) :ref
		  "68eabd05ce0650bce0539b2933d0b5ab5944fbd4"))
 (forge-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/forge-extras.el") :package "forge-extras"
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (forge-search :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "benthamite/forge-search.el" :branch
			"fix/forge-get-repository" :package "forge-search" :ref
			"dc792fa9cd1d26c194313f888b4c0092b6f42e03"))
 (frame-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/frame-extras.el") :package "frame-extras"
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		 "21e042438537bf2bbfdd2d25a58f5ab5c799a8f6"))
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
		  :ref "7f15e57e1a5c7dcc504457708645aef2deceb425"))
 (gptel-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth nil :host github
			:repo "benthamite/dotfiles" :files
			("emacs/extras/gptel-extras.el") :package "gptel-extras"
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (gptel-plus :source "elpaca-menu-lock-file" :recipe
	     (:source nil :protocol https :inherit t :depth treeless :host
		      github :repo "benthamite/gptel-plus" :package "gptel-plus"
		      :ref "d08a0aeadd6e4ca637997385e77da2af5916f329"))
 (gptel-quick :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "karthink/gptel-quick" :package
		       "gptel-quick" :ref
		       "018ff2be8f860a1e8fe3966eec418ad635620c38"))
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
			 "ef757c6ce226ebabc834d49db5161ec90cf82202"))
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
		      :ref "26bdadf604b34e5a6b9628f3476bf7f5e88d2c3d"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher
			 github :files (:defaults "NEWS" "logo.svg") :source
			 "MELPA" :protocol https :inherit t :depth treeless :ref
			 "383b4b77753ef83420c7a755f86e1ec4770f551b"))
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
		    "9540fc414014822dde00f0188b74e17ac99e916d"))
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
		    :ref "fa644880699adea3770504f913e6dddbec90c076"))
 (hydra :source "elpaca-menu-lock-file" :recipe
	(:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
		  (:defaults (:exclude "lv.el")) :source "MELPA" :protocol https
		  :inherit t :depth treeless :ref
		  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
	     (:package "inheritenv" :fetcher github :repo "purcell/inheritenv"
		       :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el" "tests.el"
				  "*-test.el" "*-tests.el" "LICENSE" "README*"
				  "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t :depth
		       treeless :host github :ref
		       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (init :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth nil :host github :repo
		"benthamite/init" :wait t :package "init" :ref
		"4b5156cd95530190088eb22a7a3a026291c9e894"))
 (isearch-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/isearch-extras.el") :package
			  "isearch-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		 "a58866a501b4b4ea2f73fd8b0a15659296932f5f"))
 (jinx-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/jinx-extras.el") :package "jinx-extras"
		       :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		    :ref "b831e380c4deb1d51ce5db0a965b96427aec52e4"))
 (kmacro-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/kmacro-extras.el") :package
			 "kmacro-extras" :ref
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			"9ab399186fad220f59f3c1bbbcaddabf49ed9de8"))
 (ledger-mode-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :protocol https :inherit t :depth nil :host
			      github :repo "benthamite/dotfiles" :files
			      ("emacs/extras/ledger-mode-extras.el") :package
			      "ledger-mode-extras" :ref
			      "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (lin :source "elpaca-menu-lock-file" :recipe
      (:package "lin" :repo ("https://github.com/protesilaos/lin" . "lin")
		:files ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
		:source "GNU ELPA" :protocol https :inherit t :depth treeless
		:ref "3716e1d68edbf9d38f1c6d923b5e59c79ae4ece9"))
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
		  "2a89ba755b0459914a44b1ffa793e57f759a5b85"))
 (llm :source "elpaca-menu-lock-file" :recipe
      (:package "llm" :repo ("https://github.com/ahyatt/llm" . "llm") :files
		("*" (:exclude ".git")) :source "GNU ELPA" :protocol https
		:inherit t :depth treeless :ref
		"8a63863b11410a9695f07bdbf8a974792d85910e"))
 (llm-tool-collection :source "elpaca-menu-lock-file" :recipe
		      (:source nil :protocol https :inherit t :depth treeless
			       :host github :repo "skissue/llm-tool-collection"
			       :package "llm-tool-collection" :ref
			       "a383ccf3df6c86684da77fb61ea4ebe67a21eedb"))
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
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el")
	       :source "MELPA" :protocol https :inherit t :depth treeless :ref
	       "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (macos :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth treeless :host github
		 :repo "benthamite/macos" :package "macos" :ref
		 "4f467e8c349e8a6964224886b3a8f6b7b6723881"))
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
		   ("git-hooks" "git-hooks/*")
		   (:exclude "lisp/magit-section.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :host github :branch "main" :build
		  (:not elpaca--check-version) :ref
		  "fe0c43b6f5b3b20fce9ed30d203bf1267831b14f"))
 (magit-extra :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/magit-extra.el") :package "magit-extra"
		       :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			    "4a60438fd2a349610e571f10596f6642dfab119d"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
		(:package "magit-section" :fetcher github :repo "magit/magit"
			  :files
			  ("lisp/magit-section.el" "docs/magit-section.texi"
			   "magit-section-pkg.el")
			  :source "MELPA" :protocol https :inherit t :depth
			  treeless :ref
			  "fe0c43b6f5b3b20fce9ed30d203bf1267831b14f"))
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
		       treeless :ref "fc0cee1151fced42db6014e1d29a61ed63de81d9"))
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
			  "92802fae9ebbc8c2e4c281c06dcdbd74b8bca80e"))
 (markdown-mode-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :protocol https :inherit t :depth nil :host
				github :repo "benthamite/dotfiles" :files
				("emacs/extras/markdown-mode-extras.el")
				:package "markdown-mode-extras" :ref
				"3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		"125e0a4478ff1404880ea4e593f5e4ff0122cb83"))
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
		      :ref "0e31867d879beb7076dd58eb8c34370d1264a407"))
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
			 "8a45a1393aa68cf93f1a973b00d87e3e9de4833d"))
 (modus-themes-extras :source "elpaca-menu-lock-file" :recipe
		      (:source nil :protocol https :inherit t :depth nil :host
			       github :repo "benthamite/dotfiles" :files
			       ("emacs/extras/modus-themes-extras.el") :package
			       "modus-themes-extras" :ref
			       "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (monet :source "elpaca-menu-lock-file" :recipe
	(:source nil :protocol https :inherit t :depth treeless :host github
		 :repo "stevemolitor/monet" :package "monet" :ref
		 "72a18d372fef4b0971267bf13f127dcce681859a"))
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
		(:not elpaca--compile-info) :ref
		"1a501281443eca6ccf7a7267a1c9c720bc6ccca1" :package "mu4e"))
 (mu4e-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/mu4e-extras.el") :package "mu4e-extras"
		       :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (mullvad :source "elpaca-menu-lock-file" :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github
		   :repo "benthamite/mullvad" :package "mullvad" :ref
		   "d389205d46810edd36b814a050c459f60273102a"))
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
		       "f9d240b59f9a5461d98a2eeac25901a5ce6c72e9"))
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
				  "d09ea987ed3d2cc64137234f27851594050e2b64"))
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
			     "3265d6c4b552eae457d50d423adb10494113d70b"))
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
			 "303999eb940e58bb96fe0424ef393fe3b24e8f16"))
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
		"874daf5e4791a6d4f47741422c80e2736e907351"))
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
 (ob-aider :source "elpaca-menu-lock-file" :recipe
	   (:package "ob-aider" :fetcher github :repo "localredhead/ob-aider.el"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth treeless
		     :host github :ref
		     "f611b0e733323c04bbbcab710a78a87f47e5fc74"))
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
		      :ref "fb338f771f1693436da0472a8a4d230b28af14f3"))
 (orderless-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/orderless-extras.el") :package
			    "orderless-extras" :ref
			    "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			 "690ae7e735e16eee6969ef4d79cdff021a621db1"))
 (org-contrib :source "elpaca-menu-lock-file" :recipe
	      (:package "org-contrib" :host github :repo
			"emacsmirror/org-contrib" :files (:defaults) :source
			"Org" :protocol https :inherit t :depth treeless :ref
			"90e1d6bd6288615233dae273f0525a43a9d8779d"))
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
		      "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		     "36cbdb453d950b120bee08d0f05d2ab77b2f5edd"))
 (org-gcal-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/org-gcal-extras.el") :package
			   "org-gcal-extras" :ref
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			treeless :ref "831ecfd50a29057c239b9fa55ebc02d402a6d4a7"))
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
		       treeless :ref "56006c3e4079d1742b0c5e8b50d2c643e9136087"))
 (org-modern-indent :source "elpaca-menu-lock-file" :recipe
		    (:source nil :protocol https :inherit t :depth treeless
			     :host github :repo "jdtsmith/org-modern-indent"
			     :package "org-modern-indent" :ref
			     "ebf9a8e571db523dc6e4cd9ed80d0e0626983ae4"))
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
		    :ref "327768e2c38020f6ea44730e71f2a62f3f0ce3bd"))
 (org-msg-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/org-msg-extras.el") :package
			  "org-msg-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (org-noter :source "elpaca-menu-lock-file" :recipe
	    (:package "org-noter" :fetcher github :repo "org-noter/org-noter"
		      :files
		      ("*.el" "modules"
		       (:exclude "*-test-utils.el" "*-devel.el"))
		      :source "MELPA" :protocol https :inherit t :depth treeless
		      :host github :ref
		      "81765d267e51efd8b4f5b7276000332ba3eabbf5"))
 (org-noter-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/org-noter-extras.el") :package
			    "org-noter-extras" :ref
			    "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			       "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (org-ql :source "elpaca-menu-lock-file" :recipe
	 (:package "org-ql" :fetcher github :repo "alphapapa/org-ql" :files
		   (:defaults (:exclude "helm-org-ql.el")) :source "MELPA"
		   :protocol https :inherit t :depth treeless :ref
		   "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca"))
 (org-ref :source "elpaca-menu-lock-file" :recipe
	  (:package "org-ref" :fetcher github :repo "jkitchin/org-ref" :files
		    (:defaults "org-ref.org" "org-ref.bib" "citeproc") :source
		    "MELPA" :protocol https :inherit t :depth treeless :ref
		    "dc2481d430906fe2552f9318f4405242e6d37396"))
 (org-ref-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/org-ref-extras.el") :package
			  "org-ref-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
	   (:package "org-roam" :fetcher github :repo "org-roam/org-roam" :files
		     (:defaults "extensions/*") :source "MELPA" :protocol https
		     :inherit t :depth treeless :ref
		     "c72702cf27891899ea6321fe6505ff04befaf43e"))
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
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (org-roam-ui :source "elpaca-menu-lock-file" :recipe
	      (:package "org-roam-ui" :fetcher github :repo
			"org-roam/org-roam-ui" :files ("*.el" "out") :source
			"MELPA" :protocol https :inherit t :depth treeless :host
			github :branch "main" :ref
			"2894dcbf56d2eca8d3cae2b1ae183f51724b5db6"))
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
			     "6bc151d8d7afe889ce512637771e1329ac67b51d"))
 (org-vcard :source "elpaca-menu-lock-file" :recipe
	    (:package "org-vcard" :fetcher github :repo "pinoaffe/org-vcard"
		      :files ("org-vcard.el" "styles") :source "MELPA" :protocol
		      https :inherit t :depth treeless :ref
		      "03c504c34e5c31091d971090b249064e332987d7"))
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
				"3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		  "24c8fe48c477d561c2ce1720223f8c5aec664f4e"))
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
			"c2116b8701498bd11d8674065a5429d844985e46"))
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
			treeless :ref "66d02c1d197a9506f8df7a7e35f760bdbbcddb3b"))
 (outli :source "elpaca-menu-lock-file" :recipe
	(:package "outli" :fetcher github :repo "jdtsmith/outli" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth treeless
		  :host github :ref "009e74c1757143040a0427f477ae882107b14592"))
 (outline-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/outline-extras.el") :package
			  "outline-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		    :ref "b7dc44dc28911b9d8e3055a18deac16c3b560b03"))
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
		      :ref "1caeb56a4be26597319e7288edbc2cabada151b4"))
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
			treeless :ref "8f46da90228a9ce22de24da234ba53860257640a"))
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
		     :ref "5b837e0a5b91a69cc0e5086d8e4a71d6d86dac93"))
 (pass :source "elpaca-menu-lock-file" :recipe
       (:package "pass" :fetcher github :repo "NicolasPetton/pass" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless :ref
		 "de4adfaeba5eb4d1facaf75f582f1ba36373299a"))
 (pass-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth nil :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/pass-extras.el") :package "pass-extras"
		       :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		 "paths" :ref "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		      :ref "365f88238f46f9b1425685562105881800f10386"))
 (pdf-tools-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :protocol https :inherit t :depth nil :host
			    github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/pdf-tools-extras.el") :package
			    "pdf-tools-extras" :ref
			    "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		    treeless :ref "3b4b421d5185f2c33bae478aa057dff13701cc25"))
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
			    "24e41d1952bef5953ef0af2288de146265c7ee10"))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"222f1da892462d7bea5c7a7bbcb6b5a5f4cb2158"))
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
		     :ref "14b1fd8d2a183f11b123f62e02801dc1139da9c1"))
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
		     :ref "d93828bf6c36383c365bd564ad3bab5a4403804c"))
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
		      :ref "d893429168b87003a99bf567932dce57fdac93fa"))
 (profiler-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/profiler-extras.el") :package
			   "profiler-extras" :ref
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (prot-common :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "protesilaos/dotfiles" :local-repo
		       "prot-common" :main
		       "emacs/.emacs.d/prot-lisp/prot-common.el" :build
		       (:not elpaca--check-version) :files
		       ("emacs/.emacs.d/prot-lisp/prot-common.el") :package
		       "prot-common" :ref
		       "d58f691a1984b0e8c5cce50deb31b3a5d9a675e7"))
 (prot-eww :source "elpaca-menu-lock-file" :recipe
	   (:source nil :protocol https :inherit t :depth treeless :host github
		    :repo "protesilaos/dotfiles" :local-repo "prot-eww" :main
		    "emacs/.emacs.d/prot-lisp/prot-eww.el" :build
		    (:not elpaca--check-version) :files
		    ("emacs/.emacs.d/prot-lisp/prot-eww.el") :package "prot-eww"
		    :ref "d58f691a1984b0e8c5cce50deb31b3a5d9a675e7"))
 (prot-scratch :source "elpaca-menu-lock-file" :recipe
	       (:source nil :protocol https :inherit t :depth treeless :host
			github :repo "protesilaos/dotfiles" :local-repo
			"prot-scratch" :main
			"emacs/.emacs.d/prot-lisp/prot-scratch.el" :build
			(:not elpaca--check-version) :files
			("emacs/.emacs.d/prot-lisp/prot-scratch.el") :package
			"prot-scratch" :ref
			"d58f691a1984b0e8c5cce50deb31b3a5d9a675e7"))
 (prot-simple :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "protesilaos/dotfiles" :local-repo
		       "prot-simple" :main
		       "emacs/.emacs.d/prot-lisp/prot-simple.el" :build
		       (:not elpaca--check-version) :files
		       ("emacs/.emacs.d/prot-lisp/prot-simple.el") :package
		       "prot-simple" :ref
		       "d58f691a1984b0e8c5cce50deb31b3a5d9a675e7"))
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
		       treeless :ref "8e5128ff7f722a4d68ddaa22022cb99ef9ddcf9a"))
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
		     :ref "bf364a29e2f21828941ee3d11a27127bc260740f"))
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
 (ragmacs :source "elpaca-menu-lock-file" :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github
		   :repo "positron-solutions/ragmacs" :build
		   (:not elpaca--check-version) :package "ragmacs" :ref
		   "d3ad46ded557a651faa959f1545ca4df48da78f0"))
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
			     "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (register-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :protocol https :inherit t :depth nil :host
			   github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/register-extras.el") :package
			   "register-extras" :ref
			   "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		   :host github :ref "37edf10e575aff98be128d3e656300f5b5a1e616"))
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
 (shell-maker :source "elpaca-menu-lock-file" :recipe
	      (:package "shell-maker" :fetcher github :repo
			"xenodium/shell-maker" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
			 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
			 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el" "tests.el"
				   "*-test.el" "*-tests.el" "LICENSE" "README*"
				   "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t :depth
			treeless :ref "6eafe72de916cb3e75deb4f7220085ac3e775a11"))
 (shr-heading :source "elpaca-menu-lock-file" :recipe
	      (:source nil :protocol https :inherit t :depth treeless :host
		       github :repo "oantolin/emacs-config" :files
		       ("my-lisp/shr-heading.el") :package "shr-heading" :ref
		       "26feddb4b20cf316459eba672af6388fb3abcdf3"))
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
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		  :ref "a35c605a2fe98e95d82a80f0f320fd4913418f52"))
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
			      "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"))
 (subed :source "elpaca-menu-lock-file" :recipe
	(:package "subed" :repo "sachac/subed" :files ("subed/*.el") :source
		  "NonGNU ELPA" :protocol https :inherit t :depth treeless :host
		  github :ref "0e4ed5d14af146da619d8cb6e5a8e1b5bbae168d"))
 (substitute :source "elpaca-menu-lock-file" :recipe
	     (:package "substitute" :repo "protesilaos/substitute" :files
		       ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
		       :source "GNU ELPA" :protocol https :inherit t :depth
		       treeless :host github :ref
		       "cb15c1b4c974f9a04c60af7c58590dcd85578d69"))
 (tab-bar-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :protocol https :inherit t :depth nil :host github
			  :repo "benthamite/dotfiles" :files
			  ("emacs/extras/tab-bar-extras.el") :package
			  "tab-bar-extras" :ref
			  "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
		   "e34fb226ec56b17f77a6eb99c22e98d82529d2de"))
 (telega-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/telega-extras.el") :package
			 "telega-extras" :ref
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (tmr :source "elpaca-menu-lock-file" :recipe
      (:package "tmr" :repo ("https://github.com/protesilaos/tmr" . "tmr")
		:files
		("*" (:exclude ".git" "COPYING" "doclicense.texi" "Makefile"))
		:source "GNU ELPA" :protocol https :inherit t :depth treeless
		:ref "3f34bc737417660e5aa7b009a6fd70e04860d422"))
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
		      "642343e847a5a26a0adc2493b8b83b62f407d768"))
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
			      "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (vc-extras :source "elpaca-menu-lock-file" :recipe
	    (:source nil :protocol https :inherit t :depth nil :host github
		     :repo "benthamite/dotfiles" :files
		     ("emacs/extras/vc-extras.el") :package "vc-extras" :ref
		     "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (vertico :source "elpaca-menu-lock-file" :recipe
	  (:package "vertico" :repo "minad/vertico" :files
		    (:defaults "extensions/*") :fetcher github :source "MELPA"
		    :protocol https :inherit t :depth treeless :includes
		    (vertico-indexed vertico-flat vertico-grid vertico-mouse
				     vertico-quick vertico-buffer vertico-repeat
				     vertico-reverse vertico-directory
				     vertico-multiform vertico-unobtrusive)
		    :ref "2ba626d028b9d4f2acceeebc58fcfd276be9e80f"))
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
			       "e1be9a1545157d24454d950c0ac79553c540edb7"))
 (vulpea :source "elpaca-menu-lock-file" :recipe
	 (:package "vulpea" :fetcher github :repo "d12frosted/vulpea" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth treeless
		   :ref "9c08e333029d25de4d1a4e85d8478ed31ac2b489"))
 (vulpea-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :protocol https :inherit t :depth nil :host github
			 :repo "benthamite/dotfiles" :files
			 ("emacs/extras/vulpea-extras.el") :package
			 "vulpea-extras" :ref
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
 (vundo :source "elpaca-menu-lock-file" :recipe
	(:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo")
		  :files ("*" (:exclude ".git" "test")) :source "GNU ELPA"
		  :protocol https :inherit t :depth treeless :ref
		  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (w3m :source "elpaca-menu-lock-file" :recipe
      (:package "w3m" :fetcher github :repo "emacs-w3m/emacs-w3m" :files
		(:defaults "icons"
			   (:exclude "octet.el" "mew-w3m.el" "w3m-xmas.el"
				     "doc/*.texi"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"ec18c21418bf7c1be159bd3cf7e79a370d4be1f3"))
 (wasabi :source "elpaca-menu-lock-file" :recipe
	 (:source nil :protocol https :inherit t :depth treeless :host github
		  :repo "xenodium/wasabi" :package "wasabi" :ref
		  "90424d2f1e102e83d4e8e79e4602ba52bcdde60b"))
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
			 "3e5912f500a75aa872aea57ae13a09f175df65fe"))
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
	     "902b4d572af2c2f36060da01e3c33d194cdec32b"))
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
		    :ref "56593e877468682eef355b35dbb405ddce54bd53"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth treeless
		 :host github :ref "f2369fb4985ed054be47ae111760ff2075dff72a"))
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
			       "606ee926df6839243098de6d71332a697518cb86"))
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
			:ref "3e5912f500a75aa872aea57ae13a09f175df65fe")))
