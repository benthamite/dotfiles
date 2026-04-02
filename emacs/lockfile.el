((abbrev-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "abbrev-extras" :id abbrev-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/abbrev-extras.el"
			  "emacs/extras/doc/abbrev-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
 (ace-link :source "elpaca-menu-lock-file" :recipe
	   (:package "ace-link" :repo "abo-abo/ace-link" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id ace-link :type git :protocol https
		     :inherit t :depth treeless :ref
		     "d9bd4a25a02bdfde4ea56247daf3a9ff15632ea4"))
 (ace-link-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "ace-link-extras" :id ace-link-extras
			   :host github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/ace-link-extras.el"
			    "emacs/extras/doc/ace-link-extras.texi")
			   :depth nil :type git :protocol https :inherit t :ref
			   "8177d5794e74526809e462866d6b0a7aeae955da"))
 (acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id acp :type git :protocol https :inherit t
		:depth treeless :ref "f7e20ce831ce342c457bb6860ca3d41eb183152c"))
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
				:source "MELPA" :id activity-watch-mode :type
				git :protocol https :inherit t :depth treeless
				:ref "19aed6ca81a3b1e549f47867c924d180d8536791"))
 (affe :source "elpaca-menu-lock-file" :recipe
       (:package "affe" :repo "minad/affe" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id affe :type git :protocol https :inherit t
		 :depth treeless :ref "295e2fb26a2de66e13c0f8414d1ada5b090a1011"))
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
			:source "MELPA" :id agent-shell :type git :protocol
			https :inherit t :depth treeless :ref
			"c72ff6e10855b63a418a73dff315a1c107aec86c"))
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
			      :source "MELPA" :id aggressive-indent :type git
			      :protocol https :inherit t :depth treeless :ref
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
		      :source "MELPA" :id aidermacs :type git :protocol https
		      :inherit t :depth treeless :ref
		      "6d0c41d1cfd24821fb32933edf8c0c2a9bb8c847"))
 (aidermacs-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :package "aidermacs-extras" :id aidermacs-extras
			    :host github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/aidermacs-extras.el"
			     "emacs/extras/doc/aidermacs-extras.texi")
			    :depth nil :type git :protocol https :inherit t :ref
			    "8177d5794e74526809e462866d6b0a7aeae955da"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
		("aio.el" "README.md" "UNLICENSE") :source "MELPA" :id aio :type
		git :protocol https :inherit t :depth treeless :ref
		"0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (alert :source "elpaca-menu-lock-file" :recipe
	(:package "alert" :fetcher github :repo "jwiegley/alert" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id alert :type git :protocol https :inherit t
		  :depth treeless :ref
		  "79f6936ab4d85227530959811143429347a6971b"))
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
		     :source "MELPA" :id anaphora :type git :protocol https
		     :inherit t :depth treeless :ref
		     "a755afa7db7f3fa515f8dd2c0518113be0b027f6"))
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
			:source "MELPA" :id anki-editor :host github :type git
			:protocol https :inherit t :depth treeless :ref
			"d50f9e35015df768feeb7efab17f6af6f938ce13"))
 (anki-editor-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :package "anki-editor-extras" :id
			      anki-editor-extras :host github :repo
			      "benthamite/dotfiles" :files
			      ("emacs/extras/anki-editor-extras.el"
			       "emacs/extras/doc/anki-editor-extras.texi")
			      :depth nil :type git :protocol https :inherit t
			      :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (anki-noter :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "anki-noter" :id anki-noter :host github
		      :repo "benthamite/anki-noter" :type git :protocol https
		      :inherit t :depth treeless :ref
		      "a5934373f57c083147123743ebbb16587bed0a4b"))
 (ankiorg :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "ankiorg" :id ankiorg :host github :repo
		   "orgtre/ankiorg" :type git :protocol https :inherit t :depth
		   treeless :ref "0a866cf128cb20c23374f92537c3caee50695baa"))
 (annas-archive :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "annas-archive" :id annas-archive :host
			 github :repo "benthamite/annas-archive" :type git
			 :protocol https :inherit t :depth treeless :ref
			 "b83c58590bf61eacd4aa20882a4d2869eab8973d"))
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
			     :source "MELPA" :id applescript-mode :type git
			     :protocol https :inherit t :depth treeless :ref
			     "82e5c35d0de9c8db6281aed21105f09acbb69eba"))
 (async :source "elpaca-menu-lock-file" :recipe
	(:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id async :type git :protocol https :inherit t
		  :depth treeless :ref
		  "31cb2fea8f4bc7a593acd76187a89075d8075500"))
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
			  :source "MELPA" :id atomic-chrome :host github :type
			  git :protocol https :inherit t :depth treeless :ref
			  "344247d45ae19b03751344a4c9ee4c163fc82379"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id avy :type git :protocol https :inherit t
		:depth treeless :ref "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (avy-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "avy-extras" :id avy-extras :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/avy-extras.el"
		       "emacs/extras/doc/avy-extras.texi")
		      :depth nil :type git :protocol https :inherit t :ref
		      "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			:source "MELPA" :id back-button :type git :protocol
			https :inherit t :depth treeless :ref
			"f8783c98a7fefc1d0419959c1b462c7dcadce5a8"))
 (bbdb :source "elpaca-menu-lock-file" :recipe
       (:package "bbdb" :fetcher git :url
		 "https://git.savannah.nongnu.org/git/bbdb.git" :files
		 (:defaults "lisp/*.el") :source "MELPA" :id bbdb :host github
		 :repo "emacsmirror/bbdb" :pre-build
		 (("./autogen.sh") ("./configure") ("make")) :build
		 (:not elpaca-build-docs) :type git :protocol https :inherit t
		 :depth treeless :ref "53e8ba04c47b3542db75b68f9663941daf2e6ca4"))
 (bbdb-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "bbdb-extras" :id bbdb-extras :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/bbdb-extras.el"
			"emacs/extras/doc/bbdb-extras.texi")
		       :depth nil :type git :protocol https :inherit t :ref
		       "8177d5794e74526809e462866d6b0a7aeae955da"))
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
		       :source "MELPA" :id bbdb-vcard :type git :protocol https
		       :inherit t :depth treeless :ref
		       "113c66115ce68316e209f51ebce56de8dded3606"))
 (bib :source "elpaca-menu-lock-file" :recipe
      (:source nil :package "bib" :id bib :host github :repo "benthamite/bib"
	       :depth nil :type git :protocol https :inherit t :ref
	       "d8d65a7bd02d7fefed85f0e341f26569ff3bcd77"))
 (biblio :source "elpaca-menu-lock-file" :recipe
	 (:package "biblio" :repo "cpitclaudel/biblio.el" :fetcher github :files
		   (:defaults (:exclude "biblio-core.el")) :source "MELPA" :id
		   biblio :type git :protocol https :inherit t :depth treeless
		   :ref "bb9d6b4b962fb2a4e965d27888268b66d868766b"))
 (biblio-core :source "elpaca-menu-lock-file" :recipe
	      (:package "biblio-core" :repo "cpitclaudel/biblio.el" :fetcher
			github :files ("biblio-core.el") :source "MELPA" :id
			biblio-core :type git :protocol https :inherit t :depth
			treeless :ref "bb9d6b4b962fb2a4e965d27888268b66d868766b"))
 (bibtex-completion :source "elpaca-menu-lock-file" :recipe
		    (:package "bibtex-completion" :fetcher github :repo
			      "tmalsburg/helm-bibtex" :files
			      ("bibtex-completion.el") :source "MELPA" :id
			      bibtex-completion :version (lambda (_) "2.0.0")
			      :type git :protocol https :inherit t :depth
			      treeless :ref
			      "6064e8625b2958f34d6d40312903a85c173b5261"))
 (bibtex-completion-extras :source "elpaca-menu-lock-file" :recipe
			   (:source nil :package "bibtex-completion-extras" :id
				    bibtex-completion-extras :host github :repo
				    "benthamite/dotfiles" :files
				    ("emacs/extras/bibtex-completion-extras.el"
				     "emacs/extras/doc/bibtex-completion-extras.texi")
				    :depth nil :type git :protocol https
				    :inherit t :ref
				    "8177d5794e74526809e462866d6b0a7aeae955da"))
 (bibtex-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "bibtex-extras" :id bibtex-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/bibtex-extras.el"
			  "emacs/extras/doc/bibtex-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
 (breadcrumb :source "elpaca-menu-lock-file" :recipe
	     (:package "breadcrumb" :repo
		       ("https://github.com/joaotavora/breadcrumb"
			. "breadcrumb")
		       :tar "1.0.1" :host gnu :files ("*" (:exclude ".git"))
		       :source "GNU ELPA" :id breadcrumb :type git :protocol
		       https :inherit t :depth treeless :ref
		       "1d9dd90f77a594cd50b368e6efc85d44539ec209"))
 (browse-url-extras :source "elpaca-menu-lock-file" :recipe
		    (:source nil :package "browse-url-extras" :id
			     browse-url-extras :host github :repo
			     "benthamite/dotfiles" :files
			     ("emacs/extras/browse-url-extras.el"
			      "emacs/extras/doc/browse-url-extras.texi")
			     :depth nil :type git :protocol https :inherit t
			     :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (bug-hunter :source "elpaca-menu-lock-file" :recipe
	     (:package "bug-hunter" :repo
		       ("https://github.com/Malabarba/elisp-bug-hunter"
			. "bug-hunter")
		       :tar "1.3.1" :host gnu :files ("*" (:exclude ".git"))
		       :source "GNU ELPA" :id bug-hunter :type git :protocol
		       https :inherit t :depth treeless :ref
		       "31a2da8fd5825f0938a1cce976baf39805b13e9f"))
 (calendar-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "calendar-extras" :id calendar-extras
			   :host github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/calendar-extras.el"
			    "emacs/extras/doc/calendar-extras.texi")
			   :depth nil :type git :protocol https :inherit t :ref
			   "8177d5794e74526809e462866d6b0a7aeae955da"))
 (calfw :source "elpaca-menu-lock-file" :recipe
	(:package "calfw" :fetcher github :repo "kiwanami/emacs-calfw" :files
		  ("calfw.el" "calfw-compat.el") :source "MELPA" :id calfw :type
		  git :protocol https :inherit t :depth treeless :ref
		  "36846cdca91794cf38fa171d5a3ac291d3ebc060"))
 (calfw-blocks :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "calfw-blocks" :id calfw-blocks :host
			github :repo "benthamite/calfw-blocks" :type git
			:protocol https :inherit t :depth treeless :ref
			"96ba30067a94249ee073e6c1754c6bda696bbd74"))
 (calfw-org :source "elpaca-menu-lock-file" :recipe
	    (:package "calfw-org" :fetcher github :repo "kiwanami/emacs-calfw"
		      :files ("calfw-org.el" "calfw-compat.el") :source "MELPA"
		      :id calfw-org :type git :protocol https :inherit t :depth
		      treeless :ref "36846cdca91794cf38fa171d5a3ac291d3ebc060"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id cape :type git :protocol https :inherit t
		 :depth treeless :ref "2b2a5c5bef16eddcce507d9b5804e5a0cc9481ae"))
 (casual :source "elpaca-menu-lock-file" :recipe
	 (:package "casual" :fetcher github :repo "kickingvegas/casual"
		   :old-names
		   (casual-agenda casual-bookmarks casual-calc casual-dired
				  casual-editkit casual-ibuffer casual-info
				  casual-isearch cc-isearch-menu casual-lib
				  casual-re-builder)
		   :files (:defaults "docs/images") :source "MELPA" :id casual
		   :type git :protocol https :inherit t :depth treeless :ref
		   "0b04ad703efad2a53a3c756c02784e819d0f8b4e"))
 (circe :source "elpaca-menu-lock-file" :recipe
	(:package "circe" :repo "emacs-circe/circe" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id circe :type git :protocol https :inherit t
		  :depth treeless :ref
		  "e909ff49e59c396b19564855a3f282684a4e716e"))
 (citar :source "elpaca-menu-lock-file" :recipe
	(:package "citar" :repo "emacs-citar/citar" :fetcher github :files
		  (:defaults (:exclude "citar-embark.el")) :old-names
		  (bibtex-actions) :source "MELPA" :id citar :host github
		  :includes (citar-org) :type git :protocol https :inherit t
		  :depth treeless :ref
		  "911a7d59c4ac94318fc6b6fb6f55840ad04482ad"))
 (citar-embark :source "elpaca-menu-lock-file" :recipe
	       (:package "citar-embark" :repo "emacs-citar/citar" :fetcher
			 github :files ("citar-embark.el") :source "MELPA" :id
			 citar-embark :type git :protocol https :inherit t
			 :depth treeless :ref
			 "911a7d59c4ac94318fc6b6fb6f55840ad04482ad"))
 (citar-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "citar-extras" :id citar-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/citar-extras.el"
			 "emacs/extras/doc/citar-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
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
			   :source "MELPA" :id citar-org-roam :host github :type
			   git :protocol https :inherit t :depth treeless :ref
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
		     :source "MELPA" :id citeproc :type git :protocol https
		     :inherit t :depth treeless :ref
		     "4bde999a41803fe519ea80eab8b813d53503eebd"))
 (claude-code :source "elpaca-menu-lock-file" :recipe
	      (:package "claude-code" :fetcher github :repo
			"stevemolitor/claude-code.el" :files
			("*.el" (:exclude "images/*")) :source "MELPA" :id
			claude-code :host github :branch "main" :type git
			:protocol https :inherit t :depth treeless :ref
			"4a9914bd4161eb43f489820f9174c62390e5adc8"))
 (claude-code-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :package "claude-code-extras" :id
			      claude-code-extras :host github :repo
			      "benthamite/dotfiles" :files
			      ("emacs/extras/claude-code-extras.el"
			       "emacs/extras/doc/claude-code-extras.texi")
			      :depth nil :type git :protocol https :inherit t
			      :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (agent-log :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "agent-log" :id agent-log :host github
		      :repo "benthamite/agent-log" :type git :protocol https
		      :inherit t :depth treeless :ref
		      "74b697f5064220ff52436e6f4ae57284421caae7"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "clojure-mode" :repo "clojure-emacs/clojure-mode"
			 :fetcher github :files ("clojure-mode.el") :source
			 "MELPA" :id clojure-mode :type git :protocol https
			 :inherit t :depth treeless :ref
			 "e7d8b57d4f37318040fe1eb71d5e1c8892d4db5c"))
 (closql :source "elpaca-menu-lock-file" :recipe
	 (:package "closql" :fetcher github :repo "magit/closql" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id closql :host github :type git :protocol
		   https :inherit t :depth treeless :ref
		   "947426d0c93e5ad5374c464b2f121c36cdaf2132"))
 (color-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "color-extras" :id color-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/color-extras.el"
			 "emacs/extras/doc/color-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (company :source "elpaca-menu-lock-file" :recipe
	  (:package "company" :fetcher github :repo "company-mode/company-mode"
		    :files
		    (:defaults "icons" ("images/small" "doc/images/small/*.png"))
		    :source "MELPA" :id company :type git :protocol https
		    :inherit t :depth treeless :ref
		    "42d3897308a992cd2268ba2d4e2ec013fc6c961e"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			"*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :source "MELPA" :id cond-let :type git :protocol https :inherit t
	     :depth treeless :ref "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
	  (:package "consult" :repo "minad/consult" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id consult :type git :protocol https
		    :inherit t :depth treeless :ref
		    "f8c2ef57e83af3d45e345e5c14089f2f9973682b"))
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
			:source "MELPA" :id consult-dir :type git :protocol
			https :inherit t :depth treeless :ref
			"1497b46d6f48da2d884296a1297e5ace1e050eb5"))
 (consult-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "consult-extras" :id consult-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/consult-extras.el"
			   "emacs/extras/doc/consult-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			     :source "MELPA" :id consult-flycheck :type git
			     :protocol https :inherit t :depth treeless :ref
			     "9fe96c4b75c8566170ad41a04c3849d2e2606104"))
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
				 :source "MELPA" :id consult-git-log-grep :type
				 git :protocol https :inherit t :depth treeless
				 :ref "5b1669ebaff9a91000ea185264cfcb850885d21f"))
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
			 :source "MELPA" :id consult-todo :type git :protocol
			 https :inherit t :depth treeless :ref
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
			      :source "MELPA" :id consult-yasnippet :type git
			      :protocol https :inherit t :depth treeless :ref
			      "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
 (copilot :source "elpaca-menu-lock-file" :recipe
	  (:package "copilot" :fetcher github :repo "copilot-emacs/copilot.el"
		    :files ("dist" "*.el") :source "MELPA" :id copilot :host
		    github :build (:not elpaca-check-version) :type git
		    :protocol https :inherit t :depth treeless :ref
		    "59a4a292236ac9bea8756c0a0613b750b14d91eb"))
 (copilot-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "copilot-extras" :id copilot-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/copilot-extras.el"
			   "emacs/extras/doc/copilot-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
 (corfu :source "elpaca-menu-lock-file" :recipe
	(:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/*")
		  :fetcher github :source "MELPA" :id corfu :includes
		  (corfu-info corfu-echo corfu-history corfu-popupinfo
			      corfu-quick)
		  :type git :protocol https :inherit t :depth treeless :ref
		  "d2a995c5c732d0fc439efe09440870a9de779a74"))
 (corfu-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "corfu-extras" :id corfu-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/corfu-extras.el"
			 "emacs/extras/doc/corfu-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (corg :source "elpaca-menu-lock-file" :recipe
       (:source nil :package "corg" :id corg :host github :repo
		"isamert/corg.el" :type git :protocol https :inherit t :depth
		treeless :ref "54c0ed1a38a216b05eca3c7d1f00b28847cd5bb1"))
 (crux :source "elpaca-menu-lock-file" :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id crux :type git :protocol https :inherit t
		 :depth treeless :ref "3b72275fce66162770b53cf72eb72515c3e68492"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "csv-mode" :repo
		     ("https://github.com/emacsmirror/gnu_elpa" . "csv-mode")
		     :tar "1.27" :host gnu :branch "externals/csv-mode" :files
		     ("*" (:exclude ".git")) :source "GNU ELPA" :id csv-mode
		     :type git :protocol https :inherit t :depth treeless :ref
		     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (ct :source "elpaca-menu-lock-file" :recipe
     (:package "ct" :fetcher github :repo "neeasade/ct.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :id ct :type git :protocol https :inherit t
	       :depth treeless :ref "66fb78baf83525ca068c3ddd156ef0989a65bf9d"))
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
			  :source "MELPA" :id curl-to-elisp :type git :protocol
			  https :inherit t :depth treeless :ref
			  "63d8d9c6d5efb8af8aa88042bfc0690ba699ef64"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
		 ("dash.el" "dash.texi") :source "MELPA" :id dash :type git
		 :protocol https :inherit t :depth treeless :ref
		 "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (deferred :source "elpaca-menu-lock-file" :recipe
	   (:package "deferred" :repo "kiwanami/emacs-deferred" :fetcher github
		     :files ("deferred.el") :source "MELPA" :id deferred :type
		     git :protocol https :inherit t :depth treeless :ref
		     "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (dired-du :source "elpaca-menu-lock-file" :recipe
	   (:package "dired-du" :repo
		     ("https://github.com/emacsmirror/gnu_elpa" . "dired-du")
		     :tar "0.5.2" :host gnu :branch "externals/dired-du" :files
		     ("*" (:exclude ".git")) :source "GNU ELPA" :id dired-du
		     :type git :protocol https :inherit t :depth treeless :ref
		     "f7e1593e94388b0dfb71af8e9a3d5d07edf5a159"))
 (dired-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "dired-extras" :id dired-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/dired-extras.el"
			 "emacs/extras/doc/dired-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (dired-git-info :source "elpaca-menu-lock-file" :recipe
		 (:package "dired-git-info" :repo
			   ("https://github.com/clemera/dired-git-info"
			    . "dired-git-info")
			   :tar "0.3.1" :host gnu :files ("*" (:exclude ".git"))
			   :source "GNU ELPA" :id dired-git-info :type git
			   :protocol https :inherit t :depth treeless :ref
			   "91d57e3a4c5104c66a3abc18e281ee55e8979176"))
 (dired-hacks :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "dired-hacks" :id dired-hacks :host github
		       :repo "Fuco1/dired-hacks" :type git :protocol https
		       :inherit t :depth treeless :ref
		       "de9336f4b47ef901799fe95315fa080fa6d77b48"))
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
			     :source "MELPA" :id dired-quick-sort :type git
			     :protocol https :inherit t :depth treeless :ref
			     "7f01a60997b5fa8c5d572dece9c5db16b1438b9b"))
 (djvu :source "elpaca-menu-lock-file" :recipe
       (:package "djvu" :repo
		 ("https://github.com/emacsmirror/gnu_elpa" . "djvu") :tar
		 "1.1.2" :host gnu :branch "externals/djvu" :files
		 ("*" (:exclude ".git")) :source "GNU ELPA" :id djvu :type git
		 :protocol https :inherit t :depth treeless :ref
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
			  :source "MELPA" :id doom-modeline :build
			  (:not elpaca-check-version) :type git :protocol https
			  :inherit t :depth treeless :ref
			  "febde435ae62b36466154c0d41649e7b4ee5685e"))
 (doom-modeline-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :package "doom-modeline-extras" :id
				doom-modeline-extras :host github :repo
				"benthamite/dotfiles" :files
				("emacs/extras/doom-modeline-extras.el"
				 "emacs/extras/doc/doom-modeline-extras.texi")
				:depth nil :type git :protocol https :inherit t
				:ref "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			       :source "MELPA" :id dwim-shell-command :host
			       github :type git :protocol https :inherit t
			       :depth treeless :ref
			       "eb86e3fbd2c775345fdc018ece27138a779730f5"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo "akib/emacs-eat" :tar "0.9.4" :host codeberg :files
		("*.el" ("term" "term/*.el") "*.texi" "*.ti"
		 ("terminfo/e" "terminfo/e/*") ("terminfo/65" "terminfo/65/*")
		 ("integration" "integration/*")
		 (:exclude ".dir-locals.el" "*-tests.el"))
		:source "NonGNU ELPA" :id eat :type git :protocol https :inherit
		t :depth treeless :ref
		"c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (eat-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "eat-extras" :id eat-extras :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/eat-extras.el"
		       "emacs/extras/doc/eat-extras.texi")
		      :depth nil :type git :protocol https :inherit t :ref
		      "8177d5794e74526809e462866d6b0a7aeae955da"))
 (ebib :source "elpaca-menu-lock-file" :recipe
       (:package "ebib" :fetcher github :repo "joostkremers/ebib" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id ebib :type git :protocol https :inherit t
		 :depth treeless :ref "fb0c0376a069ea720f20e2c06e1692e28f2c4375"))
 (ebib-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "ebib-extras" :id ebib-extras :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/ebib-extras.el"
			"emacs/extras/doc/ebib-extras.texi")
		       :depth nil :type git :protocol https :inherit t :ref
		       "8177d5794e74526809e462866d6b0a7aeae955da"))
 (ediff-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "ediff-extras" :id ediff-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/ediff-extras.el"
			 "emacs/extras/doc/ediff-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
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
			  :source "MELPA" :id edit-indirect :type git :protocol
			  https :inherit t :depth treeless :ref
			  "82a28d8a85277cfe453af464603ea330eae41c05"))
 (ein :source "elpaca-menu-lock-file" :recipe
      (:package "ein" :repo "millejoh/emacs-ipython-notebook" :fetcher github
		:files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id ein :type git :protocol https :inherit t
		:depth treeless :ref "8fa836fcd1c22f45d36249b09590b32a890f2b9e"))
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
		     :source "MELPA" :id el-patch :type git :protocol https
		     :inherit t :depth treeless :ref
		     "5adb7097d0ff3d9e004a8bb07c0b25f7ee20ba8a"))
 (elfeed :source "elpaca-menu-lock-file" :recipe
	 (:package "elfeed" :repo "benthamite/elfeed" :fetcher github :files
		   (:defaults "README.md") :source "MELPA" :id elfeed :host
		   github :branch "debounce-search-update" :type git :protocol
		   https :inherit t :depth treeless :ref
		   "0e7a94e931e7989fb6f2900d2345bcb88d3a2a45"))
 (elfeed-ai :source "elpaca-menu-lock-file" :recipe
	    (:package "elfeed-ai" :fetcher github :repo "benthamite/elfeed-ai"
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :id elfeed-ai :host github :type git
		      :protocol https :inherit t :depth treeless :ref
		      "329ae01a8b45ab100a5e35a5f532c861261de3b2"))
 (elfeed-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "elfeed-extras" :id elfeed-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/elfeed-extras.el"
			  "emacs/extras/doc/elfeed-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
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
		       :source "MELPA" :id elfeed-org :type git :protocol https
		       :inherit t :depth treeless :ref
		       "34c0b4d758942822e01a5dbe66b236e49a960583"))
 (elfeed-tube :source "elpaca-menu-lock-file" :recipe
	      (:package "elfeed-tube" :fetcher github :repo
			"benthamite/elfeed-tube" :files
			(:defaults (:exclude "elfeed-tube-mpv.el")) :source
			"MELPA" :id elfeed-tube :host github :type git :protocol
			https :inherit t :depth treeless :ref
			"65657a6b97058cac296eea9b9eb761c835548213"))
 (elfeed-tube-mpv :source "elpaca-menu-lock-file" :recipe
		  (:package "elfeed-tube-mpv" :repo "benthamite/elfeed-tube"
			    :fetcher github :files ("elfeed-tube-mpv.el")
			    :source "MELPA" :id elfeed-tube-mpv :host github
			    :type git :protocol https :inherit t :depth treeless
			    :ref "65657a6b97058cac296eea9b9eb761c835548213"))
 (elgantt :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "elgantt" :id elgantt :host github :repo
		   "legalnonsense/elgantt" :type git :protocol https :inherit t
		   :depth treeless :ref
		   "23fe6a3dd4f1a991e077f13869fb960b8b29e183"))
 (elgrep :source "elpaca-menu-lock-file" :recipe
	 (:package "elgrep" :repo "TobiasZawada/elgrep" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id elgrep :type git :protocol https :inherit
		   t :depth treeless :ref
		   "329eaf2e9e994e5535c7f7fe2685ec21d8323384"))
 (elisp-demos :source "elpaca-menu-lock-file" :recipe
	      (:package "elisp-demos" :fetcher github :repo
			"xuchunyang/elisp-demos" :files (:defaults "*.org")
			:source "MELPA" :id elisp-demos :type git :protocol
			https :inherit t :depth treeless :ref
			"1a108d1c5011f9ced58be2ca98bea1fbd4130a2f"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
	     (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github
		       :files (:defaults (:exclude "elisp-refs-bench.el"))
		       :source "MELPA" :id elisp-refs :type git :protocol https
		       :inherit t :depth treeless :ref
		       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
	    "https://github.com/benthamite/elpaca.git" :ref
	    "c57962b9bd13d84f71a5f661c8b576484bd3dba3" :depth 1 :inherit ignore
	    :files (:defaults "elpaca-test.el" (:exclude "extensions")) :build
	    (:not elpaca-activate) :type git :protocol https))
 (elpaca-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "elpaca-extras" :id elpaca-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/elpaca-extras.el"
			  "emacs/extras/doc/elpaca-extras.texi")
			 :depth nil :wait t :type git :protocol https :inherit t
			 :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
		     (:package "elpaca-use-package" :wait t :repo
			       "https://github.com/progfolio/elpaca.git" :files
			       ("extensions/elpaca-use-package.el") :main
			       "extensions/elpaca-use-package.el" :build
			       (:not elpaca-build-docs) :source
			       "Elpaca extensions" :id elpaca-use-package :type
			       git :protocol https :inherit t :depth treeless
			       :ref "5543a4f0019edefe9d7f6697d551276d1ed2b28c"))
 (elpy :source "elpaca-menu-lock-file" :recipe
       (:package "elpy" :fetcher github :repo "jorgenschaefer/elpy" :files
		 ("*.el" "NEWS.rst" "snippets" "elpy") :source "MELPA" :id elpy
		 :type git :protocol https :inherit t :depth treeless :ref
		 "0b381f55969438ab2ccc2d1a1614045fcf7c9545"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
	  (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
		    (:defaults "README.md" "sqlite") :source "MELPA" :id emacsql
		    :type git :protocol https :inherit t :depth treeless :ref
		    "6ef7473248fce048c7daa00c3b2ae75af987c89c"))
 (embark :source "elpaca-menu-lock-file" :recipe
	 (:package "embark" :repo "oantolin/embark" :fetcher github :files
		   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA"
		   :id embark :type git :protocol https :inherit t :depth
		   treeless :ref "e0238889b1c946514fd967d21d70599af9c4e887"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
		 (:package "embark-consult" :repo "oantolin/embark" :fetcher
			   github :files ("embark-consult.el") :source "MELPA"
			   :id embark-consult :type git :protocol https :inherit
			   t :depth treeless :ref
			   "e0238889b1c946514fd967d21d70599af9c4e887"))
 (emojify :source "elpaca-menu-lock-file" :recipe
	  (:package "emojify" :fetcher github :repo "iqbalansari/emacs-emojify"
		    :files (:defaults "data" "images") :source "MELPA" :id
		    emojify :type git :protocol https :inherit t :depth treeless
		    :ref "1b726412f19896abf5e4857d4c32220e33400b55"))
 (empv :source "elpaca-menu-lock-file" :recipe
       (:package "empv" :fetcher github :repo "isamert/empv.el" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id empv :host github :type git :protocol https
		 :inherit t :depth treeless :ref
		 "6a00b33030fffcadf209f81432708a85386b3f3f"))
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
			:source "MELPA" :id engine-mode :type git :protocol
			https :inherit t :depth treeless :ref
			"e7f317f1b284853b6df4dfd37ab7715b248e0ebd"))
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
				       :source "MELPA" :id
				       eshell-syntax-highlighting :type git
				       :protocol https :inherit t :depth
				       treeless :ref
				       "62418fd8b2380114a3f6dad699c1ba45329db1d2"))
 (esi-dictate :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "esi-dictate" :id esi-dictate :host
		       sourcehut :repo "lepisma/emacs-speech-input" :type git
		       :protocol https :inherit t :depth treeless :ref
		       "f3d523e2ec4354bbe68e2c2a74fe3d65a5b4ac10"))
 (esxml :source "elpaca-menu-lock-file" :recipe
	(:package "esxml" :fetcher github :repo "tali713/esxml" :files
		  ("esxml.el" "esxml-query.el") :source "MELPA" :id esxml :type
		  git :protocol https :inherit t :depth treeless :ref
		  "affada143fed7e2da08f2b3d927a027f26ad4a8f"))
 (eww-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "eww-extras" :id eww-extras :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/eww-extras.el"
		       "emacs/extras/doc/eww-extras.texi")
		      :depth nil :type git :protocol https :inherit t :ref
		      "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			  :source "MELPA" :id expand-region :type git :protocol
			  https :inherit t :depth treeless :ref
			  "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	      :source "MELPA" :id f :type git :protocol https :inherit t :depth
	      treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (faces-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "faces-extras" :id faces-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/faces-extras.el"
			 "emacs/extras/doc/faces-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (fatebook :source "elpaca-menu-lock-file" :recipe
	   (:source nil :package "fatebook" :id fatebook :repo
		    "sonofhypnos/fatebook.el" :host github :files
		    ("fatebook.el") :type git :protocol https :inherit t :depth
		    treeless :ref "7b70876ea0de1ee78047600e4dfc07bf8069916f"))
 (files-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "files-extras" :id files-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/files-extras.el"
			 "emacs/extras/doc/files-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
	   (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id flycheck :type git :protocol https
		     :inherit t :depth treeless :ref
		     "fea21359413f59b15f36ef2e237399341237259f"))
 (flycheck-languagetool :source "elpaca-menu-lock-file" :recipe
			(:package "flycheck-languagetool" :repo
				  "benthamite/flycheck-languagetool" :fetcher
				  github :files
				  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
				   "*.texinfo" "doc/dir" "doc/*.info"
				   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				   "docs/dir" "docs/*.info" "docs/*.texi"
				   "docs/*.texinfo"
				   (:exclude ".dir-locals.el" "test.el"
					     "tests.el" "*-test.el" "*-tests.el"
					     "LICENSE" "README*" "*-pkg.el"))
				  :source "MELPA" :id flycheck-languagetool
				  :host github :branch
				  "fix/guard-stale-buffer-positions" :type git
				  :protocol https :inherit t :depth treeless
				  :ref
				  "46b32d0af28a1eba2e853534c31204cfe08fd692"))
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
			    :source "MELPA" :id flycheck-ledger :type git
			    :protocol https :inherit t :depth treeless :ref
			    "48bed9193c8601b142245df03968ae493b7d430c"))
 (flymake-mdl :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "flymake-mdl" :id flymake-mdl :host github
		       :repo "MicahElliott/flymake-mdl" :type git :protocol
		       https :inherit t :depth treeless :ref
		       "97fe83116842944ca780de0e4c5151c0f6dcfc72"))
 (forge :source "elpaca-menu-lock-file" :recipe
	(:package "forge" :fetcher github :repo "magit/forge" :files
		  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
		  :id forge :host github :branch "main" :build
		  (:not elpaca-check-version) :type git :protocol https :inherit
		  t :depth treeless :ref
		  "86ed2978236d3f39a083e6a07f4819019c91ae5a"))
 (forge-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "forge-extras" :id forge-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/forge-extras.el"
			 "emacs/extras/doc/forge-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (forge-search :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "forge-search" :id forge-search :host
			github :repo "benthamite/forge-search.el" :branch
			"fix/forge-get-repository" :type git :protocol https
			:inherit t :depth treeless :ref
			"dc792fa9cd1d26c194313f888b4c0092b6f42e03"))
 (frame-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "frame-extras" :id frame-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/frame-extras.el"
			 "emacs/extras/doc/frame-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id gcmh :type git :protocol https :inherit t
		 :depth treeless :ref "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (gdocs :source "elpaca-menu-lock-file" :recipe
	(:source nil :package "gdocs" :id gdocs :host github :repo
		 "benthamite/gdocs" :type git :protocol https :inherit t :depth
		 treeless :ref "57f1de605addaea581e24142a6d47d8bb885a5fb"))
 (gdrive :source "elpaca-menu-lock-file" :recipe
	 (:source nil :package "gdrive" :id gdrive :host github :repo
		  "benthamite/gdrive" :depth nil :type git :protocol https
		  :inherit t :ref "a2c99e87097bb2a8317a551a7b9456d6d2a9352c"))
 (gh :source "elpaca-menu-lock-file" :recipe
     (:package "gh" :repo "sigma/gh.el" :fetcher github :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :id gh :version (lambda (_) "2.29") :type git
	       :protocol https :inherit t :depth treeless :ref
	       "b1551245d3404eac6394abaebe1a9e0b2c504235"))
 (ghub :source "elpaca-menu-lock-file" :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
		 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
		 :id ghub :host github :build (:not elpaca-check-version)
		 :branch "main" :type git :protocol https :inherit t :depth
		 treeless :ref "c22858596c1f5a1f5b439e475e7ba0e6a2e1718b"))
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
				 :source "MELPA" :id git-auto-commit-mode :type
				 git :protocol https :inherit t :depth treeless
				 :ref "a7b59acea622a737d23c783ce7d212fefb29f7e6"))
 (gntp :source "elpaca-menu-lock-file" :recipe
       (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id gntp :type git :protocol https :inherit t
		 :depth treeless :ref "767571135e2c0985944017dc59b0be79af222ef5"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
	  (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
		    ("go-mode.el") :source "MELPA" :id go-mode :type git
		    :protocol https :inherit t :depth treeless :ref
		    "0ed3c5227e7f622589f1411b4939c3ee34711ebd"))
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
			     :source "MELPA" :id goto-last-change :type git
			     :protocol https :inherit t :depth treeless :ref
			     "58b0928bc255b47aad318cd183a5dce8f62199cc"))
 (gptel :source "elpaca-menu-lock-file" :recipe
	(:package "gptel" :repo "karthink/gptel" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id gptel :type git :protocol https :inherit t
		  :depth treeless :ref
		  "9682e8f5f96a72c858fb3420003642194793b32f"))
 (gptel-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "gptel-extras" :id gptel-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/gptel-extras.el"
			 "emacs/extras/doc/gptel-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (gptel-plus :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "gptel-plus" :id gptel-plus :host github
		      :repo "benthamite/gptel-plus" :type git :protocol https
		      :inherit t :depth treeless :ref
		      "ce478c153916ce9310fb58285c3dbe04c89adb70"))
 (gptel-quick :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "gptel-quick" :id gptel-quick :host github
		       :repo "karthink/gptel-quick" :type git :protocol https
		       :inherit t :depth treeless :ref
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
			 :source "MELPA" :id graphql-mode :type git :protocol
			 https :inherit t :depth treeless :ref
			 "d7f105a4bfcffa54bdc6a6f3d6eb740c561355c2"))
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
		      :source "MELPA" :id grip-mode :type git :protocol https
		      :inherit t :depth treeless :ref
		      "b8b9e603edbb258ab38a94a0518c4a8c7a22e53c"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher
			 github :files (:defaults "NEWS" "logo.svg") :source
			 "MELPA" :id haskell-mode :type git :protocol https
			 :inherit t :depth treeless :ref
			 "2dd755a5fa11577a9388af88f385d2a8e18f7a8d"))
 (helpful :source "elpaca-menu-lock-file" :recipe
	  (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id helpful :type git :protocol https
		    :inherit t :depth treeless :ref
		    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
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
				  :source "MELPA" :id highlight-indentation
				  :type git :protocol https :inherit t :depth
				  treeless :ref
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
				  :source "MELPA" :id highlight-parentheses
				  :type git :protocol https :inherit t :depth
				  treeless :ref
				  "965b18dd69eff4457e17c9e84b3cbfdbfca2ddfb"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
	  (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id hl-todo :build
		    (:not elpaca-check-version) :type git :protocol https
		    :inherit t :depth treeless :ref
		    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (hsluv :source "elpaca-menu-lock-file" :recipe
	(:package "hsluv" :fetcher github :repo "hsluv/hsluv-emacs" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id hsluv :type git :protocol https :inherit t
		  :depth treeless :ref
		  "c3bc5228e30d66e7dee9ff1a0694c2b976862fc0"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :id ht :type git :protocol https :inherit t
	       :depth treeless :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmlize :source "elpaca-menu-lock-file" :recipe
	  (:package "htmlize" :fetcher github :repo "emacsorphanage/htmlize"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id htmlize :type git :protocol https
		    :inherit t :depth treeless :ref
		    "fa644880699adea3770504f913e6dddbec90c076"))
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
		       :source "MELPA" :id inheritenv :host github :type git
		       :protocol https :inherit t :depth treeless :ref
		       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (init :source "elpaca-menu-lock-file" :recipe
       (:source nil :package "init" :id init :host github :repo
		"benthamite/init" :depth nil :wait t :type git :protocol https
		:inherit t :ref "a521d1cf9509206c0d01e8419d5c66920ded8ad7"))
 (institution-calendar :source "elpaca-menu-lock-file" :recipe
		       (:source nil :package "institution-calendar" :id
				institution-calendar :host github :repo
				"protesilaos/institution-calendar" :type git
				:protocol https :inherit t :depth treeless :ref
				"282d21b533f4ee9059cd3d5f6453f32a32326d05"))
 (isearch-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "isearch-extras" :id isearch-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/isearch-extras.el"
			   "emacs/extras/doc/isearch-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
 (jeison :source "elpaca-menu-lock-file" :recipe
	 (:package "jeison" :repo "SavchenkoValeriy/jeison" :fetcher github
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id jeison :type git :protocol https :inherit
		   t :depth treeless :ref
		   "19a51770f24eaa7b538c7be6a8a5c25d154b641f"))
 (jinx :source "elpaca-menu-lock-file" :recipe
       (:package "jinx" :repo "minad/jinx" :files
		 (:defaults "jinx-mod.c" "emacs-module.h") :fetcher github
		 :source "MELPA" :id jinx :type git :protocol https :inherit t
		 :depth treeless :ref "61bed3f77d37ae02100e8a2ec1cfb849d649fa5d"))
 (jinx-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "jinx-extras" :id jinx-extras :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/jinx-extras.el"
			"emacs/extras/doc/jinx-extras.texi")
		       :depth nil :type git :protocol https :inherit t :ref
		       "8177d5794e74526809e462866d6b0a7aeae955da"))
 (johnson :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "johnson" :id johnson :host github :repo
		   "benthamite/johnson" :type git :protocol https :inherit t
		   :depth treeless :ref
		   "f9d329ec1effd8c1158f28928640d6dcf658abe5"))
 (js2-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id js2-mode :type git :protocol https
		     :inherit t :depth treeless :ref
		     "e0c302872de4d26a9c1614fac8d6b94112b96307"))
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
		      :source "MELPA" :id json-mode :type git :protocol https
		      :inherit t :depth treeless :ref
		      "466d5b563721bbeffac3f610aefaac15a39d90a9"))
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
			  :source "MELPA" :id json-snatcher :type git :protocol
			  https :inherit t :depth treeless :ref
			  "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (kelly :source "elpaca-menu-lock-file" :recipe
	(:source nil :package "kelly" :id kelly :host github :repo
		 "benthamite/kelly" :type git :protocol https :inherit t :depth
		 treeless :ref "a828f1c7642de49c00bce4bed7de3a3a85bbb3f7"))
 (keycast :source "elpaca-menu-lock-file" :recipe
	  (:package "keycast" :fetcher github :repo "tarsius/keycast" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id keycast :type git :protocol https
		    :inherit t :depth treeless :ref
		    "b831e380c4deb1d51ce5db0a965b96427aec52e4"))
 (kmacro-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "kmacro-extras" :id kmacro-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/kmacro-extras.el"
			  "emacs/extras/doc/kmacro-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			       :source "MELPA" :id language-detection :type git
			       :protocol https :inherit t :depth treeless :ref
			       "54a6ecf55304fba7d215ef38a4ec96daff2f35a4"))
 (ledger-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode"
			:files ("ledger-*.el" "doc/*.texi") :old-names
			(ldg-mode) :source "MELPA" :id ledger-mode :type git
			:protocol https :inherit t :depth treeless :ref
			"9ab399186fad220f59f3c1bbbcaddabf49ed9de8"))
 (ledger-mode-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :package "ledger-mode-extras" :id
			      ledger-mode-extras :host github :repo
			      "benthamite/dotfiles" :files
			      ("emacs/extras/ledger-mode-extras.el"
			       "emacs/extras/doc/ledger-mode-extras.texi")
			      :depth nil :type git :protocol https :inherit t
			      :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (lin :source "elpaca-menu-lock-file" :recipe
      (:package "lin" :repo ("https://github.com/protesilaos/lin" . "lin") :tar
		"2.0.0" :host gnu :files
		("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
		"GNU ELPA" :id lin :type git :protocol https :inherit t :depth
		treeless :ref "7356aca603393cd4732ec2437a4fd19c6ab11c7d"))
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
		       :source "MELPA" :id list-utils :type git :protocol https
		       :inherit t :depth treeless :ref
		       "bbea0e7cc7ab7d96e7f062014bde438aa8ffcd43"))
 (llama :source "elpaca-menu-lock-file" :recipe
	(:package "llama" :fetcher github :repo "tarsius/llama" :files
		  ("llama.el" ".dir-locals.el") :source "MELPA" :id llama :type
		  git :protocol https :inherit t :depth treeless :ref
		  "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (llm :source "elpaca-menu-lock-file" :recipe
      (:package "llm" :repo ("https://github.com/ahyatt/llm" . "llm") :tar
		"0.29.0" :host gnu :files ("*" (:exclude ".git")) :source
		"GNU ELPA" :id llm :type git :protocol https :inherit t :depth
		treeless :ref "951524c93502d658538b5c9a0a3f2706211bdb12"))
 (llm-tool-collection :source "elpaca-menu-lock-file" :recipe
		      (:source nil :package "llm-tool-collection" :id
			       llm-tool-collection :host github :repo
			       "skissue/llm-tool-collection" :type git :protocol
			       https :inherit t :depth treeless :ref
			       "b9fd45bedf3e0fb07d289730991199ae18785157"))
 (log4e :source "elpaca-menu-lock-file" :recipe
	(:package "log4e" :repo "aki2o/log4e" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id log4e :type git :protocol https :inherit t
		  :depth treeless :ref
		  "6d71462df9bf595d3861bfb328377346aceed422"))
 (logito :source "elpaca-menu-lock-file" :recipe
	 (:package "logito" :repo "sigma/logito" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id logito :type git :protocol https :inherit
		   t :depth treeless :ref
		   "d5934ce10ba3a70d3fcfb94d742ce3b9136ce124"))
 (macos :source "elpaca-menu-lock-file" :recipe
	(:source nil :package "macos" :id macos :host github :repo
		 "benthamite/macos" :type git :protocol https :inherit t :depth
		 treeless :ref "4f467e8c349e8a6964224886b3a8f6b7b6723881"))
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
		      :source "MELPA" :id macrostep :type git :protocol https
		      :inherit t :depth treeless :ref
		      "d0928626b4711dcf9f8f90439d23701118724199"))
 (magit :source "elpaca-menu-lock-file" :recipe
	(:package "magit" :fetcher github :repo "magit/magit" :files
		  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
		   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
		   ("git-hooks" "git-hooks/*")
		   (:exclude "lisp/magit-section.el"))
		  :source "MELPA" :id magit :host github :branch "main" :build
		  (:not elpaca-check-version) :type git :protocol https :inherit
		  t :depth treeless :ref
		  "b9f19bae4d5e5c485d2d8d7bf52364eeb7d22a6b"))
 (magit-extra :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "magit-extra" :id magit-extra :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/magit-extra.el"
			"emacs/extras/doc/magit-extra.texi")
		       :depth nil :type git :protocol https :inherit t :ref
		       "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			    :source "MELPA" :id magit-gptcommit :type git
			    :protocol https :inherit t :depth treeless :ref
			    "4a60438fd2a349610e571f10596f6642dfab119d"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
		(:package "magit-section" :fetcher github :repo "magit/magit"
			  :files
			  ("lisp/magit-section.el" "docs/magit-section.texi"
			   "magit-section-pkg.el")
			  :source "MELPA" :id magit-section :type git :protocol
			  https :inherit t :depth treeless :ref
			  "b9f19bae4d5e5c485d2d8d7bf52364eeb7d22a6b"))
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
		       :source "MELPA" :id marginalia :type git :protocol https
		       :inherit t :depth treeless :ref
		       "d28a5e5c1a2e5f3e6669b0197f38da84e08f94a0"))
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
			  :source "MELPA" :id markdown-mode :type git :protocol
			  https :inherit t :depth treeless :ref
			  "107a368a6deffa943544c220b5a6b1304ffc9945"))
 (markdown-mode-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :package "markdown-mode-extras" :id
				markdown-mode-extras :host github :repo
				"benthamite/dotfiles" :files
				("emacs/extras/markdown-mode-extras.el"
				 "emacs/extras/doc/markdown-mode-extras.texi")
				:depth nil :type git :protocol https :inherit t
				:ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (marshal :source "elpaca-menu-lock-file" :recipe
	  (:package "marshal" :fetcher github :repo "sigma/marshal.el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id marshal :type git :protocol https
		    :inherit t :depth treeless :ref
		    "bc00044d9073482f589aad959e34d563598f682a"))
 (mcp :source "elpaca-menu-lock-file" :recipe
      (:package "mcp" :fetcher github :repo "lizqwerscott/mcp.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id mcp :host github :build
		(:not elpaca-check-version) :type git :protocol https :inherit t
		:depth treeless :ref "5c105a8db470eb9777fdbd26251548dec42c03f0"))
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
		      :source "MELPA" :id mediawiki :type git :protocol https
		      :inherit t :depth treeless :ref
		      "e7b229450ac5383c4cde2639836cb0dc99220127"))
 (mercado-libre :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "mercado-libre" :id mercado-libre :host
			 github :repo "benthamite/mercado-libre" :type git
			 :protocol https :inherit t :depth treeless :ref
			 "f70ee561d22452f04aabadd9007aad27ff3485cc"))
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
			 :source "MELPA" :id modus-themes :host github :type git
			 :protocol https :inherit t :depth treeless :ref
			 "ed36e34132bacaf8e87c7310837f57757a43b0c9"))
 (modus-themes-extras :source "elpaca-menu-lock-file" :recipe
		      (:source nil :package "modus-themes-extras" :id
			       modus-themes-extras :host github :repo
			       "benthamite/dotfiles" :files
			       ("emacs/extras/modus-themes-extras.el"
				"emacs/extras/doc/modus-themes-extras.texi")
			       :depth nil :type git :protocol https :inherit t
			       :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (monet :source "elpaca-menu-lock-file" :recipe
	(:source nil :package "monet" :id monet :host github :repo
		 "stevemolitor/monet" :type git :protocol https :inherit t
		 :depth treeless :ref "72a18d372fef4b0971267bf13f127dcce681859a"))
 (moon-reader :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "moon-reader" :id moon-reader :host github
		       :repo "benthamite/moon-reader" :type git :protocol https
		       :inherit t :depth treeless :ref
		       "e10c4189e53952a855a15b0bafe7205465bc423f"))
 (mpv :source "elpaca-menu-lock-file" :recipe
      (:package "mpv" :repo "kljohann/mpv.el" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id mpv :type git :protocol https :inherit t
		:depth treeless :ref "62cb8825d525d7c9475dd93d62ba84d419bc4832"))
 (mu4e :source "elpaca-menu-lock-file" :recipe
       (:source nil :package "mu4e" :id mu4e :host github :files
		("mu4e/*.el" "build/mu4e/mu4e-meta.el"
		 "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info")
		:repo "djcb/mu" :main "mu4e/mu4e.el" :pre-build
		(("./autogen.sh") ("ninja" "-C" "build")) :build
		(:not elpaca-build-docs) :ref
		"1a501281443eca6ccf7a7267a1c9c720bc6ccca1" :depth nil :type git
		:protocol https :inherit t))
 (mu4e-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "mu4e-extras" :id mu4e-extras :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/mu4e-extras.el"
			"emacs/extras/doc/mu4e-extras.texi")
		       :depth nil :type git :protocol https :inherit t :ref
		       "8177d5794e74526809e462866d6b0a7aeae955da"))
 (mullvad :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "mullvad" :id mullvad :host github :repo
		   "benthamite/mullvad" :type git :protocol https :inherit t
		   :depth treeless :ref
		   "71e4b4b33aa6392eb6d14467dfc665aa2d461838"))
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
		      :source "MELPA" :id nav-flash :type git :protocol https
		      :inherit t :depth treeless :ref
		      "5d4b48567862f6be0ca973d6b1dca90e4815cb9b"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
	     (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el"
		       :fetcher github :files (:defaults "data") :source "MELPA"
		       :id nerd-icons :type git :protocol https :inherit t
		       :depth treeless :ref
		       "9a7f44db9a53567f04603bc88d05402cad49c64c"))
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
				  :source "MELPA" :id nerd-icons-completion
				  :type git :protocol https :inherit t :depth
				  treeless :ref
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
			     :source "MELPA" :id nerd-icons-dired :type git
			     :protocol https :inherit t :depth treeless :ref
			     "929b62f01b93d30a3f42cc507fc45c84a2457b3f"))
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
			 :source "MELPA" :id no-littering :wait t :type git
			 :protocol https :inherit t :depth treeless :ref
			 "3b1d39d6b37f1f6f7dbda46712e40bc700ac79d2"))
 (nov :source "elpaca-menu-lock-file" :recipe
      (:package "nov" :fetcher git :url "https://depp.brause.cc/nov.el.git"
		:files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id nov :type git :protocol https :inherit t
		:depth treeless :ref "874daf5e4791a6d4f47741422c80e2736e907351"))
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
			:source "MELPA" :id oauth2-auto :host github :protocol
			ssh :type git :inherit t :depth treeless :ref
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
		     :source "MELPA" :id ob-aider :host github :type git
		     :protocol https :inherit t :depth treeless :ref
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
			  :source "MELPA" :id ob-typescript :type git :protocol
			  https :inherit t :depth treeless :ref
			  "5fe1762f8d8692dd5b6f1697bedbbf4cae9ef036"))
 (ol-emacs-slack :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "ol-emacs-slack" :id ol-emacs-slack :host
			  github :repo "ag91/ol-emacs-slack" :type git :protocol
			  https :inherit t :depth treeless :ref
			  "93ecc9d5fbe94f1693a4aa0226a09f35b55d9acd"))
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
		      :source "MELPA" :id orderless :type git :protocol https
		      :inherit t :depth treeless :ref
		      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (orderless-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :package "orderless-extras" :id orderless-extras
			    :host github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/orderless-extras.el"
			     "emacs/extras/doc/orderless-extras.texi")
			    :depth nil :type git :protocol https :inherit t :ref
			    "8177d5794e74526809e462866d6b0a7aeae955da"))
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
		       :source "MELPA" :id org-appear :type git :protocol https
		       :inherit t :depth treeless :ref
		       "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (org-archive-hierarchically :source "elpaca-menu-lock-file" :recipe
			     (:source nil :package "org-archive-hierarchically"
				      :id org-archive-hierarchically :host
				      gitlab :repo
				      "andersjohansson/org-archive-hierarchically"
				      :type git :protocol https :inherit t
				      :depth treeless :ref
				      "c7ddf3f36570e50d6163e7a4e3099c2c8117f894"))
 (org-autosort :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "org-autosort" :id org-autosort :host
			github :repo "yantar92/org-autosort" :type git :protocol
			https :inherit t :depth treeless :ref
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
				  :source "MELPA" :id org-clock-convenience
				  :type git :protocol https :inherit t :depth
				  treeless :ref
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
			    :source "MELPA" :id org-clock-split :host github
			    :branch "support-emacs-29.1" :type git :protocol
			    https :inherit t :depth treeless :ref
			    "65b7872864038a458990418947cd94b8907f7e38"))
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
			 :source "MELPA" :id org-contacts :build
			 (:not elpaca-check-version) :type git :protocol https
			 :inherit t :depth treeless :ref
			 "a9175a2918d2e6d5400368d9504a617e4eb2b49c"))
 (org-contrib :source "elpaca-menu-lock-file" :recipe
	      (:package "org-contrib" :host github :repo
			"emacsmirror/org-contrib" :files (:defaults) :source
			"Org" :id org-contrib :type git :protocol https :inherit
			t :depth treeless :ref
			"b840bdabd1867f9d51ee36bef7bac4be7073288c"))
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
			 :source "MELPA" :id org-download :type git :protocol
			 https :inherit t :depth treeless :ref
			 "c8be2611786d1d8d666b7b4f73582de1093f25ac"))
 (org-extras :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "org-extras" :id org-extras :host github
		      :repo "benthamite/dotfiles" :files
		      ("emacs/extras/org-extras.el"
		       "emacs/extras/doc/org-extras.texi")
		      :depth nil :type git :protocol https :inherit t :ref
		      "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-gcal :source "elpaca-menu-lock-file" :recipe
	   (:package "org-gcal" :fetcher github :repo "benthamite/org-gcal.el"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id org-gcal :host github :branch
		     "fix/strip-html-descriptions" :build
		     (:not elpaca-check-version) :type git :protocol https
		     :inherit t :depth treeless :ref
		     "3feab0b811b8305811d4753463c1a5788fb33017"))
 (org-gcal-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "org-gcal-extras" :id org-gcal-extras
			   :host github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/org-gcal-extras.el"
			    "emacs/extras/doc/org-gcal-extras.texi")
			   :depth nil :type git :protocol https :inherit t :ref
			   "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-indent-pixel :source "elpaca-menu-lock-file" :recipe
		   (:source nil :package "org-indent-pixel" :id org-indent-pixel
			    :host github :repo "benthamite/org-indent-pixel"
			    :type git :protocol https :inherit t :depth treeless
			    :ref "e7303c20c78690c0cea7f02458be62a1b39eff0d"))
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
			:source "MELPA" :id org-journal :type git :protocol
			https :inherit t :depth treeless :ref
			"831ecfd50a29057c239b9fa55ebc02d402a6d4a7"))
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
			 :source "MELPA" :id org-make-toc :type git :protocol
			 https :inherit t :depth treeless :ref
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
		       :source "MELPA" :id org-modern :type git :protocol https
		       :inherit t :depth treeless :ref
		       "f514a2570da0f7a8ff0d72641458dbcf96ccf702"))
 (org-modern-indent :source "elpaca-menu-lock-file" :recipe
		    (:source nil :package "org-modern-indent" :id
			     org-modern-indent :host github :repo
			     "jdtsmith/org-modern-indent" :type git :protocol
			     https :inherit t :depth treeless :ref
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
		    :source "MELPA" :id org-msg :type git :protocol https
		    :inherit t :depth treeless :ref
		    "aa608b399586fb771ad37045a837f8286a0b6124"))
 (org-msg-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "org-msg-extras" :id org-msg-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/org-msg-extras.el"
			   "emacs/extras/doc/org-msg-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-noter :source "elpaca-menu-lock-file" :recipe
	    (:package "org-noter" :fetcher github :repo "org-noter/org-noter"
		      :files
		      ("*.el" "modules"
		       (:exclude "*-test-utils.el" "*-devel.el"))
		      :source "MELPA" :id org-noter :host github :type git
		      :protocol https :inherit t :depth treeless :ref
		      "81765d267e51efd8b4f5b7276000332ba3eabbf5"))
 (org-noter-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :package "org-noter-extras" :id org-noter-extras
			    :host github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/org-noter-extras.el"
			     "emacs/extras/doc/org-noter-extras.texi")
			    :depth nil :type git :protocol https :inherit t :ref
			    "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-pdftools :source "elpaca-menu-lock-file" :recipe
	       (:package "org-pdftools" :fetcher github :repo
			 "fuxialexander/org-pdftools" :files ("org-pdftools.el")
			 :old-names (org-pdfview) :source "MELPA" :id
			 org-pdftools :build (:not elpaca-check-version) :type
			 git :protocol https :inherit t :depth treeless :ref
			 "2b3357828a4c2dfba8f87c906d64035d8bf221f2"))
 (org-pomodoro :source "elpaca-menu-lock-file" :recipe
	       (:package "org-pomodoro" :fetcher github :repo
			 "marcinkoziej/org-pomodoro" :files
			 (:defaults "resources") :source "MELPA" :id
			 org-pomodoro :type git :protocol https :inherit t
			 :depth treeless :ref
			 "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
 (org-pomodoro-extras :source "elpaca-menu-lock-file" :recipe
		      (:source nil :package "org-pomodoro-extras" :id
			       org-pomodoro-extras :host github :repo
			       "benthamite/dotfiles" :files
			       ("emacs/extras/org-pomodoro-extras.el"
				"emacs/extras/doc/org-pomodoro-extras.texi")
			       :depth nil :type git :protocol https :inherit t
			       :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-ql :source "elpaca-menu-lock-file" :recipe
	 (:package "org-ql" :fetcher github :repo "alphapapa/org-ql" :files
		   (:defaults (:exclude "helm-org-ql.el")) :source "MELPA" :id
		   org-ql :type git :protocol https :inherit t :depth treeless
		   :ref "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca"))
 (org-ref :source "elpaca-menu-lock-file" :recipe
	  (:package "org-ref" :fetcher github :repo "jkitchin/org-ref" :files
		    (:defaults "org-ref.org" "org-ref.bib" "citeproc") :source
		    "MELPA" :id org-ref :type git :protocol https :inherit t
		    :depth treeless :ref
		    "dc2481d430906fe2552f9318f4405242e6d37396"))
 (org-ref-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "org-ref-extras" :id org-ref-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/org-ref-extras.el"
			   "emacs/extras/doc/org-ref-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
	   (:package "org-roam" :fetcher github :repo "benthamite/org-roam"
		     :files (:defaults "extensions/*") :source "MELPA" :id
		     org-roam :host github :branch "fix/handle-nil-db-version"
		     :type git :protocol https :inherit t :depth treeless :ref
		     "0c94c89901a7d37ff2f9c618a7f991233040f913"))
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
			    :source "MELPA" :id org-roam-bibtex :type git
			    :protocol https :inherit t :depth treeless :ref
			    "b065198f2c3bc2a47ae520acd2b1e00e7b0171e6"))
 (org-roam-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "org-roam-extras" :id org-roam-extras
			   :host github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/org-roam-extras.el"
			    "emacs/extras/doc/org-roam-extras.texi")
			   :depth nil :type git :protocol https :inherit t :ref
			   "8177d5794e74526809e462866d6b0a7aeae955da"))
 (org-roam-ui :source "elpaca-menu-lock-file" :recipe
	      (:package "org-roam-ui" :fetcher github :repo
			"org-roam/org-roam-ui" :files ("*.el" "out") :source
			"MELPA" :id org-roam-ui :host github :branch "main"
			:type git :protocol https :inherit t :depth treeless
			:ref "2894dcbf56d2eca8d3cae2b1ae183f51724b5db6"))
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
			     :source "MELPA" :id org-super-agenda :type git
			     :protocol https :inherit t :depth treeless :ref
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
		     :source "MELPA" :id org-tidy :type git :protocol https
		     :inherit t :depth treeless :ref
		     "0bea3a2ceaa999e0ad195ba525c5c1dcf5fba43b"))
 (org-transclusion :source "elpaca-menu-lock-file" :recipe
		   (:package "org-transclusion" :repo
			     ("https://github.com/nobiot/org-transclusion"
			      . "org-transclusion")
			     :tar "1.4.0" :host gnu :files
			     ("*" (:exclude ".git")) :source "GNU ELPA" :id
			     org-transclusion :type git :protocol https :inherit
			     t :depth treeless :ref
			     "f70f5b8cb58ed148c5eec7273b7e9923441b94a3"))
 (org-vcard :source "elpaca-menu-lock-file" :recipe
	    (:package "org-vcard" :fetcher github :repo "pinoaffe/org-vcard"
		      :files ("org-vcard.el" "styles") :source "MELPA" :id
		      org-vcard :type git :protocol https :inherit t :depth
		      treeless :ref "03c504c34e5c31091d971090b249064e332987d7"))
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
			  :source "MELPA" :id org-web-tools :type git :protocol
			  https :inherit t :depth treeless :ref
			  "7a6498f442fc7f29504745649948635c7165d847"))
 (org-web-tools-extras :source "elpaca-menu-lock-file" :recipe
		       (:source nil :package "org-web-tools-extras" :id
				org-web-tools-extras :host github :repo
				"benthamite/dotfiles" :files
				("emacs/extras/org-web-tools-extras.el"
				 "emacs/extras/doc/org-web-tools-extras.texi")
				:depth nil :type git :protocol https :inherit t
				:ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (orgit :source "elpaca-menu-lock-file" :recipe
	(:package "orgit" :fetcher github :repo "magit/orgit" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id orgit :build (:not elpaca-check-version)
		  :type git :protocol https :inherit t :depth treeless :ref
		  "4fb91faff3bf32dac5f6f932654c280cd1f190f7"))
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
			:source "MELPA" :id orgit-forge :build
			(:not elpaca-check-version) :type git :protocol https
			:inherit t :depth treeless :ref
			"c2116b8701498bd11d8674065a5429d844985e46"))
 (orgtbl-edit :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "orgtbl-edit" :id orgtbl-edit :host github
		       :repo "shankar2k/orgtbl-edit" :type git :protocol https
		       :inherit t :depth treeless :ref
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
			:source "MELPA" :id orgtbl-join :type git :protocol
			https :inherit t :depth treeless :ref
			"bd9edf54bdd55d1d33b8c6fb51a7f23a78c09355"))
 (outli :source "elpaca-menu-lock-file" :recipe
	(:package "outli" :fetcher github :repo "jdtsmith/outli" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id outli :host github :type git :protocol
		  https :inherit t :depth treeless :ref
		  "009e74c1757143040a0427f477ae882107b14592"))
 (outline-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "outline-extras" :id outline-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/outline-extras.el"
			   "emacs/extras/doc/outline-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
 (ov :source "elpaca-menu-lock-file" :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :id ov :type git :protocol https :inherit t
	       :depth treeless :ref "e2971ad986b6ac441e9849031d34c56c980cf40b"))
 (ox-clip :source "elpaca-menu-lock-file" :recipe
	  (:package "ox-clip" :fetcher github :repo "jkitchin/ox-clip" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id ox-clip :type git :protocol https
		    :inherit t :depth treeless :ref
		    "a549cc8e1747beb6b7e567ffac27e31ba45cb8e8"))
 (ox-gfm :source "elpaca-menu-lock-file" :recipe
	 (:package "ox-gfm" :fetcher github :repo "larstvei/ox-gfm" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id ox-gfm :type git :protocol https :inherit
		   t :depth treeless :ref
		   "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb"))
 (ox-hugo :source "elpaca-menu-lock-file" :recipe
	  (:package "ox-hugo" :fetcher github :repo "kaushalmodi/ox-hugo" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id ox-hugo :type git :protocol https
		    :inherit t :depth treeless :ref
		    "b7dc44dc28911b9d8e3055a18deac16c3b560b03"))
 (ox-hugo-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "ox-hugo-extras" :id ox-hugo-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/ox-hugo-extras.el"
			   "emacs/extras/doc/ox-hugo-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
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
		      :source "MELPA" :id ox-pandoc :type git :protocol https
		      :inherit t :depth treeless :ref
		      "1caeb56a4be26597319e7288edbc2cabada151b4"))
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
			:source "MELPA" :id pandoc-mode :type git :protocol
			https :inherit t :depth treeless :ref
			"8f46da90228a9ce22de24da234ba53860257640a"))
 (pangram :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "pangram" :id pangram :host github :repo
		   "benthamite/pangram" :type git :protocol https :inherit t
		   :depth treeless :ref
		   "999decd75f4e304ea79d6ccca762adcadc3f1f8c"))
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
		     :source "MELPA" :id parsebib :type git :protocol https
		     :inherit t :depth treeless :ref
		     "5b837e0a5b91a69cc0e5086d8e4a71d6d86dac93"))
 (pass :source "elpaca-menu-lock-file" :recipe
       (:package "pass" :fetcher github :repo "NicolasPetton/pass" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id pass :type git :protocol https :inherit t
		 :depth treeless :ref "143456809fd2dbece9f241f4361085e1de0b0e75"))
 (pass-extras :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "pass-extras" :id pass-extras :host github
		       :repo "benthamite/dotfiles" :files
		       ("emacs/extras/pass-extras.el"
			"emacs/extras/doc/pass-extras.texi")
		       :depth nil :type git :protocol https :inherit t :ref
		       "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			       :source "MELPA" :id password-generator :host
			       github :type git :protocol https :inherit t
			       :depth treeless :ref
			       "2d0deb52f2fd978bff9001e155e36ac5bd287d52"))
 (password-store :source "elpaca-menu-lock-file" :recipe
		 (:package "password-store" :fetcher github :repo
			   "zx2c4/password-store" :files ("contrib/emacs/*.el")
			   :source "MELPA" :id password-store :type git
			   :protocol https :inherit t :depth treeless :ref
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
			       :source "MELPA" :id password-store-otp :version
			       (lambda (_) "0.1.5") :type git :protocol https
			       :inherit t :depth treeless :ref
			       "be3a00a981921ed1b2f78012944dc25eb5a0beca"))
 (paths :source "elpaca-menu-lock-file" :recipe
	(:source nil :package "paths" :id paths :host github :repo
		 "benthamite/dotfiles" :files
		 ("emacs/extras/paths.el" "emacs/extras/doc/paths.texi") :depth
		 nil :type git :protocol https :inherit t :ref
		 "8177d5794e74526809e462866d6b0a7aeae955da"))
 (pcache :source "elpaca-menu-lock-file" :recipe
	 (:package "pcache" :repo "sigma/pcache" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id pcache :type git :protocol https :inherit
		   t :depth treeless :ref
		   "e287b5d116679f79789ee9ee22ee213dc6cef68c"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
	  (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id pcre2el :type git :protocol https
		    :inherit t :depth treeless :ref
		    "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
	    (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools"
		      :files
		      (:defaults "README" ("build" "Makefile")
				 ("build" "server"))
		      :source "MELPA" :id pdf-tools :type git :protocol https
		      :inherit t :depth treeless :ref
		      "365f88238f46f9b1425685562105881800f10386"))
 (pdf-tools-extras :source "elpaca-menu-lock-file" :recipe
		   (:source nil :package "pdf-tools-extras" :id pdf-tools-extras
			    :host github :repo "benthamite/dotfiles" :files
			    ("emacs/extras/pdf-tools-extras.el"
			     "emacs/extras/doc/pdf-tools-extras.texi")
			    :depth nil :type git :protocol https :inherit t :ref
			    "8177d5794e74526809e462866d6b0a7aeae955da"))
 (pdf-tools-pages :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "pdf-tools-pages" :id pdf-tools-pages
			   :host github :repo "benthamite/pdf-tools-pages" :type
			   git :protocol https :inherit t :depth treeless :ref
			   "1dfde538f70168c38d6323d619b3d47d0623ee7a"))
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
			     :source "MELPA" :id pdf-view-restore :type git
			     :protocol https :inherit t :depth treeless :ref
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
		       :source "MELPA" :id peep-dired :type git :protocol https
		       :inherit t :depth treeless :ref
		       "1cb81016dd78a7afca48ad1dba6dc7996ec6e167"))
 (persist :source "elpaca-menu-lock-file" :recipe
	  (:package "persist" :repo
		    ("https://github.com/emacsmirror/gnu_elpa" . "persist") :tar
		    "0.8" :host gnu :branch "externals/persist" :files
		    ("*" (:exclude ".git")) :source "GNU ELPA" :id persist :type
		    git :protocol https :inherit t :depth treeless :ref
		    "3b4b421d5185f2c33bae478aa057dff13701cc25"))
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
			       :source "MELPA" :id persistent-scratch :type git
			       :protocol https :inherit t :depth treeless :ref
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
			    :source "MELPA" :id persistent-soft :type git
			    :protocol https :inherit t :depth treeless :ref
			    "c94b34332529df573bad8a97f70f5a35d5da7333"))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :id pet :type git :protocol https :inherit t
		:depth treeless :ref "222f1da892462d7bea5c7a7bbcb6b5a5f4cb2158"))
 (plz :source "elpaca-menu-lock-file"
   :recipe
   (:package "plz" :repo ("https://github.com/alphapapa/plz.el.git" . "plz")
	     :tar "0.9.1" :host gnu :files ("*" (:exclude ".git" "LICENSE"))
	     :source "GNU ELPA" :id plz :type git :protocol https :inherit t
	     :depth treeless :ref "e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e"))
 (plz-event-source :source "elpaca-menu-lock-file" :recipe
		   (:package "plz-event-source" :repo
			     ("https://github.com/r0man/plz-event-source"
			      . "plz-event-source")
			     :tar "0.1.3" :host gnu :files
			     ("*" (:exclude ".git")) :source "GNU ELPA" :id
			     plz-event-source :type git :protocol https :inherit
			     t :depth treeless :ref
			     "de89214ce14e2b82cbfdc30e1adcf3e77b1f250a"))
 (plz-media-type :source "elpaca-menu-lock-file" :recipe
		 (:package "plz-media-type" :repo
			   ("https://github.com/r0man/plz-media-type"
			    . "plz-media-type")
			   :tar "0.2.4" :host gnu :files ("*" (:exclude ".git"))
			   :source "GNU ELPA" :id plz-media-type :type git
			   :protocol https :inherit t :depth treeless :ref
			   "b1127982d53affff082447030cda6e8ead3899cb"))
 (polymarket :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "polymarket" :id polymarket :host github
		      :repo "benthamite/polymarket" :type git :protocol https
		      :inherit t :depth treeless :ref
		      "dfbb185c08f713f5d86c77e082be0b3da1736794"))
 (polymode :source "elpaca-menu-lock-file" :recipe
	   (:package "polymode" :fetcher github :repo "polymode/polymode" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id polymode :type git :protocol https
		     :inherit t :depth treeless :ref
		     "4604f55cc020c75562526fb76b723e5e242c97c0"))
 (popper :source "elpaca-menu-lock-file" :recipe
	 (:package "popper" :fetcher github :repo "karthink/popper" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id popper :type git :protocol https :inherit
		   t :depth treeless :ref
		   "d83b894ee7a9daf7c8e9b864c23d08f1b23d78f6"))
 (posframe :source "elpaca-menu-lock-file" :recipe
	   (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id posframe :type git :protocol https
		     :inherit t :depth treeless :ref
		     "3a80911b2f45ce6926196930bb7d5cc662c7b3c8"))
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
			   :source "MELPA" :id powerthesaurus :type git
			   :protocol https :inherit t :depth treeless :ref
			   "4b97797cf789aaba411c61a85fe23474ebc5bedc"))
 (pr-review :source "elpaca-menu-lock-file" :recipe
	    (:package "pr-review" :fetcher github :repo
		      "blahgeek/emacs-pr-review" :files (:defaults "graphql")
		      :source "MELPA" :id pr-review :type git :protocol https
		      :inherit t :depth treeless :ref
		      "d893429168b87003a99bf567932dce57fdac93fa"))
 (profiler-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "profiler-extras" :id profiler-extras
			   :host github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/profiler-extras.el"
			    "emacs/extras/doc/profiler-extras.texi")
			   :depth nil :type git :protocol https :inherit t :ref
			   "8177d5794e74526809e462866d6b0a7aeae955da"))
 (prot-common :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "prot-common" :id prot-common :host github
		       :repo "protesilaos/dotfiles" :local-repo "prot-common"
		       :main "emacs/.emacs.d/prot-lisp/prot-common.el" :build
		       (:not elpaca-check-version) :files
		       ("emacs/.emacs.d/prot-lisp/prot-common.el") :type git
		       :protocol https :inherit t :depth treeless :ref
		       "72461ab316dd4bb7e861037566817901c0fd9938"))
 (prot-eww :source "elpaca-menu-lock-file" :recipe
	   (:source nil :package "prot-eww" :id prot-eww :host github :repo
		    "protesilaos/dotfiles" :local-repo "prot-eww" :main
		    "emacs/.emacs.d/prot-lisp/prot-eww.el" :build
		    (:not elpaca-check-version) :files
		    ("emacs/.emacs.d/prot-lisp/prot-eww.el") :type git :protocol
		    https :inherit t :depth treeless :ref
		    "72461ab316dd4bb7e861037566817901c0fd9938"))
 (prot-scratch :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "prot-scratch" :id prot-scratch :host
			github :repo "protesilaos/dotfiles" :local-repo
			"prot-scratch" :main
			"emacs/.emacs.d/prot-lisp/prot-scratch.el" :build
			(:not elpaca-check-version) :files
			("emacs/.emacs.d/prot-lisp/prot-scratch.el") :type git
			:protocol https :inherit t :depth treeless :ref
			"72461ab316dd4bb7e861037566817901c0fd9938"))
 (prot-simple :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "prot-simple" :id prot-simple :host github
		       :repo "protesilaos/dotfiles" :local-repo "prot-simple"
		       :main "emacs/.emacs.d/prot-lisp/prot-simple.el" :build
		       (:not elpaca-check-version) :files
		       ("emacs/.emacs.d/prot-lisp/prot-simple.el") :type git
		       :protocol https :inherit t :depth treeless :ref
		       "72461ab316dd4bb7e861037566817901c0fd9938"))
 (puni :source "elpaca-menu-lock-file" :recipe
       (:package "puni" :repo "AmaiKinono/puni" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id puni :type git :protocol https :inherit t
		 :depth treeless :ref "fe132f803868f325cf6f162139e327b76df9e4c1"))
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
		       :source "MELPA" :id pyenv-mode :type git :protocol https
		       :inherit t :depth treeless :ref
		       "8e5128ff7f722a4d68ddaa22022cb99ef9ddcf9a"))
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
		     :source "MELPA" :id pythonic :type git :protocol https
		     :inherit t :depth treeless :ref
		     "bf364a29e2f21828941ee3d11a27127bc260740f"))
 (pyvenv :source "elpaca-menu-lock-file" :recipe
	 (:package "pyvenv" :fetcher github :repo "jorgenschaefer/pyvenv" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id pyvenv :type git :protocol https :inherit
		   t :depth treeless :ref
		   "31ea715f2164dd611e7fc77b26390ef3ca93509b"))
 (queue :source "elpaca-menu-lock-file" :recipe
	(:package "queue" :repo
		  ("https://github.com/emacsmirror/gnu_elpa" . "queue") :tar
		  "0.2" :host gnu :branch "externals/queue" :files
		  ("*" (:exclude ".git")) :source "GNU ELPA" :id queue :type git
		  :protocol https :inherit t :depth treeless :ref
		  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (ragmacs :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "ragmacs" :id ragmacs :host github :repo
		   "positron-solutions/ragmacs" :build
		   (:not elpaca-check-version) :type git :protocol https
		   :inherit t :depth treeless :ref
		   "d3ad46ded557a651faa959f1545ca4df48da78f0"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "rainbow-mode" :repo
			 ("https://github.com/emacsmirror/gnu_elpa"
			  . "rainbow-mode")
			 :tar "1.0.6" :host gnu :branch "externals/rainbow-mode"
			 :files ("*" (:exclude ".git")) :source "GNU ELPA" :id
			 rainbow-mode :type git :protocol https :inherit t
			 :depth treeless :ref
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
		       :source "MELPA" :id read-aloud :type git :protocol https
		       :inherit t :depth treeless :ref
		       "c662366226abfb07204ab442b4f853ed85438d8a"))
 (read-aloud-extras :source "elpaca-menu-lock-file" :recipe
		    (:source nil :package "read-aloud-extras" :id
			     read-aloud-extras :host github :repo
			     "benthamite/dotfiles" :files
			     ("emacs/extras/read-aloud-extras.el"
			      "emacs/extras/doc/read-aloud-extras.texi")
			     :depth nil :type git :protocol https :inherit t
			     :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (register-extras :source "elpaca-menu-lock-file" :recipe
		  (:source nil :package "register-extras" :id register-extras
			   :host github :repo "benthamite/dotfiles" :files
			   ("emacs/extras/register-extras.el"
			    "emacs/extras/doc/register-extras.texi")
			   :depth nil :type git :protocol https :inherit t :ref
			   "8177d5794e74526809e462866d6b0a7aeae955da"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
	     ("request.el") :source "MELPA" :id request :type git :protocol
	     https :inherit t :depth treeless :ref
	     "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (request-deferred :source "elpaca-menu-lock-file" :recipe
		   (:package "request-deferred" :repo "tkf/emacs-request"
			     :fetcher github :files ("request-deferred.el")
			     :source "MELPA" :id request-deferred :type git
			     :protocol https :inherit t :depth treeless :ref
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
				 :source "MELPA" :id reveal-in-osx-finder :type
				 git :protocol https :inherit t :depth treeless
				 :ref "5710e5936e47139a610ec9a06899f72e77ddc7bc"))
 (reverso :source "elpaca-menu-lock-file" :recipe
	  (:package "reverso" :repo "SqrtMinusOne/reverso.el" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id reverso :host github :type git :protocol
		    https :inherit t :depth treeless :ref
		    "40ed3d83c4f04c39e05d69d84595761ae2956a64"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	      :source "MELPA" :id s :type git :protocol https :inherit t :depth
	      treeless :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (scroll-other-window :source "elpaca-menu-lock-file" :recipe
		      (:source nil :package "scroll-other-window" :id
			       scroll-other-window :host github :repo
			       "benthamite/scroll-other-window" :type git
			       :protocol https :inherit t :depth treeless :ref
			       "98775d18d6e82d0c7b0ce85986165afa2537eb27"))
 (seq :source "elpaca-menu-lock-file" :recipe
      (:package "seq" :repo ("https://github.com/emacsmirror/gnu_elpa" . "seq")
		:tar "2.24" :host gnu :branch "externals/seq" :files
		("*" (:exclude ".git")) :source "GNU ELPA" :id seq :build
		(:before elpaca-activate elpaca-unload-seq) :type git :protocol
		https :inherit t :depth treeless :ref
		"27a90793a13f149121180e864fa53d68b9eac0b3"))
 (session :source "elpaca-menu-lock-file" :recipe
	  (:package "session" :fetcher github :repo "emacsattic/session" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id session :type git :protocol https
		    :inherit t :depth treeless :ref
		    "3be207c50dfe964de3cbf5cd8fa9b07fc7d2e609"))
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
			:source "MELPA" :id shell-maker :type git :protocol
			https :inherit t :depth treeless :ref
			"afd5509b99b28589bab3a0d06786118905eadba9"))
 (shr-heading :source "elpaca-menu-lock-file" :recipe
	      (:source nil :package "shr-heading" :id shr-heading :host github
		       :repo "oantolin/emacs-config" :files
		       ("my-lisp/shr-heading.el") :type git :protocol https
		       :inherit t :depth treeless :ref
		       "e33c83d9a46880698d3212fef28cd95661d8d9ca"))
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
				  :source "MELPA" :id shr-tag-pre-highlight
				  :type git :protocol https :inherit t :depth
				  treeless :ref
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
			:source "MELPA" :id shrink-path :type git :protocol
			https :inherit t :depth treeless :ref
			"c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (shut-up
   :source "elpaca-menu-lock-file" :recipe
   (:package "shut-up" :fetcher github :repo "cask/shut-up" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			"*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :source "MELPA" :id shut-up :type git :protocol https :inherit t
	     :depth treeless :ref "ed62a7fefdf04c81346061016f1bc69ca045aaf6"))
 (simple-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "simple-extras" :id simple-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/simple-extras.el"
			  "emacs/extras/doc/simple-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
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
			 :source "MELPA" :id simple-httpd :type git :protocol
			 https :inherit t :depth treeless :ref
			 "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (slack :source "elpaca-menu-lock-file" :recipe
	(:package "slack" :fetcher github :repo "emacs-slack/emacs-slack" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id slack :type git :protocol https :inherit t
		  :depth treeless :ref
		  "a39bf51e52ce3843c7330c2cd00ef189ba6b5fa7"))
 (slack-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "slack-extras" :id slack-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/slack-extras.el"
			 "emacs/extras/doc/slack-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (smartrep :source "elpaca-menu-lock-file" :recipe
	   (:package "smartrep" :repo "myuhe/smartrep.el" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		      "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE" "README*"
				"*-pkg.el"))
		     :source "MELPA" :id smartrep :type git :protocol https
		     :inherit t :depth treeless :ref
		     "fdf135e3781b286174b5de4d613f12c318d2023c"))
 (spacious-padding :source "elpaca-menu-lock-file" :recipe
		   (:package "spacious-padding" :repo
			     ("https://github.com/protesilaos/spacious-padding"
			      . "spacious-padding")
			     :tar "0.8.0" :host gnu :files
			     ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
			     :source "GNU ELPA" :id spacious-padding :tag
			     "0.3.0" :type git :protocol https :inherit t :depth
			     treeless :ref
			     "9d96d301d5bccf192daaf00dba64bca9979dcb5a"))
 (stafforini :source "elpaca-menu-lock-file" :recipe
	     (:source nil :package "stafforini" :id stafforini :host github
		      :repo "benthamite/stafforini.el" :type git :protocol https
		      :inherit t :depth treeless :ref
		      "05781cff0b8ea89563ccf14b0bb690e346eb2740"))
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
			      :source "MELPA" :id string-inflection :type git
			      :protocol https :inherit t :depth treeless :ref
			      "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"))
 (subed :source "elpaca-menu-lock-file" :recipe
	(:package "subed" :repo "sachac/subed" :tar "1.4.1" :host github :files
		  ("subed/*.el") :source "NonGNU ELPA" :id subed :type git
		  :protocol https :inherit t :depth treeless :ref
		  "67a4c1e5feb84f3d546dcf19a55b30584544abaf"))
 (substitute :source "elpaca-menu-lock-file" :recipe
	     (:package "substitute" :repo "protesilaos/substitute" :tar "0.5.0"
		       :host github :files
		       ("*" (:exclude ".git" "COPYING" "doclicense.texi"))
		       :source "GNU ELPA" :id substitute :type git :protocol
		       https :inherit t :depth treeless :ref
		       "63acc983d2b8704e5067a5e1b50b486893a8d9e5"))
 (tab-bar-extras :source "elpaca-menu-lock-file" :recipe
		 (:source nil :package "tab-bar-extras" :id tab-bar-extras :host
			  github :repo "benthamite/dotfiles" :files
			  ("emacs/extras/tab-bar-extras.el"
			   "emacs/extras/doc/tab-bar-extras.texi")
			  :depth nil :type git :protocol https :inherit t :ref
			  "8177d5794e74526809e462866d6b0a7aeae955da"))
 (tablist :source "elpaca-menu-lock-file" :recipe
	  (:package "tablist" :fetcher github :repo "emacsorphanage/tablist"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		     "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :source "MELPA" :id tablist :type git :protocol https
		    :inherit t :depth treeless :ref
		    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (tangodb :source "elpaca-menu-lock-file" :recipe
	  (:source nil :package "tangodb" :id tangodb :host github :repo
		   "benthamite/tangodb.el" :type git :protocol https :inherit t
		   :depth treeless :ref
		   "0803af1da82b02fbcda73c65831d34df2a6b9018"))
 (telega :source "elpaca-menu-lock-file" :recipe
	 (:package "telega" :fetcher github :repo "zevlg/telega.el" :files
		   (:defaults "etc" "server" "contrib" "Makefile") :source
		   "MELPA" :id telega :type git :protocol https :inherit t
		   :depth treeless :ref
		   "d5a52a1a9f76cc4a4c601b48544d28afa8f55a80"))
 (telega-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "telega-extras" :id telega-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/telega-extras.el"
			  "emacs/extras/doc/telega-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
 (tmr :source "elpaca-menu-lock-file" :recipe
      (:package "tmr" :repo ("https://github.com/protesilaos/tmr" . "tmr") :tar
		"1.3.0" :host gnu :files
		("*" (:exclude ".git" "COPYING" "doclicense.texi" "Makefile"))
		:source "GNU ELPA" :id tmr :type git :protocol https :inherit t
		:depth treeless :ref "7f074024de46f4d471943057403f900a6321e299"))
 (tomelr :source "elpaca-menu-lock-file" :recipe
	 (:package "tomelr" :repo
		   ("https://github.com/kaushalmodi/tomelr" . "tomelr") :tar
		   "0.4.3" :host gnu :files ("*" (:exclude ".git" "LICENSE"))
		   :source "GNU ELPA" :id tomelr :type git :protocol https
		   :inherit t :depth treeless :ref
		   "670e0a08f625175fd80137cf69e799619bf8a381"))
 (track-changes :source "elpaca-menu-lock-file" :recipe
		(:package "track-changes" :repo
			  "https://github.com/emacs-straight/track-changes.git"
			  :tar "1.5" :host gnu :branch "master" :files
			  ("*" (:exclude ".git")) :source "GNU ELPA" :id
			  track-changes :type git :protocol https :inherit t
			  :depth treeless :ref
			  "6d8fb08f6ef72e0b9bd8bea61d91d47a8b00ec81"))
 (transient :source "elpaca-menu-lock-file" :recipe
	    (:package "transient" :fetcher github :repo "magit/transient" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		       "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE" "README*"
				 "*-pkg.el"))
		      :source "MELPA" :id transient :host github :branch "main"
		      :build (:not elpaca-check-version) :type git :protocol
		      https :inherit t :depth treeless :ref
		      "7131bec61e558e022ce75e2d2d5e55c748fcf8e0"))
 (treepy :source "elpaca-menu-lock-file" :recipe
	 (:package "treepy" :repo "volrath/treepy.el" :fetcher github :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id treepy :type git :protocol https :inherit
		   t :depth treeless :ref
		   "651e2634f01f346da9ec8a64613c51f54b444bc3"))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		"doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
		"docs/*.info" "docs/*.texi" "docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :source "MELPA" :id ts :type git :protocol https :inherit t
	       :depth treeless :ref "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (use-package-extras :source "elpaca-menu-lock-file" :recipe
		     (:source nil :package "use-package-extras" :id
			      use-package-extras :host github :repo
			      "benthamite/dotfiles" :files
			      ("emacs/extras/use-package-extras.el"
			       "emacs/extras/doc/use-package-extras.texi")
			      :depth nil :type git :protocol https :inherit t
			      :ref "8177d5794e74526809e462866d6b0a7aeae955da"))
 (vc-extras :source "elpaca-menu-lock-file" :recipe
	    (:source nil :package "vc-extras" :id vc-extras :host github :repo
		     "benthamite/dotfiles" :files
		     ("emacs/extras/vc-extras.el"
		      "emacs/extras/doc/vc-extras.texi")
		     :depth nil :type git :protocol https :inherit t :ref
		     "8177d5794e74526809e462866d6b0a7aeae955da"))
 (vertico :source "elpaca-menu-lock-file" :recipe
	  (:package "vertico" :repo "minad/vertico" :files
		    (:defaults "extensions/*") :fetcher github :source "MELPA"
		    :id vertico :includes
		    (vertico-indexed vertico-flat vertico-grid vertico-mouse
				     vertico-quick vertico-buffer vertico-repeat
				     vertico-reverse vertico-directory
				     vertico-multiform vertico-unobtrusive)
		    :type git :protocol https :inherit t :depth treeless :ref
		    "0b96e8f169653cba6530da1ab0a1c28ffa44b180"))
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
			       :source "MELPA" :id visual-fill-column :type git
			       :protocol https :inherit t :depth treeless :ref
			       "e1be9a1545157d24454d950c0ac79553c540edb7"))
 (vterm :source "elpaca-menu-lock-file" :recipe
	(:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
		  ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc"
		   "utf8.c" "utf8.h" "vterm.el" "vterm-module.c"
		   "vterm-module.h")
		  :source "MELPA" :id vterm :type git :protocol https :inherit t
		  :depth treeless :ref
		  "a01a2894a1c1e81a39527835a9169e35b7ec5dec"))
 (vterm-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "vterm-extras" :id vterm-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/vterm-extras.el"
			 "emacs/extras/doc/vterm-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da"))
 (vulpea :source "elpaca-menu-lock-file" :recipe
	 (:package "vulpea" :fetcher github :repo "d12frosted/vulpea" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		    "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :source "MELPA" :id vulpea :type git :protocol https :inherit
		   t :depth treeless :ref
		   "050345486f689cd25d0136c2c282c3ee3d5b7956"))
 (vulpea-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "vulpea-extras" :id vulpea-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/vulpea-extras.el"
			  "emacs/extras/doc/vulpea-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
 (vundo :source "elpaca-menu-lock-file" :recipe
	(:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo")
		  :tar "2.4.0" :host gnu :files ("*" (:exclude ".git" "test"))
		  :source "GNU ELPA" :id vundo :type git :protocol https
		  :inherit t :depth treeless :ref
		  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (w3m :source "elpaca-menu-lock-file" :recipe
      (:package "w3m" :fetcher github :repo "emacs-w3m/emacs-w3m" :files
		(:defaults "icons"
			   (:exclude "octet.el" "mew-w3m.el" "w3m-xmas.el"
				     "doc/*.texi"))
		:source "MELPA" :id w3m :type git :protocol https :inherit t
		:depth treeless :ref "ec18c21418bf7c1be159bd3cf7e79a370d4be1f3"))
 (wasabi :source "elpaca-menu-lock-file" :recipe
	 (:source nil :package "wasabi" :id wasabi :host github :repo
		  "xenodium/wasabi" :type git :protocol https :inherit t :depth
		  treeless :ref "93a74069d3e1408d251fa22146350e263a33bb7b"))
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
		      :source "MELPA" :id websocket :type git :protocol https
		      :inherit t :depth treeless :ref
		      "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
	(:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep"
		  :files ("wgrep.el") :source "MELPA" :id wgrep :type git
		  :protocol https :inherit t :depth treeless :ref
		  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (wikipedia :source "elpaca-menu-lock-file" :recipe
	    (:source "Init file" :package "wikipedia" :id wikipedia :host github
		     :repo "benthamite/wikipedia" :depth nil :type git :protocol
		     https :inherit t :ref
		     "88f943d2a40adaca559c869ef48eca3ed65135b5"))
 (window-extras :source "elpaca-menu-lock-file" :recipe
		(:source nil :package "window-extras" :id window-extras :host
			 github :repo "benthamite/dotfiles" :files
			 ("emacs/extras/window-extras.el"
			  "emacs/extras/doc/window-extras.texi")
			 :depth nil :type git :protocol https :inherit t :ref
			 "8177d5794e74526809e462866d6b0a7aeae955da"))
 (winum :source "elpaca-menu-lock-file" :recipe
	(:package "winum" :fetcher github :repo "deb0ch/emacs-winum" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id winum :type git :protocol https :inherit t
		  :depth treeless :ref
		  "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
	      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
	      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			"*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :source "MELPA" :id with-editor :type git :protocol https :inherit
	     t :depth treeless :ref "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
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
			   :source "MELPA" :id writeroom-mode :type git
			   :protocol https :inherit t :depth treeless :ref
			   "cca2b4b3cfcfea1919e1870519d79ed1a69aa5e2"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id yaml :host github :type git :protocol https
		 :inherit t :depth treeless :ref
		 "f2369fb4985ed054be47ae111760ff2075dff72a"))
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
		      :source "MELPA" :id yaml-mode :type git :protocol https
		      :inherit t :depth treeless :ref
		      "d91f878729312a6beed77e6637c60497c5786efa"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
	    (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github
		      :files ("yasnippet.el" "snippets") :source "MELPA" :id
		      yasnippet :type git :protocol https :inherit t :depth
		      treeless :ref "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
		     (:package "yasnippet-snippets" :repo
			       "AndreaCrotti/yasnippet-snippets" :fetcher github
			       :files ("*.el" "snippets" ".nosearch") :source
			       "MELPA" :id yasnippet-snippets :type git
			       :protocol https :inherit t :depth treeless :ref
			       "606ee926df6839243098de6d71332a697518cb86"))
 (ytdl :source "elpaca-menu-lock-file" :recipe
       (:package "ytdl" :repo "tuedachu/ytdl" :fetcher gitlab :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
		  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :source "MELPA" :id ytdl :type git :protocol https :inherit t
		 :depth treeless :ref "309ad5ce95368ad2e35d1c1701a1f3c0043415a3"))
 (zotra :source "elpaca-menu-lock-file" :recipe
	(:package "zotra" :fetcher github :repo "mpedramfar/zotra" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		   "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
			     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :source "MELPA" :id zotra :host github :type git :protocol
		  https :inherit t :depth treeless :ref
		  "fe9093b226a1678fc6c2fadd31a09d5a22ecdcf1"))
 (zotra-extras :source "elpaca-menu-lock-file" :recipe
	       (:source nil :package "zotra-extras" :id zotra-extras :host
			github :repo "benthamite/dotfiles" :files
			("emacs/extras/zotra-extras.el"
			 "emacs/extras/doc/zotra-extras.texi")
			:depth nil :type git :protocol https :inherit t :ref
			"8177d5794e74526809e462866d6b0a7aeae955da")))
