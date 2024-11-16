;;; paths.el --- Set variable paths -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/paths.el
;; Version: 0.1

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

;; Set variable paths. Values set via these variables will be available at startup for each user.

;;; Code:

;;;; User options

(defgroup paths ()
  "Set personal paths."
  :group 'emacs)

;;;;; Directories

(defcustom paths-dir-downloads
  (file-name-concat (expand-file-name "~") "Downloads/")
  "Path to the `downloads' directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-emacs-profiles
  (file-name-concat (expand-file-name "~") ".config/emacs-profiles/")
  "Path to the Emacs profiles directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-root
  "/"
  "Path to the root directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-system-apps
  "/Applications/"
  "Path to the system applications directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox
  (file-name-concat (expand-file-name "~") "Library/CloudStorage/Dropbox/")
  "Path to the Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-emacs
  user-emacs-directory
  "Path to the Emacs directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive
  (file-name-concat (expand-file-name "~") "Google Drive/")
  "Path to the Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-pdf-library
  (file-name-concat paths-dir-google-drive "library-pdf/")
  "Path to the PDF library."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-html-library
  (file-name-concat paths-dir-google-drive "library-html/")
  "Path to the HTML library."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-media-library
  (file-name-concat paths-dir-google-drive "library-media/")
  "Path to the media library."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-music
  (file-name-concat paths-dir-google-drive "music/")
  "Path to the music directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-movies
  (file-name-concat (expand-file-name "~") "movies/")
  "Path to the movies directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-finance
  (file-name-concat paths-dir-google-drive "finance/")
  "Path to the finance directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-audiobooks
  (file-name-concat paths-dir-google-drive "audiobooks/")
  "Path to the audiobooks directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-music-tango
  (file-name-concat paths-dir-music "tango/")
  "Path to the tango music directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-music-popular
  (file-name-concat paths-dir-music "popular/")
  "Path to the popular music directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-music-classical
  (file-name-concat paths-dir-music "classical/")
  "Path to the classical music directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-music-to-sort
  (file-name-concat paths-dir-music "to sort/")
  "Path to the music to sort directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-anki
  (file-name-concat paths-dir-dropbox "anki/")
  "Path to the Anki directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-archive
  (file-name-concat paths-dir-dropbox "archive/")
  "Path to the archive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-inactive
  (file-name-concat paths-dir-dropbox "inactive/")
  "Path to the inactive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-blog
  (file-name-concat paths-dir-dropbox "blog/")
  "Path to the blog directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-journal
  (file-name-concat paths-dir-dropbox "journal/")
  "Path to the journal directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-wiki
  (file-name-concat paths-dir-dropbox "wiki/")
  "Path to the wiki directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-wiki-entries
  (file-name-concat paths-dir-wiki "entries/")
  "Path to the wiki entries directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-wiki-references
  (file-name-concat paths-dir-wiki "references/")
  "Path to the wiki references directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dotfiles
  (file-name-concat paths-dir-dropbox "dotfiles/")
  "Path to the dotfiles directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-karabiner
  (file-name-concat paths-dir-dotfiles "karabiner/")
  "Path to the Karabiner dotfiles directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-bibliographic-notes
  (file-name-concat paths-dir-dropbox "bibliographic-notes/")
  "Path to the bibliographic notes directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dotemacs
  (let ((dir (pcase (getenv "HOME")
	       ("/Users/pablostafforini" "Library/CloudStorage/Dropbox/dotfiles/emacs/")
	       ("/Users/fede" "source/dotfiles/emacs/")
	       ("/Users/cartago" "source/dotfiles/emacs/")
	       (_ (user-error "Home directory does not match that of a known user")))))
    (file-name-concat (getenv "HOME") dir))
  "Path to the \"emacs\" directory in Pablo’s \"dotfiles\" repo."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-yasnippets
  (file-name-concat paths-dir-dotemacs "yasnippets/")
  "Path to the Yasnippets directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-yasnippets-private
  (file-name-concat paths-dir-dotemacs "yasnippets-private/")
  "Path to the private Yasnippets directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-abbrev
  (file-name-concat paths-dir-dotemacs "abbrev/")
  "Path to the abbrev directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-private
  (file-name-concat paths-dir-dotfiles "private/")
  "Path to the private dotfiles directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-ledger
  (file-name-concat paths-dir-dropbox "ledger/")
  "Path to the ledger directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-notes
  (file-name-concat paths-dir-dropbox "notes/")
  "Path to the notes directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-people
  (file-name-concat paths-dir-dropbox "people/")
  "Path to the people directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-android
  (file-name-concat paths-dir-dropbox "android/")
  "Path to the Android directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-bbdb
  (file-name-concat paths-dir-dropbox "bbdb/")
  "Path to the BBDB directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-emacs-var
  (file-name-concat paths-dir-emacs "var/")
  "Path to the Emacs `var' directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-emacs-local
  (file-name-concat paths-dir-emacs "local/")
  "Path to the Emacs `local' directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-external-repos
  (file-name-concat (expand-file-name "~") "source/")
  "Path to the external repositories directory.
This is where the repositories that are neither personal
repositories (`paths-dir-personal-repos') nor Tlön
repositories(`paths-dir-tlon-repos') will be cloned. If you clone all
repositories in the same directory, all these variables will have the same
value."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-tlon-repos
  (file-name-concat paths-dir-dropbox "repos/")
  "Path to the Tlön repositories directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-personal-repos
  (file-name-concat paths-dir-dropbox "repos/")
  "Path to the personal repositories directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-split-git
  (file-name-concat (expand-file-name "~") "git-dirs/")
  "Path to the \".git\" directory in split repos."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-tlon-docs
  (file-name-concat paths-dir-tlon-repos "tlon-docs/")
  "Path to the `tlon-docs' repo directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-translation-server
  (file-name-concat paths-dir-external-repos "translation-server/")
  "Path to the the `translation-server' directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-PW
  (file-name-concat paths-dir-dropbox "PW/")
  "Path to the PW Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon
  (file-name-concat paths-dir-google-drive "tlon/")
  "Path to the Tlön Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-leo
  (file-name-concat paths-dir-google-drive-tlon "leo/")
  "Path to the Leo Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon
  (file-name-concat paths-dir-google-drive-tlon "babel/")
  "Path to the Babel Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-EAN
  (file-name-concat paths-dir-google-drive-tlon "EAN/")
  "Path to the EAN Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-FM
  (file-name-concat paths-dir-google-drive-tlon "FM/")
  "Path to the FM Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-GPE
  (file-name-concat paths-dir-google-drive-tlon "GPE/")
  "Path to the GPE Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-HEAR
  (file-name-concat paths-dir-google-drive-tlon "HEAR/")
  "Path to the HEAR Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-LBDLH
  (file-name-concat paths-dir-google-drive-tlon "LBDLH/")
  "Path to the LBDLH Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-LP
  (file-name-concat paths-dir-google-drive-tlon "LP/")
  "Path to the LP Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-RAE
  (file-name-concat paths-dir-google-drive-tlon "RAE/")
  "Path to the RAE Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-RCG
  (file-name-concat paths-dir-google-drive-tlon "RCG/")
  "Path to the RCG Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-core
  (file-name-concat paths-dir-google-drive-tlon "core/")
  "Path to the Core Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-google-drive-tlon-fede
  (file-name-concat paths-dir-google-drive-tlon "fede/")
  "Path to the Fede Google Drive directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon
  (file-name-concat paths-dir-dropbox "tlon/")
  "Path to the Tlön Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-core
  (file-name-concat paths-dir-dropbox-tlon "core/")
  "Path to the Core Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-leo
  (file-name-concat paths-dir-dropbox-tlon "leo/")
  "Path to the Leo Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-fede
  (file-name-concat paths-dir-dropbox-tlon "fede/")
  "Path to the Fede Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-pablo
  (file-name-concat paths-dir-dropbox-tlon "pablo/")
  "Path to the Pablo Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-ledger
  (file-name-concat paths-dir-dropbox-tlon-core "ledger/")
  "Path to the Ledger Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-pass
  (file-name-concat paths-dir-dropbox-tlon-core "pass/")
  "Path to the Pass Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon
  (file-name-concat paths-dir-dropbox-tlon "babel/")
  "Path to the Babel Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-EAN
  (file-name-concat paths-dir-dropbox-tlon "EAN/")
  "Path to the EAN Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-FM
  (file-name-concat paths-dir-dropbox-tlon "FM/")
  "Path to the FM Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-GPE
  (file-name-concat paths-dir-dropbox-tlon "GPE/")
  "Path to the GPE Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-HEAR
  (file-name-concat paths-dir-dropbox-tlon "HEAR/")
  "Path to the HEAR Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-LBDLH
  (file-name-concat paths-dir-dropbox-tlon "LBDLH/")
  "Path to the LBDLH Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-LP
  (file-name-concat paths-dir-dropbox-tlon "LP/")
  "Path to the LP Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-RAE
  (file-name-concat paths-dir-dropbox-tlon "RAE/")
  "Path to the RAE Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-dropbox-tlon-RCG
  (file-name-concat paths-dir-dropbox-tlon "RCG/")
  "Path to the RCG Dropbox directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-org
  paths-dir-dropbox
  "Path to the directory containing your org files.
This should be the path you set `org-directory' to."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-org-roam
  paths-dir-org
  "Path to the directory containing your org roam files.
This should be the path you set `org-roam-directory' to."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-org-images
  (file-name-concat paths-dir-google-drive "Pictures/org/")
  "Path to the directory containing the `org-mode' images."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-websites
  (file-name-concat paths-dir-dropbox "websites/")
  "Path to the `websites' directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-personal-bibliography
  (file-name-concat paths-dir-dropbox "bibliography/")
  "Path to the `bibliography' directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-babel-refs
  (file-name-concat paths-dir-tlon-repos "babel-refs/")
  "Path to the `babel-refs' repo directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-all-repos
  (list paths-dir-android
	paths-dir-personal-bibliography
	paths-dir-bibliographic-notes
	paths-dir-journal
	paths-dir-notes
	paths-dir-people)
  "List of all personal repos."
  :type '(repeat directory)
  :group 'paths)

(defcustom paths-dir-personal-csl-styles
  (file-name-concat paths-dir-personal-bibliography "styles/")
  "Path to the CSL styles directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-personal-csl-locales
  (file-name-concat paths-dir-personal-bibliography "locales/")
  "Path to the CSL locales directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-tlon-csl-styles
  (file-name-concat paths-dir-tlon-repos "babel-refs/styles/")
  "Path to the Tlön CSL styles directory."
  :type 'directory
  :group 'paths)

(defcustom paths-dir-tlon-csl-locales
  (file-name-concat paths-dir-tlon-repos "babel-refs/locales/")
  "Path to the Tlön CSL locales directory."
  :type 'directory
  :group 'paths)

;;;;; Files

(defcustom paths-file-notes
  (file-name-concat paths-dir-notes "notes.org")
  "Path to the notes file."
  :type 'file
  :group 'paths)

(defcustom paths-file-inbox-desktop
  (file-name-concat paths-dir-android "inbox-desktop.org")
  "Path to the desktop inbox file."
  :type 'file
  :group 'paths)

(defcustom paths-file-inbox-mobile
  (file-name-concat paths-dir-android "inbox-mobile.org")
  "Path to the mobile inbox file."
  :type 'file
  :group 'paths)

(defcustom paths-file-calendar
  (file-name-concat paths-dir-android "calendar.org")
  "Path to the calendar file."
  :type 'file
  :group 'paths)

(defcustom paths-file-feeds-pablo
  (file-name-concat paths-dir-notes "feeds.org")
  "Path to Pablo's feeds file."
  :type 'file
  :group 'paths)

(defcustom paths-file-tlon-feeds
  (file-name-concat paths-dir-dropbox-tlon-core "feeds.org")
  "Path to the Tlön feeds file."
  :type 'file
  :group 'paths)

(defcustom paths-file-anki
  (file-name-concat paths-dir-anki "main.org")
  "Path to the Anki file."
  :type 'file
  :group 'paths)

(defcustom paths-file-config
  (file-name-concat paths-dir-dotemacs "config.org")
  "Path to the `org-mode' config file."
  :type 'file
  :group 'paths)

(defcustom paths-file-karabiner
  (file-name-concat paths-dir-karabiner "modifications.org")
  "Path to the Karabiner `org-mode' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-karabiner-edn
  (file-name-concat paths-dir-karabiner "karabiner.edn")
  "Path to the Karabiner `edn' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-personal-bibliography-old
  (file-name-concat paths-dir-personal-bibliography "old.bib")
  "Path to the `old.bib' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-personal-bibliography-new
  (file-name-concat paths-dir-personal-bibliography "new.bib")
  "Path to the `new.bib' file."
  :type 'file
  :group 'paths)

(defcustom paths-files-bibliography-personal
  `(,paths-file-personal-bibliography-new
    ,paths-file-personal-bibliography-old)
  "List of personal bibliography files."
  :type '(repeat file)
  :group 'paths)

(defcustom paths-files-bibliography-all '()
  "List of all bibliography files.
This includes personal files and `tlon' files."
  :type '(repeat file)
  :group 'paths)

(defcustom paths-file-wiki-notes
  (file-name-concat paths-dir-wiki "wiki-notes.org")
  "Path to the `wiki-notes.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-wiki-published
  (file-name-concat paths-dir-wiki "wiki-published.org")
  "Path to the `wiki-published.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-wiki-help
  (file-name-concat paths-dir-wiki "wiki-help.org")
  "Path to the `wiki-help.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-quotes
  (file-name-concat paths-dir-blog "quotes.org")
  "Path to the `quotes.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-films
  (file-name-concat paths-dir-notes "films.org")
  "Path to the `films.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-tlon-tareas-leo
  (file-name-concat paths-dir-dropbox-tlon-leo "tareas.org")
  "Path to the Leo's `tareas.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-tlon-tareas-fede
  (file-name-concat paths-dir-dropbox-tlon-fede "tareas.org")
  "Path to the Fede's `tareas.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-org2blog
  (file-name-concat paths-dir-websites ".org2blog.org")
  "Path to the `.org2blog.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-orb-noter-template
  (file-name-concat paths-dir-personal-bibliography "orb-noter-template.org")
  "Path to the `orb' noter template."
  :type 'file
  :group 'paths)

(defcustom paths-file-orb-capture-template
  (file-name-concat paths-dir-bibliographic-notes "${citekey}.org")
  "Path to the `orb' capture template."
  :type 'file
  :group 'paths)

(defcustom paths-file-bookmarks
  (file-name-concat paths-dir-dropbox "bookmarks")
  "Path to the bookmarks file."
  :type 'file
  :group 'paths)

(defcustom paths-file-ledger
  (file-name-concat paths-dir-ledger "journal.ledger")
  "Path to the `ledger' journal file."
  :type 'file
  :group 'paths)

(defcustom paths-file-ledger-db
  (file-name-concat paths-dir-ledger ".pricedb")
  "Path to the `ledger' `.pricedb' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-metaculus
  (file-name-concat paths-dir-notes "metaculus.org")
  "Path to the `metaculus.org' file."
  :type 'file
  :group 'paths)

(defcustom paths-file-fm
  (file-name-concat paths-dir-notes "future-matters.org")
  "Path to the Future Matters project file."
  :type 'file
  :group 'paths)

(defcustom paths-file-ean
  (file-name-concat paths-dir-notes "ea.news.org")
  "Path to the EA News file."
  :type 'file
  :group 'paths)

(defcustom paths-file-work
  (file-name-concat paths-dir-notes "work-dashboard.org")
  "Path to the work dashboard file."
  :type 'file
  :group 'paths)

(defcustom paths-file-tlon-ledger-schedule-file
  (file-name-concat paths-dir-dropbox-tlon-ledger "ledger-schedule.ledger")
  "Path to the Tlön `ledger' scheduler file."
  :type 'file
  :group 'paths)

(defcustom paths-file-tlon
  (file-name-concat paths-dir-notes "babel.org")
  "Path to the Babel project file."
  :type 'file
  :group 'paths)

(defcustom paths-file-tlon-ledger
  (file-name-concat paths-dir-dropbox-tlon-ledger "tlon.ledger")
  "Path to the Tlön `ledger' journal file."
  :type 'file
  :group 'paths)

(defcustom paths-dir-init-default
  (file-name-concat paths-dir-emacs-profiles "develop/")
  "Default target location for the init files to be tangled."
  :type 'directory
  :group 'paths)

(defcustom paths-file-init-tangle-flags-sans-directory
  "tangle-flags-pablo.el"
  "Name of the file containing the tangle flags."
  :type 'file
  :group 'paths)

;;;;; Other

(defcustom paths-tlon-todos-generic-id
  "4388B4D0-3830-48E0-A118-C3195B62F0D1"
  "ID of the user-specific `org-mode' heading where generic TODOs are stored.
\"Generic\" TODOs are all TODOs except those related to a translation job."
  :type 'string
  :group 'paths)

(defcustom paths-tlon-todos-jobs-id
  "F99006B0-3AFC-47A0-98C5-89FB86ADCDFB"
  "ID of the user-specific `org-mode' heading where job TODOs are stored.
A job TODO is a TODO for a translation job."
  :type 'string
  :group 'paths)

(provide 'paths)

;;; paths.el ends here
