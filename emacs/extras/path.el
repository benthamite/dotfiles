;;; path.el --- Set personal paths -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/path.el
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

;; Set personal paths.

;;; Code:

;;;; User options

(defgroup path ()
  "Set personal paths."
  :group 'emacs)

;;;;; Directories

(defcustom path-dir-user
  (file-name-as-directory (expand-file-name (getenv "HOME")))
  "Home user directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-downloads
  (file-name-concat path-dir-user "Downloads/")
  "Path to the `downloads' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-chemacs-profiles
  (file-name-concat path-dir-user ".config/emacs-profiles/")
  "Path to the Chemacs profiles directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-root
  "/"
  "Path to the root directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-system-apps
  "/Applications/"
  "Path to the system applications directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox
  (file-name-concat path-dir-user "Library/CloudStorage/Dropbox/")
  "Path to the Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-emacs
  user-emacs-directory
  "Path to the Emacs directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive
  (file-name-concat path-dir-user "Google Drive/")
  "Path to the Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-pdf-library
  (file-name-concat path-dir-google-drive "library-pdf/")
  "Path to the PDF library."
  :type 'directory
  :group 'path)

(defcustom path-dir-html-library
  (file-name-concat path-dir-google-drive "library-html/")
  "Path to the HTML library."
  :type 'directory
  :group 'path)

(defcustom path-dir-media-library
  (file-name-concat path-dir-google-drive "library-media/")
  "Path to the media library."
  :type 'directory
  :group 'path)

(defcustom path-dir-music
  (file-name-concat path-dir-google-drive "music/")
  "Path to the music directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-movies
  (file-name-concat path-dir-user "movies/")
  "Path to the movies directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-finance
  (file-name-concat path-dir-google-drive "finance/")
  "Path to the finance directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-audiobooks
  (file-name-concat path-dir-google-drive "audiobooks/")
  "Path to the audiobooks directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-music-tango
  (file-name-concat path-dir-music "tango/")
  "Path to the tango music directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-music-popular
  (file-name-concat path-dir-music "popular/")
  "Path to the popular music directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-music-classical
  (file-name-concat path-dir-music "classical/")
  "Path to the classical music directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-music-to-sort
  (file-name-concat path-dir-music "to sort/")
  "Path to the music to sort directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-anki
  (file-name-concat path-dir-dropbox "anki/")
  "Path to the Anki directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-archive
  (file-name-concat path-dir-dropbox "archive/")
  "Path to the archive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-inactive
  (file-name-concat path-dir-dropbox "inactive/")
  "Path to the inactive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-blog
  (file-name-concat path-dir-dropbox "blog/")
  "Path to the blog directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-journal
  (file-name-concat path-dir-dropbox "journal/")
  "Path to the journal directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-wiki
  (file-name-concat path-dir-dropbox "wiki/")
  "Path to the wiki directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-wiki-entries
  (file-name-concat path-dir-wiki "entries/")
  "Path to the wiki entries directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-wiki-references
  (file-name-concat path-dir-wiki "references/")
  "Path to the wiki references directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dotfiles
  (file-name-concat path-dir-dropbox "dotfiles/")
  "Path to the dotfiles directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dotemacs
  (file-name-concat path-dir-dotfiles "emacs/")
  "Path to the Emacs dotfiles directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-extras
  (file-name-concat path-dir-dotemacs "extras/")
  "Path to the `extras' package directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-karabiner
  (file-name-concat path-dir-dotfiles "karabiner/")
  "Path to the Karabiner dotfiles directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-bibliographic-notes
  (file-name-concat path-dir-dropbox "bibliographic-notes/")
  "Path to the bibliographic notes directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-yasnippets
  (file-name-concat path-dir-dotemacs "yasnippets/")
  "Path to the Yasnippets directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-yasnippets-private
  (file-name-concat path-dir-dotemacs "yasnippets-private/")
  "Path to the private Yasnippets directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-abbrev
  (file-name-concat path-dir-dotemacs "abbrev/")
  "Path to the abbrev directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-private
  (file-name-concat path-dir-dotfiles "private/")
  "Path to the private dotfiles directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-ledger
  (file-name-concat path-dir-dropbox "ledger/")
  "Path to the ledger directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-notes
  (file-name-concat path-dir-dropbox "notes/")
  "Path to the notes directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-people
  (file-name-concat path-dir-dropbox "people/")
  "Path to the people directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-android
  (file-name-concat path-dir-dropbox "android/")
  "Path to the Android directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-bbdb
  (file-name-concat path-dir-dropbox "bbdb/")
  "Path to the BBDB directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-emacs-var
  (file-name-concat path-dir-emacs "var/")
  "Path to the Emacs `var' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-emacs-local
  (file-name-concat path-dir-emacs "local/")
  "Path to the Emacs `local' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-source
  (file-name-concat path-dir-user "source/")
  "Path to the source repos directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-translation-server
  (file-name-concat path-dir-source "translation-server/")
  "Path to the the `translation-server' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-PW
  (file-name-concat path-dir-dropbox "PW/")
  "Path to the PW Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon
  (file-name-concat path-dir-google-drive "tlon/")
  "Path to the Tlön Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-leo
  (file-name-concat path-dir-google-drive-tlon "leo/")
  "Path to the Leo Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-BAE
  (file-name-concat path-dir-google-drive-tlon "BAE/")
  "Path to the BAE Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-EAN
  (file-name-concat path-dir-google-drive-tlon "EAN/")
  "Path to the EAN Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-FM
  (file-name-concat path-dir-google-drive-tlon "FM/")
  "Path to the FM Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-GPE
  (file-name-concat path-dir-google-drive-tlon "GPE/")
  "Path to the GPE Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-HEAR
  (file-name-concat path-dir-google-drive-tlon "HEAR/")
  "Path to the HEAR Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-LBDLH
  (file-name-concat path-dir-google-drive-tlon "LBDLH/")
  "Path to the LBDLH Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-LP
  (file-name-concat path-dir-google-drive-tlon "LP/")
  "Path to the LP Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-RAE
  (file-name-concat path-dir-google-drive-tlon "RAE/")
  "Path to the RAE Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-RCG
  (file-name-concat path-dir-google-drive-tlon "RCG/")
  "Path to the RCG Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-core
  (file-name-concat path-dir-google-drive-tlon "core/")
  "Path to the Core Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-google-drive-tlon-fede
  (file-name-concat path-dir-google-drive-tlon "fede/")
  "Path to the Fede Google Drive directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon
  (file-name-concat path-dir-dropbox "tlon/")
  "Path to the Tlön Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-core
  (file-name-concat path-dir-dropbox-tlon "core/")
  "Path to the Core Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-leo
  (file-name-concat path-dir-dropbox-tlon "leo/")
  "Path to the Leo Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-fede
  (file-name-concat path-dir-dropbox-tlon "fede/")
  "Path to the Fede Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-pablo
  (file-name-concat path-dir-dropbox-tlon "pablo/")
  "Path to the Pablo Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-ledger
  (file-name-concat path-dir-dropbox-tlon-core "ledger/")
  "Path to the Ledger Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-pass
  (file-name-concat path-dir-dropbox-tlon-core "pass/")
  "Path to the Pass Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-BAE
  (file-name-concat path-dir-dropbox-tlon "BAE/")
  "Path to the BAE Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-EAN
  (file-name-concat path-dir-dropbox-tlon "EAN/")
  "Path to the EAN Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-FM
  (file-name-concat path-dir-dropbox-tlon "FM/")
  "Path to the FM Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-GPE
  (file-name-concat path-dir-dropbox-tlon "GPE/")
  "Path to the GPE Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-HEAR
  (file-name-concat path-dir-dropbox-tlon "HEAR/")
  "Path to the HEAR Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-LBDLH
  (file-name-concat path-dir-dropbox-tlon "LBDLH/")
  "Path to the LBDLH Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-LP
  (file-name-concat path-dir-dropbox-tlon "LP/")
  "Path to the LP Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-RAE
  (file-name-concat path-dir-dropbox-tlon "RAE/")
  "Path to the RAE Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-dropbox-tlon-RCG
  (file-name-concat path-dir-dropbox-tlon "RCG/")
  "Path to the RCG Dropbox directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-repos
  (file-name-concat path-dir-dropbox "repos/")
  "Path to the Dropbox repos directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-tlon-babel
  (file-name-concat path-dir-repos "babel/")
  "Path to the `tlon-babel' repo directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-tlon-docs
  (file-name-concat path-dir-repos "tlon-docs/")
  "Path to the `tlon-docs' repo directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-org
  path-dir-dropbox
  "Path to the directory containing your org files.
This should be the path you set `org-directory' to."
  :type 'directory
  :group 'path)

(defcustom path-dir-org-roam
  path-dir-org
  "Path to the directory containing your org roam files.
This should be the path you set `org-roam-directory' to."
  :type 'directory
  :group 'path)

(defcustom path-dir-org-images
  (file-name-concat path-dir-google-drive "Pictures/org/")
  "Path to the directory containing the `org-mode' images."
  :type 'directory
  :group 'path)

(defcustom path-dir-websites
  (file-name-concat path-dir-dropbox "websites/")
  "Path to the `websites' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-calibre
  (file-name-concat path-dir-user "Calibre Library/")
  "Path to the `Calibre Library' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-adobe-digital-editions
  (file-name-concat path-dir-user "Documents/Digital Editions/")
  "Path to the Adobe Digital Editions directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-personal-bibliography
  (file-name-concat path-dir-dropbox "bibliography/")
  "Path to the `bibliography' directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-all-repos
  (list path-dir-android
	path-dir-personal-bibliography
	path-dir-bibliographic-notes
	path-dir-journal
	path-dir-notes
	path-dir-people)
  "List of all personal repos."
  :type '(repeat directory)
  :group 'path)

(defcustom path-dir-personal-csl-styles
  (file-name-concat path-dir-personal-bibliography "styles/")
  "Path to the CSL styles directory."
  :type 'directory
  :group 'path)

(defcustom path-dir-personal-csl-locales
  (file-name-concat path-dir-personal-bibliography "locales/")
  "Path to the CSL locales directory."
  :type 'directory
  :group 'path)

;;;;; Files

(defcustom path-file-notes
  (file-name-concat path-dir-notes "notes.org")
  "Path to the notes file."
  :type 'file
  :group 'path)

(defcustom path-file-inbox-desktop
  (file-name-concat path-dir-android "inbox-desktop.org")
  "Path to the desktop inbox file."
  :type 'file
  :group 'path)

(defcustom path-file-inbox-mobile
  (file-name-concat path-dir-android "inbox-mobile.org")
  "Path to the mobile inbox file."
  :type 'file
  :group 'path)

(defcustom path-file-calendar
  (file-name-concat path-dir-android "calendar.org")
  "Path to the calendar file."
  :type 'file
  :group 'path)

(defcustom path-file-feeds-pablo
  (file-name-concat path-dir-notes "feeds.org")
  "Path to Pablo's feeds file."
  :type 'file
  :group 'path)

(defcustom path-file-tlon-feeds
  (file-name-concat path-dir-dropbox-tlon-core "feeds.org")
  "Path to the Tlön feeds file."
  :type 'file
  :group 'path)

(defcustom path-file-anki
  (file-name-concat path-dir-anki "main.org")
  "Path to the Anki file."
  :type 'file
  :group 'path)

(defcustom path-file-init
  (file-name-concat path-dir-dotemacs "init.el")
  "Path to the `init.el' file."
  :type 'file
  :group 'path)

(defcustom path-file-config
  (file-name-concat path-dir-dotemacs "config.org")
  "Path to the `org-mode' config file."
  :type 'file
  :group 'path)

(defcustom path-file-karabiner
  (file-name-concat path-dir-karabiner "modifications.org")
  "Path to the Karabiner `org-mode' file."
  :type 'file
  :group 'path)

(defcustom path-file-karabiner-edn
  (file-name-concat path-dir-karabiner "karabiner.edn")
  "Path to the Karabiner `edn' file."
  :type 'file
  :group 'path)

(defcustom path-file-personal-bibliography-old
  (file-name-concat path-dir-personal-bibliography "old.bib")
  "Path to the `old.bib' file."
  :type 'file
  :group 'path)

(defcustom path-file-personal-bibliography-new
  (file-name-concat path-dir-personal-bibliography "new.bib")
  "Path to the `new.bib' file."
  :type 'file
  :group 'path)

(defcustom path-files-bibliography-personal
  `(,path-file-personal-bibliography-new
    ,path-file-personal-bibliography-old)
  "List of personal bibliography files."
  :type '(repeat file)
  :group 'path)

(defcustom path-files-bibliography-all '()
  "List of all bibliography files.
This includes personal files and `tlon-babel' files."
  :type '(repeat file)
  :group 'path)

(defcustom path-file-wiki-notes
  (file-name-concat path-dir-wiki "wiki-notes.org")
  "Path to the `wiki-notes.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-wiki-published
  (file-name-concat path-dir-wiki "wiki-published.org")
  "Path to the `wiki-published.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-wiki-help
  (file-name-concat path-dir-wiki "wiki-help.org")
  "Path to the `wiki-help.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-quotes
  (file-name-concat path-dir-blog "quotes.org")
  "Path to the `quotes.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-films
  (file-name-concat path-dir-notes "films.org")
  "Path to the `films.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-tlon-tareas-leo
  (file-name-concat path-dir-dropbox-tlon-leo "tareas.org")
  "Path to the Leo's `tareas.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-tlon-tareas-fede
  (file-name-concat path-dir-dropbox-tlon-fede "tareas.org")
  "Path to the Fede's `tareas.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-org2blog
  (file-name-concat path-dir-websites ".org2blog.org")
  "Path to the `.org2blog.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-orb-noter-template
  (file-name-concat path-dir-personal-bibliography "orb-noter-template.org")
  "Path to the `orb' noter template."
  :type 'file
  :group 'path)

(defcustom path-file-orb-capture-template
  (file-name-concat path-dir-bibliographic-notes "${citekey}.org")
  "Path to the `orb' capture template."
  :type 'file
  :group 'path)

(defcustom path-file-bookmarks
  (file-name-concat path-dir-dropbox "bookmarks")
  "Path to the bookmarks file."
  :type 'file
  :group 'path)

(defcustom path-file-ledger
  (file-name-concat path-dir-ledger "journal.ledger")
  "Path to the `ledger' journal file."
  :type 'file
  :group 'path)

(defcustom path-file-ledger-db
  (file-name-concat path-dir-ledger ".pricedb")
  "Path to the `ledger' `.pricedb' file."
  :type 'file
  :group 'path)

(defcustom path-file-metaculus
  (file-name-concat path-dir-notes "metaculus.org")
  "Path to the `metaculus.org' file."
  :type 'file
  :group 'path)

(defcustom path-file-fm
  (file-name-concat path-dir-notes "future-matters.org")
  "Path to the Future Matters project file."
  :type 'file
  :group 'path)

(defcustom path-file-ean
  (file-name-concat path-dir-notes "ea.news.org")
  "Path to the EA News file."
  :type 'file
  :group 'path)

(defcustom path-file-cookies
  (file-name-concat path-dir-google-drive "Apps/Chrome/cookies.txt")
  "Path to the Chrome cookies file."
  :type 'file
  :group 'path)

(defcustom path-file-work
  (file-name-concat path-dir-notes "work-dashboard.org")
  "Path to the work dashboard file."
  :type 'file
  :group 'path)

(defcustom path-file-tlon-ledger-schedule-file
  (file-name-concat path-dir-dropbox-tlon-ledger "ledger-schedule.ledger")
  "Path to the Tlön `ledger' scheduler file."
  :type 'file
  :group 'path)

(defcustom path-file-tlon-babel
  (file-name-concat path-dir-notes "babel.org")
  "Path to the Babel project file."
  :type 'file
  :group 'path)

(defcustom path-file-tlon-ledger
  (file-name-concat path-dir-dropbox-tlon-ledger "tlon.ledger")
  "Path to the Tlön `ledger' journal file."
  :type 'file
  :group 'path)

(provide 'path)

;;; path.el ends here
