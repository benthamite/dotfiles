;;; ox-hugo-extras.el --- Extensions for ox-hugo -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ox-hugo-extras.el
;; Version: 0.1
;; Package-Requires: ((ox-hugo "0.8"))

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

;; Extensions for `ox-hugo'.  Registers org-cite export processors for
;; stafforini.com Hugo export.

;;; Code:

(require 'ox-hugo)
(require 'oc)

;;;; Citation processors

;; For notes: convert [cite:@Key] to {{< cite "Key" >}} shortcodes.
;; Style /t or /short emits a short (title-only) citation: {{< cite "-Key" >}}
(org-cite-register-processor 'hugo-cite
  :export-citation
  (lambda (citation style _backend _info)
    (let* ((refs (org-cite-get-references citation))
           (short (member (car style) '("t" "short")))
           (parts
            (mapcar
             (lambda (ref)
               (let* ((key (org-element-property :key ref))
                      (cite-key (if short (concat "-" key) key))
                      (suffix (org-element-property :suffix ref))
                      (suffix-str (if suffix
                                      (string-trim (org-element-interpret-data suffix))
                                    "")))
                 (if (string-empty-p suffix-str)
                     (format "{{< cite \"%s\" >}}" cite-key)
                   (format "{{< cite \"%s\" \"%s\" >}}"
                           cite-key
                           (replace-regexp-in-string "\"" "\\\\\"" suffix-str)))))
             refs)))
      (mapconcat #'identity parts ", ")))
  :export-bibliography
  (lambda (_keys _files _style _props _backend _info) ""))

;; For quotes: suppress citations entirely (handled by work pages)
(org-cite-register-processor 'hugo-cite-noop
  :export-citation (lambda (_citation _style _backend _info) "")
  :export-bibliography (lambda (_keys _files _style _props _backend _info) ""))

(provide 'ox-hugo-extras)
;;; ox-hugo-extras.el ends here
