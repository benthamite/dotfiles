#!/usr/bin/env bash
# PostToolUse: regenerate declared Texinfo manuals after agent edits.
# Export only the owned sibling artifacts. Refuse unreviewed evaluation and
# external-data directives; use private staging and sanitized partial reports.
set -euo pipefail

changed_manual_paths() {
  printf '%s' "$1" | jq -r '.tool_input.file_path // empty'
}
input=$(cat)

record_failure() {
  failed=$((failed + 1))
  failures="$failures$1: $2"$'\n'
}

manual_output_names() {
  MANUAL_EXPORT_SOURCE="$1" MANUAL_EXPORT_MODE=names emacs --batch -Q --eval '
(progn
  (setq enable-local-variables nil
        enable-local-eval nil
        enable-dir-local-variables nil)
  (require (quote org))
  (require (quote org-element))
  (require (quote json))
  (setq org-element-use-cache nil)
  (condition-case nil
      (let ((source (getenv "MANUAL_EXPORT_SOURCE"))
            exports infos)
        (with-temp-buffer
          (insert-file-contents source)
          (org-element-map (org-element-parse-buffer) (quote keyword)
            (lambda (element)
              (let ((key (org-element-property :key element))
                    (value (org-element-property :value element)))
                (cond ((equal key "EXPORT_FILE_NAME") (push value exports))
                      ((equal key "TEXINFO_FILENAME") (push value infos)))))))
        (when (or (> (length exports) 1) (> (length infos) 1))
          (error "Ambiguous output declarations"))
        (let* ((export-name (or (car exports) (file-name-nondirectory source)))
               (texi-name (concat (file-name-sans-extension export-name) ".texi"))
               (info-name (or (car infos)
                              (concat (file-name-sans-extension texi-name) ".info"))))
          (dolist (name (append exports infos (list texi-name info-name)))
            (when (or (string-empty-p name)
                      (member name (quote ("." "..")))
                      (string-match-p "[^A-Za-z0-9_. -]" name)
                      (not (equal name (string-trim name))))
              (error "Output must be a literal sibling basename")))
          (unless (string-suffix-p ".info" info-name)
            (error "Info output must have the .info extension"))
          (princ (json-encode (vector texi-name info-name)))))
    (error (kill-emacs 42))))' 2>/dev/null
}

artifact_state() {
  python3 - "$1" <<'PY'
import json, os, stat, sys
try:
    entry = os.lstat(sys.argv[1])
except FileNotFoundError:
    print("missing")
    sys.exit(0)
if not stat.S_ISREG(entry.st_mode):
    sys.exit(1)
print(json.dumps([entry.st_dev, entry.st_ino, entry.st_size, entry.st_mtime_ns, entry.st_ctime_ns]))
PY
}

publish_artifact() {
  python3 - "$1" "$2" "$3" <<'PY'
import json, os, stat, sys
source, destination, expected = sys.argv[1:]
try:
    entry = os.lstat(destination)
    if not stat.S_ISREG(entry.st_mode):
        sys.exit(1)
    current = json.dumps([entry.st_dev, entry.st_ino, entry.st_size, entry.st_mtime_ns, entry.st_ctime_ns])
except FileNotFoundError:
    current = "missing"
if current != expected:
    sys.exit(1)
if current != "missing":
    os.chmod(source, stat.S_IMODE(entry.st_mode))
os.replace(source, destination)
PY
}

total_texi=0
total_info=0
failed=0
failures=""
if ! scratch=$(mktemp -d /tmp/manual-export-hook.XXXXXX); then
  jq -n '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":"Texinfo export failed: private staging could not be created; no generated files were changed"}}'
  exit 0
fi
stage_texi=""
stage_info=""
clear_staging() {
  [ -z "$stage_texi" ] || rm -f -- "$stage_texi"
  [ -z "$stage_info" ] || rm -f -- "$stage_info"
}
cleanup() {
  clear_staging
  rmdir "$scratch"
}
trap cleanup EXIT

while IFS= read -r file_path; do
  [ -n "$file_path" ] || continue
  [[ "$file_path" == *.org ]] || continue
  [ -f "$file_path" ] || continue
  if [ -L "$file_path" ]; then
    record_failure "$file_path" "Manual source symlinks require an explicit reviewed export"
    continue
  fi
  # A generic Markdown/PDF export filename is not a Texinfo trigger.
  if ! grep -qiE '^[[:space:]]*#\+(texinfo_filename:[[:space:]]*[^[:space:]]|export_file_name:[[:space:]]*.+\.info[[:space:]]*$)' "$file_path"; then
    continue
  fi
  if ! dir=$(cd -- "$(dirname -- "$file_path")" 2>/dev/null && pwd -P); then
    record_failure "$file_path" "Manual directory could not be resolved"
    continue
  fi
  base=$(basename -- "$file_path" .org)
  file_path="$dir/$base.org"
  if ! source_state=$(artifact_state "$file_path" 2>/dev/null) ||
     ! names=$(manual_output_names "$file_path") ||
     ! texi_name=$(printf '%s' "$names" | jq -er '.[0]' 2>/dev/null) ||
     ! info_name=$(printf '%s' "$names" | jq -er '.[1]' 2>/dev/null); then
    record_failure "$file_path" "Manual output-name preflight failed; review unique literal sibling names without paths, quotes or directives"
    continue
  fi
  texi="$dir/$texi_name"
  info="$dir/$info_name"
  if ! texi_state=$(artifact_state "$texi" 2>/dev/null) ||
     ! info_state=$(artifact_state "$info" 2>/dev/null); then
    record_failure "$file_path" "Generated output is not an ordinary owned sibling file"
    continue
  fi
  clear_staging
  stage_texi="$scratch/$texi_name"
  stage_info="$scratch/$info_name"
  export_status=0
  MANUAL_EXPORT_SOURCE="$file_path" MANUAL_EXPORT_OUTPUT="$stage_texi" \
  MANUAL_EXPORT_INFO="$info_name" MANUAL_EXPORT_MODE=export emacs --batch -Q --eval '
(progn
  (setq enable-local-variables nil
        enable-local-eval nil
        enable-dir-local-variables nil
        create-lockfiles nil
        make-backup-files nil
        auto-save-default nil)
  (require (quote org))
  (require (quote ox-texinfo))
  (setq org-export-use-babel nil
        org-export-allow-bind-keywords nil
        org-export-global-macros nil
        org-export-before-processing-hook nil
        org-export-before-parsing-hook nil
        org-element-use-cache nil)
  (define-error (quote manual-export-unsupported) "Unsupported automatic manual export")
  (condition-case nil
      (let ((source (getenv "MANUAL_EXPORT_SOURCE"))
            (output (getenv "MANUAL_EXPORT_OUTPUT"))
            (info (getenv "MANUAL_EXPORT_INFO"))
            (case-fold-search t))
        (when (string-match-p "[@{}\\\\\n\r]" info)
          (signal (quote manual-export-unsupported) nil))
        (with-temp-buffer
          (insert-file-contents source)
          (org-element-map (org-element-parse-buffer)
              (quote (keyword babel-call export-block export-snippet))
            (lambda (element)
              (let ((kind (org-element-type element))
                    (key (org-element-property :key element))
                    (value (org-element-property :value element)))
                (when
                    (or (eq kind (quote babel-call))
                        (and (eq kind (quote keyword))
                             (or (member key (quote ("MACRO" "BIND" "CALL" "INCLUDE" "SETUPFILE"
                                                     "TEXINFO_HEADER" "TEXINFO_POST_HEADER")))
                                 (and (equal key "TEXINFO")
                                      (not (string-match-p
                                            "^[ \t]*@printindex[ \t]+\\(?:fn\\|vr\\|cp\\|ky\\|pg\\|tp\\)[ \t]*$"
                                            value)))))
                        (and (eq kind (quote export-block))
                             (equal (downcase (or (org-element-property :type element) "")) "texinfo"))
                        (and (eq kind (quote export-snippet))
                             (equal (downcase (or (org-element-property :back-end element) "")) "texinfo")))
                  (signal (quote manual-export-unsupported) nil)))))
          (setq buffer-file-name source
                default-directory (file-name-directory source))
          (org-mode)
          (let ((org-export-preserve-breaks nil)
                (org-export-with-title t))
            (org-export-to-file (quote texinfo) output nil nil nil nil
                                (list :preserve-breaks nil :with-title t :texinfo-filename info))))
        (with-temp-buffer
          (insert-file-contents output)
          (let ((filename-count 0))
            (while (re-search-forward "\\(@+\\)\\([[:alpha:]]+\\)" nil t)
              (when (= (% (length (match-string 1)) 2) 1)
                (let ((directive (downcase (match-string 2))))
                  (when (member directive (quote ("include" "verbatiminclude" "image" "macro" "rmacro"
                                                   "alias" "definfoenclose")))
                    (signal (quote manual-export-unsupported) nil))
                  (when (equal directive "setfilename")
                    (setq filename-count (1+ filename-count))))))
            (unless (= filename-count 1)
              (signal (quote manual-export-unsupported) nil))
            (goto-char (point-min))
            (unless (re-search-forward "^@setfilename .*$" nil t)
              (signal (quote manual-export-unsupported) nil))
            (replace-match (concat "@setfilename " info) t t)
            (write-region (point-min) (point-max) output nil (quote silent)))))
    (manual-export-unsupported (kill-emacs 42))
    (error (kill-emacs 1))))' >/dev/null 2>&1 || export_status=$?
  if [ "$export_status" -ne 0 ] || [ ! -s "$stage_texi" ]; then
    if [ "$export_status" -eq 42 ]; then
      record_failure "$file_path" "Texinfo export refused unsupported evaluation, include, raw Texinfo or image directives; use a reviewed manual export"
    else
      record_failure "$file_path" "Texinfo export failed; previous generated files were preserved"
    fi
    continue
  fi
  info_ready=0
  if [ "$info_state" != missing ]; then
    if ! makeinfo --no-split "$stage_texi" -o "$stage_info" >/dev/null 2>&1 ||
       [ ! -s "$stage_info" ]; then
      record_failure "$file_path" "Info regeneration failed; the previous .info was preserved"
    else
      info_ready=1
    fi
  fi
  if ! current_source=$(artifact_state "$file_path" 2>/dev/null) ||
     [ "$current_source" != "$source_state" ]; then
    record_failure "$file_path" "Manual source changed during export; generated artifacts were not published"
    continue
  fi
  if ! publish_artifact "$stage_texi" "$texi" "$texi_state" 2>/dev/null; then
    record_failure "$file_path" "Texinfo publication failed or its sibling changed; no success was confirmed"
    continue
  fi
  total_texi=$((total_texi + 1))
  if [ "$info_ready" -eq 1 ]; then
    if ! publish_artifact "$stage_info" "$info" "$info_state" 2>/dev/null; then
      record_failure "$file_path" "Info publication failed or its sibling changed; no success was confirmed"
      continue
    fi
    total_info=$((total_info + 1))
  fi
done < <(changed_manual_paths "$input" | awk '!seen[$0]++')

[ "$total_texi" -gt 0 ] || [ "$failed" -gt 0 ] || exit 0
jq -n --argjson t "$total_texi" --argjson i "$total_info" --argjson failed "$failed" --arg failures "$failures" \
  '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":("Regenerated " + ($t|tostring) + " .texi and " + ($i|tostring) + " .info file(s). Failures: " + ($failed|tostring) + (if $failed > 0 then "\n" + $failures else "" end))}}'
