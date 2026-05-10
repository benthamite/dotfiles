---
name: lint-elisp
description: Fix byte-compile warnings, checkdoc notes, and other Elisp diagnostics in one or more files. Use when the user says "lint elisp", "fix warnings", "fix diagnostics", "elisp lint", "checkdoc", "byte-compile warnings", or wants to clean up Elisp compiler/linter output.
argument-hint: [file-or-directory]
---

# Lint Elisp

Fix all byte-compile warnings and checkdoc notes in the target file(s), then verify the fixes produce a clean diagnostic run.

## Step 1: Determine targets

Check `$ARGUMENTS` for a file or directory path.

- **File path provided**: lint that single file.
- **Directory path provided**: lint all `.el` files in that directory (non-recursive). Skip files that are auto-generated (e.g. `*-autoloads.el`, `*-pkg.el`).
- **No argument provided**: ask the user which target to lint. Use the
  available structured input mechanism (`AskUserQuestion` in Claude Code,
  `request_user_input` in Codex Plan mode); if none is available, ask a concise
  plain-text question with these choices:
  - **Current file** — lint the main `.el` file in the current working directory (find it with `*.el` glob, excluding `-autoloads.el` and `-pkg.el`; if multiple, pick the one matching the directory name).
  - **Current directory** — lint all `.el` files in the current working directory.
  - **Custom path** — the user will type a file or directory path via the "Other" option.

## Step 2: Collect diagnostics

For each target `.el` file, collect diagnostics from two sources in parallel.
Use an absolute `FILE` value; the examples below are safe for paths containing
spaces. If `emacsclient` cannot return `init-current-profile`, stop and report
that the active profile could not be resolved instead of guessing.

### Byte-compile warnings

Resolve the elpaca profile and build a load-path that includes all `elpaca/builds/*/` directories plus the file's own directory. Then byte-compile:

```bash
FILE="/absolute/path/to/file.el"
DIR="$(dirname "$FILE")"
PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"')
test -n "$PROFILE" || { echo "Could not resolve init-current-profile"; exit 1; }
ELPACA="$HOME/.config/emacs-profiles/$PROFILE/elpaca"

emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  -L "$DIR" \
  --eval "
(progn
  (require 'bytecomp)
  (let* ((file (expand-file-name \"$FILE\"))
         (temp-dir (make-temp-file \"lint-elisp-byte-compile-\" t))
         (byte-compile-dest-file-function
          (lambda (source)
            (expand-file-name
             (concat (file-name-nondirectory
                      (file-name-sans-extension source))
                     \".elc\")
             temp-dir))))
    (unwind-protect
        (progn
          (setq byte-compile-warnings 'all)
          (byte-compile-file file))
      (delete-directory temp-dir t))))" \
  2>&1
```

Parse lines matching `Warning:` from stderr. The temporary destination keeps
byte-compilation from leaving `.elc` files beside the source.

### Checkdoc notes

Run flymake's checkdoc backend directly in batch mode on the same file:

```bash
FILE="/absolute/path/to/file.el"
DIR="$(dirname "$FILE")"
PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"')
test -n "$PROFILE" || { echo "Could not resolve init-current-profile"; exit 1; }
ELPACA="$HOME/.config/emacs-profiles/$PROFILE/elpaca"

emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  -L "$DIR" \
  --eval "
(progn
  (require 'flymake)
  (require 'elisp-mode)
  (find-file (expand-file-name \"$FILE\"))
  (let (diagnostics)
    (elisp-flymake-checkdoc
     (lambda (diags &rest _args)
       (setq diagnostics diags)))
    (dolist (d diagnostics)
      (message \"%s:%d: %s: %s\"
               (buffer-name)
               (line-number-at-pos (flymake-diagnostic-beg d))
               (flymake-diagnostic-type d)
               (flymake-diagnostic-text d)))))" \
  2>&1
```

Parse lines containing `:note:` or `:warning:`.

If both sources return no diagnostics for a file, skip it — it is already clean.

## Step 3: Fix each diagnostic

Read the relevant lines of the file and fix each issue. Common patterns:

| Diagnostic | Typical fix |
|---|---|
| **Free variable reference** | The `defvar`/`defcustom`/`defconst` is defined after its first use. Move the definition earlier (into the customization or variables section), or add a `(defvar VAR)` forward declaration if moving is impractical. |
| **Unused lexical variable** | Remove the variable, or prefix with `_` if it must remain (e.g. destructuring). |
| **Argument should appear in doc string** | Mention the argument (UPPERCASED) in the docstring. Weave it into the existing first sentence when possible rather than appending mechanically. |
| **"verb" should be imperative** | Checkdoc expects the first sentence of a docstring to use imperative mood. Reword: "Returns X" → "Return X", "contains X" → "contain X". If the flagged word is not actually a verb (e.g. "calls" as a noun in "tool calls"), rephrase to avoid the false positive. |
| **Doc string wider than 80 characters** | Rewrap or split the first line. |
| **First sentence should end with period** | Add a period. |

When fixing docstrings, preserve the meaning and do not add unnecessary verbosity. If a fix would make the docstring worse, note it and skip.

When moving definitions (e.g. a `defcustom` forward), place them in the section where similar definitions live — do not just shove them above the first use.

## Step 4: Verify

Re-run both diagnostic commands from Step 2 on every file that was modified. If new diagnostics appear (e.g. a rewording triggered a new checkdoc note, or a line became too wide), fix those too. Repeat until the output is clean.

For edited `.el` files, also run the relevant `elisp-conventions`
verification: `batch-test.sh PACKAGE` before commit, then a targeted live
`emacsclient -e` check after commit when the file belongs to a loaded package.

## Step 5: Report

Print a short summary:
- Number of files checked
- Number of diagnostics found and fixed
- Any diagnostics intentionally skipped, with reason

Inspect `git status --short`, stage only files modified by this lint run, and
commit them with a message summarizing what was fixed.
