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
- **No argument provided**: use `AskUserQuestion` to ask the user:
  - **Current file** — lint the main `.el` file in the current working directory (find it with `*.el` glob, excluding `-autoloads.el` and `-pkg.el`; if multiple, pick the one matching the directory name).
  - **Current directory** — lint all `.el` files in the current working directory.
  - **Custom path** — the user will type a file or directory path via the "Other" option.

## Step 2: Collect diagnostics

For each target `.el` file, collect diagnostics from two sources in parallel:

### Byte-compile warnings

Resolve the elpaca profile and build a load-path that includes all `elpaca/builds/*/` directories plus the file's own directory. Then byte-compile:

```bash
PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"')
ELPACA="$HOME/.config/emacs-profiles/$PROFILE/elpaca"

emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  -L "$(dirname FILE)" \
  --eval "(progn (setq byte-compile-warnings 'all) (byte-compile-file \"FILE\"))" \
  2>&1
```

Parse lines matching `Warning:` from stderr.

### Checkdoc notes

Run flymake's checkdoc backend in batch mode on the same file:

```bash
emacs --batch \
  --eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))" \
  -L "$(dirname FILE)" \
  --eval "
(progn
  (find-file \"FILE\")
  (require 'flymake)
  (flymake-mode 1)
  (sleep-for 5)
  (dolist (d (flymake-diagnostics))
    (message \"%s:%d: %s: %s\"
             (buffer-name)
             (line-number-at-pos (flymake-diagnostic-beg d))
             (flymake-diagnostic-type d)
             (flymake-diagnostic-text d))))" \
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

## Step 5: Report

Print a short summary:
- Number of files checked
- Number of diagnostics found and fixed
- Any diagnostics intentionally skipped, with reason

Commit all changes with a message summarizing what was fixed.
