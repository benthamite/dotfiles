---
name: doc-extras
description: Audit, create, or refresh documentation for all Emacs extras packages in the dotfiles repo. Use when the user asks to document every extras package, fill missing extras docs, refresh outdated extras docs, check extras documentation coverage, or bring emacs/extras/doc up to date with emacs/extras sources.
---

# Document extras packages

Scan all Emacs extras packages, identify missing or outdated documentation, and create or update their `.org` manuals.

Use `doc-elisp` for the detailed documentation style and source-analysis rules. This skill is the batch workflow for the dotfiles extras tree.

## When not to use

- For a single Elisp package or file, use `doc-elisp` directly.
- For generating GitHub-facing Markdown from an existing Org manual, use `generate-readme`.
- For linting, byte-compilation, or release readiness, use the relevant Elisp lint or release workflow instead.
- For non-extras packages outside `~/My Drive/dotfiles/emacs/extras/`, use `doc-elisp` unless the user explicitly asks for a broader batch documentation pass.

## Documentation guidelines

@claude/skills/doc-elisp/SKILL.md

## File locations

- **Source files**: `~/My Drive/dotfiles/emacs/extras/*.el`
- **Doc files**: `~/My Drive/dotfiles/emacs/extras/doc/*.org`
- **Emacs config** (for sample configurations): `~/My Drive/dotfiles/emacs/config.org`
- **Texinfo export hook**: `~/My Drive/dotfiles/emacs/extras/doc/.dir-locals.el`

Each top-level `.el` file should have a corresponding `.org` file in `doc/` with the same base name (for example, `ebib-extras.el` -> `doc/ebib-extras.org`). This includes files such as `paths.el` whose name does not end in `-extras`.

## Procedure

### 1. Prepare

- Read `doc-elisp` before judging documentation quality.
- Check `git status --short` and note unrelated dirty files. Do not stage or rewrite unrelated changes.
- If editing this paired global skill itself, update both `claude/skills/doc-extras/SKILL.md` and `codex/skills/doc-extras/SKILL.md` unless `ai-config-sync.json` records an explicit divergence.

### 2. Scan and classify

Scan top-level `.el` files in the extras directory. For each source file, check whether a corresponding `.org` file exists in `doc/`. A useful inventory command is:

```bash
while IFS= read -r src; do
  base=$(basename "$src" .el)
  test -f "emacs/extras/doc/$base.org" || printf 'missing %s\n' "$base"
done < <(find emacs/extras -maxdepth 1 -type f -name '*.el' | sort)
```

Classify each package as:

- **Missing**: no `.org` file, so create documentation from scratch.
- **Existing**: `.org` file present, so compare against source for staleness and quality.

### 3. Analyze existing docs for staleness

For packages with existing docs, perform the structural diff, content diff, and quality check described in `doc-elisp`.

At minimum, compare the documented public surface against the public definitions in the source:

```bash
rg -n '^\((defcustom|defvar|defconst|defun)\s+' emacs/extras/PACKAGE.el
```

Exclude internal `--` symbols from the required documentation set unless the existing manual explains them as implementation context. For commands, confirm whether each `defun` is interactive before deciding it belongs in the `Commands` section.

### 4. Process packages

Use subagents when available, ideally one package per worker. Each worker must read the `.el` source and the existing `.org` manual, then either create a new manual or update the existing one according to `doc-elisp`.

If subagents are unavailable, process packages in small independent batches. Do not skip the source/doc comparison just because many packages need attention.

### 5. Verify

Before committing any package documentation change:

- Re-read the changed `.org` manual and compare it with the source definitions.
- Confirm each changed manual has required front matter, `CUSTOM_ID` drawers, Texinfo index entries, and the final `* Indices` section from `doc-elisp`.
- Export changed manuals to Texinfo, or save them in Emacs so `emacs/extras/doc/.dir-locals.el` exports them. When running a direct export, use:

```bash
emacs --batch -Q --visit emacs/extras/doc/PACKAGE.org \
  --eval "(require 'ox-texinfo)" \
  --funcall org-texinfo-export-to-texinfo
```

- Check `git diff -- emacs/extras/PACKAGE.el emacs/extras/doc/PACKAGE.org emacs/extras/doc/PACKAGE.texi` to ensure only intended documentation and generated Texinfo changes are present.

### 6. Commit changes

After processing all packages, commit changes to git. Use one commit per package:
- For newly created docs: `PACKAGE: create documentation`
- For updated docs: `PACKAGE: update documentation`

Stage only the package's documentation files and generated Texinfo for that commit. If unrelated dirty files are present, leave them unstaged.

## Final response

Report:

- Packages documented or updated.
- Verification performed, including Texinfo export status.
- Commit hashes created.
- Any packages left unresolved and why.
