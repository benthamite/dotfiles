---
name: generate-readme
description: Generate or update a GitHub-facing README.md for an Emacs Lisp package from its README.org manual. Use when the user says "generate readme", "create readme", "update readme", "readme from manual", or wants a concise Markdown README for an Elisp package. Do not use for general project README writing or for packages without an org manual; use document-elisp-package first when the manual is missing.
---

# Generate README.md from org manual

Create or update a `README.md` for an Emacs Lisp package. The README is a lightweight GitHub-facing introduction, distinct from the org manual which is the comprehensive reference.

## Workflow

1. Verify the package and manual prerequisites before writing `README.md`.
2. Read the org manual, the main `.el` file, and any existing `README.md` thoroughly enough to distinguish canonical manual content from README-specific material.
3. Generate or update `README.md` using the structure below, preserving accurate repo-specific content from an existing README.
4. Verify the resulting README, any manual edits, and the git diff before reporting completion or committing.

## Prerequisites

Before doing anything, verify both conditions. If either fails, stop and explain why.

### 1. Elisp package

The current repository must contain at least one `.el` file with a valid package header (a `;;; filename.el --- description` line). Use the file whose name matches the repo directory name. If none matches, use the main `.el` file.

### 2. Org manual

The repository must contain a `README.org` file that serves as the package manual (this is the convention established by the `document-elisp-package` skill).

If `README.org` is not found but a case-variant exists (e.g. `readme.org`), rename it to `README.org` using `git mv` and commit the rename before proceeding.

If no org manual exists at all, abort with a message suggesting the user create one first (mention the `document-elisp-package` skill).

## Existing README.md

If a `README.md` already exists and the user explicitly asked to create, update, regenerate, or refresh it, proceed without asking for another confirmation. If the request is only exploratory or ambiguous, ask before replacing the existing README.

Before editing an existing README, inspect it for README-specific material that may not belong in the manual: badges, screenshots, CI status, contribution links, license notes, project caveats, installation variants, or historical context. Preserve accurate material that is still useful, and drop stale or duplicate content only when the manual or source files make that safe.

Since `README.org` is the manual, note that once `README.md` exists, GitHub will display `README.md` as the repo's readme instead of `README.org`. This is the desired behavior — the manual remains accessible via its link in the README.

## Generating the README

Read the org manual, the main `.el` file, and the existing README if present. Then produce a `README.md` with the following structure.

### Structure

#### 1. Title and tagline

```markdown
# `package-name`: One-sentence description from the package header
```

#### 2. Overview

A concise summary (2–4 paragraphs) explaining:

- What the package does and what problem it solves.
- Who it is for (target audience, assumed context).
- Key capabilities, organized thematically — not an exhaustive list of every command, but enough to convey scope.

Draw from the manual's Overview section but rewrite for a reader who is deciding whether to install the package. Be concrete: mention specific workflows, not just abstract features.

#### 3. Screenshots or demos

If the repository contains screenshots or GIFs (check `screenshots/`, `images/`, `media/`, or similar directories, and also any image references in the org manual or existing README), include them:

```markdown
## Screenshots

![Description](screenshots/example.png)
```

If no screenshots exist, omit this section entirely — do not add a placeholder.

#### 4. Installation

```markdown
## Installation
```

Include installation instructions for the most common methods:

- **package-vc / use-package `:vc`** (built into Emacs 30):

  ```emacs-lisp
  (use-package PACKAGE
    :vc (:url "https://github.com/OWNER/PACKAGE"))
  ```

- **Elpaca**:

  ```emacs-lisp
  (use-package PACKAGE
    :ensure (:host github :repo "OWNER/PACKAGE"))
  ```

- **straight.el**:

  ```emacs-lisp
  (use-package PACKAGE
    :straight (:host github :repo "OWNER/PACKAGE"))
  ```

Infer `OWNER/PACKAGE` from the git remote:

```bash
git remote get-url origin
```

If the repository has no GitHub remote or the remote cannot be parsed confidently, adapt the snippets to the actual host or state the assumption instead of fabricating an owner or repository name.

If the package has dependencies beyond what Emacs provides, list them after the install snippets under a "Dependencies" sub-heading.

Also mention the minimum Emacs version from the `Package-Requires` header. If the minimum is older than Emacs 30, present the `:vc` example as the Emacs 30+ built-in option rather than implying every supported Emacs version has it.

#### 5. Quick start

```markdown
## Quick start
```

A brief "getting started" section: the minimum configuration and first command a new user should try. Keep it to 5–10 lines of Elisp at most. This should give the reader a working setup in under a minute.

#### 6. Documentation

```markdown
## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).
```

#### 7. Roadmap (if applicable)

If the org manual contains a "Roadmap" section, a "Future plans" section, or a "Planned features" section, **move that content here**. Rewrite it in markdown, preserving the substance. Use a checklist format:

```markdown
## Roadmap

- [ ] Planned feature one
- [ ] Planned feature two
- [x] Completed feature (keep if recently completed and informative)
```

If no roadmap content exists in the manual, omit this section.

After moving the roadmap content to the README, **remove the Roadmap section from the org manual** and commit the removal together with the README (or as a separate commit). Do not leave any trace of the roadmap in the manual — no empty section, no "see README" pointer, no commented-out content. Do not add any note in the README about the roadmap also being present in the manual.

#### 8. Contributing and license (if applicable)

Only include these if the repository already has a `CONTRIBUTING.md` or `LICENSE` file. If so, add brief sections linking to them. Do not fabricate license information.

### Style guidelines

- Always render the package name in backtick monospace (`` `package-name` ``), both in the title and in body text.
- Write in a direct, informative tone. Second person ("you") is fine.
- Prefer short paragraphs and bullet lists over walls of text.
- Use fenced code blocks with `emacs-lisp` language tag for Elisp.
- Do not duplicate the full manual content — the README is an entry point, not a mirror.
- Do not include badges unless the repo already has CI or other badge-worthy infrastructure.
- Do not add a table of contents — GitHub renders one automatically for markdown files.

## Verification and closeout

Before finishing:

- Re-read the generated `README.md` and any edited `README.org`.
- Check that every local image, manual, license, and contributing link in the README points to an existing file.
- Check that the README has no placeholder owner, package, screenshot, or command names.
- If roadmap content was moved, verify that no Roadmap, Future plans, or Planned features section remains in `README.org`.
- Run `git diff --check`.
- Inspect `git status --short` and stage only the README/manual files changed for this task.

If the user asked for file changes and the repo is under git, commit the verified README/manual update using the repository's commit conventions. Do not commit when the user asked only for a draft, review, or recommendation.
