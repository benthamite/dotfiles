---
name: readme-gen
description: Generate or update a README.md for an Emacs Lisp package from its org manual. Use when the user says "generate readme", "create readme", "update readme", "readme from manual", or wants a GitHub-facing README for an Elisp package.
---

# Generate README.md from org manual

Create or update a `README.md` for an Emacs Lisp package. The README is a lightweight GitHub-facing introduction, distinct from the org manual which is the comprehensive reference.

## Prerequisites

Before doing anything, verify both conditions. If either fails, stop and explain why.

### 1. Elisp package

The current repository must contain at least one `.el` file with a valid package header (a `;;; filename.el --- description` line). Use the file whose name matches the repo directory name. If none matches, use the main `.el` file.

### 2. Org manual

The repository must contain a `README.org` file that serves as the package manual (this is the convention established by the `doc-elisp` skill).

If `README.org` is not found, abort with a message suggesting the user create one first (mention the `doc-elisp` skill).

## Existing README.md

If a `README.md` already exists, ask the user whether they want to update it. If they decline, stop. If they confirm (or if no `README.md` exists), proceed.

Since `README.org` is the manual, note that once `README.md` exists, GitHub will display `README.md` as the repo's readme instead of `README.org`. This is the desired behavior — the manual remains accessible via its link in the README.

## Generating the README

Read the org manual and the main `.el` file thoroughly. Then produce a `README.md` with the following structure.

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

- **package-vc** (built-in since Emacs 30):

  ```emacs-lisp
  (package-vc-install "https://github.com/OWNER/PACKAGE")
  ```

- **Elpaca**:

  ```emacs-lisp
  (use-package PACKAGE
    :ensure (PACKAGE :host github :repo "OWNER/PACKAGE"))
  ```

- **straight.el**:

  ```emacs-lisp
  (straight-use-package
   '(PACKAGE :type git :host github :repo "OWNER/PACKAGE"))
  ```

Infer `OWNER/PACKAGE` from the git remote:

```bash
git remote get-url origin
```

If the package has dependencies beyond what Emacs provides, list them after the install snippets under a "Dependencies" sub-heading.

Also mention the minimum Emacs version from the `Package-Requires` header.

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

After generating the README, tell the user that if the manual contains a Roadmap section, they may want to remove it from the org file now that it lives in the README (but do NOT modify the org file automatically).

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
