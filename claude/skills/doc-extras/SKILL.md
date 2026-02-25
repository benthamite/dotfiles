---
name: doc-extras
description: Create or update documentation for all Emacs extras packages following the Denote manual style. Use when the user wants to ensure all extras packages are thoroughly documented and up to date.
---

# Document extras packages

Scan all Emacs extras packages, identify those with missing or outdated documentation, and create or update their `.org` doc files.

## Documentation guidelines

@claude/skills/doc-elisp/SKILL.md

## File locations

- **Source files**: `~/My Drive/dotfiles/emacs/extras/*.el`
- **Doc files**: `~/My Drive/dotfiles/emacs/extras/doc/*.org`
- **Emacs config** (for sample configurations): `~/My Drive/dotfiles/emacs/config.org`

Each `.el` file should have a corresponding `.org` file in `doc/` with the same base name (e.g., `ebib-extras.el` → `doc/ebib-extras.org`).

## Procedure

### 1. Scan and classify

Scan all `.el` files in the extras directory. For each, check whether a corresponding `.org` file exists in `doc/`. Classify each package as:

- **Missing**: no `.org` file → create documentation from scratch.
- **Existing**: `.org` file present → compare against source for staleness and quality.

### 2. Analyze existing docs for staleness

For packages with existing docs, perform the structural diff, content diff, and quality check described in the documentation guidelines.

### 3. Process packages in parallel

Use a team of subagents to process all packages that need work (missing docs, outdated docs, or quality deficits). Each subagent reads the `.el` source and either creates a new `.org` file or updates the existing one, following the documentation guidelines.

### 4. Commit changes

After processing all packages, commit changes to git. Use separate commits:
- One commit for all newly created doc files: `doc: create documentation for PACKAGE-1, PACKAGE-2, ...`
- One commit for all updated doc files: `doc: update documentation for PACKAGE-1, PACKAGE-2, ...`

If both categories are present, create both commits. If only one applies, create only that one.
