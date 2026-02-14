---
name: doc-extras
description: Create or update documentation for all Emacs extras packages following the Denote manual style. Use when the user wants to ensure all extras packages are thoroughly documented and up to date.
---

# Document extras packages

Scan all Emacs extras packages, identify those with missing or outdated documentation, and create or update their `.org` doc files to match the style, rigor, and comprehensiveness of Protesilaos Stavrou's Denote manual.

## File locations

- **Source files**: `~/Library/CloudStorage/Dropbox/dotfiles/emacs/extras/*.el`
- **Doc files**: `~/Library/CloudStorage/Dropbox/dotfiles/emacs/extras/doc/*.org`
- **Emacs config** (for sample configurations): `~/Library/CloudStorage/Dropbox/dotfiles/emacs/config.org`

Each `.el` file should have a corresponding `.org` file in `doc/` with the same base name (e.g., `ebib-extras.el` → `doc/ebib-extras.org`).

## Procedure

### 1. Scan and classify

Scan all `.el` files in the extras directory. For each, check whether a corresponding `.org` file exists in `doc/`. Classify each package as:

- **Missing**: no `.org` file → create documentation from scratch.
- **Existing**: `.org` file present → compare against source for staleness and quality.

### 2. Analyze existing docs for staleness

For packages with existing docs, perform two checks:

**Structural diff.** Extract all public definitions from the `.el` file:
- `defcustom` (user options)
- `defvar`, `defconst` (public variables, no `--` prefix)
- Interactive `defun` (commands)
- Non-interactive `defun` without `--` prefix (public functions)

Compare against what the `.org` file documents. Identify:
- **Added**: definitions in `.el` not documented in `.org`.
- **Removed**: items documented in `.org` that no longer exist in `.el`.
- **Renamed**: items that appear to have been renamed (similar name, same purpose).

**Content diff.** For items present in both, compare the docstring in `.el` against the description in `.org`. Flag items where the `.org` description no longer accurately reflects the docstring or function behavior.

**Quality check.** Regardless of structural changes, assess whether the existing documentation meets the Denote-style quality standard (see below). Terse, API-reference-style documentation should be rewritten to meet the standard.

### 3. Process packages in parallel

Use a team of subagents to process all packages that need work (missing docs, outdated docs, or quality deficits). Each subagent reads the `.el` source and either creates a new `.org` file or updates the existing one.

When updating existing docs:
- Add documentation for new items in the appropriate section.
- Remove documentation for deleted items.
- Update descriptions for items whose behavior has changed.
- Rewrite terse sections to meet the quality standard.
- Ensure the Overview accurately reflects the package's current feature set.
- Preserve any existing content that is accurate and well-written.

### 4. Commit changes

After processing all packages, commit changes to git. Use separate commits:
- One commit for all newly created doc files: `doc: create documentation for PACKAGE-1, PACKAGE-2, ...`
- One commit for all updated doc files: `doc: update documentation for PACKAGE-1, PACKAGE-2, ...`

If both categories are present, create both commits. If only one applies, create only that one.

## Documentation format

### Front matter

```org
#+title: PACKAGE-NAME: Short description
#+author: Pablo Stafforini
#+email: pablo@stafforini.com
#+language: en
#+options: ':t toc:t author:t email:t num:t
#+startup: content
#+export_file_name: PACKAGE-NAME.info
#+texinfo_filename: PACKAGE-NAME.info
#+texinfo_dir_category: Emacs misc features
#+texinfo_dir_title: Display Name: (PACKAGE-NAME)
#+texinfo_dir_desc: Short description
```

Follow the front matter with a one-line introduction:

```
This manual describes the features and customization options for the Emacs Lisp file =PACKAGE-NAME.el=.
```

### Section structure

Every doc file must include these sections in this order. Omit a section only if the package has no items of that type.

1. `* Overview`
2. `* User options` — for `defcustom` variables
3. `* Commands` — for interactive functions
4. `* Functions` — for public non-interactive functions worth documenting
5. Integration-specific sections — if the package integrates with other packages in notable ways (e.g., `* Embark integration`, `* Transient menus`)
6. `* Indices` — always last

The Indices section always contains exactly:

```org
* Indices
:PROPERTIES:
:CUSTOM_ID: h:indices
:END:

** Function index
:PROPERTIES:
:INDEX: fn
:CUSTOM_ID: h:function-index
:END:

** Variable index
:PROPERTIES:
:INDEX: vr
:CUSTOM_ID: h:variable-index
:END:
```

### Subsection organization

Group related items under thematic subsections (`**`, `***`). Choose headings that describe the *purpose* or *workflow*, not the symbol name:

- GOOD: `** File handling and attachment`
- GOOD: `** Searching for entries on external websites`
- BAD: `** ebib-extras-open-file-dwim`
- BAD: `** Miscellaneous`

Individual items (a single command or option) get their own heading only when they need substantial documentation (multiple paragraphs, examples, etc.). Otherwise, group several related items under one subsection.

### PROPERTIES drawers

Every heading at every level must have a `:PROPERTIES:` drawer with `:CUSTOM_ID:`:

```org
* Commands
:PROPERTIES:
:CUSTOM_ID: h:commands
:END:

** File handling
:PROPERTIES:
:CUSTOM_ID: h:file-handling
:END:
```

Convention for CUSTOM_ID values:
- Top-level sections: `h:overview`, `h:user-options`, `h:commands`, `h:functions`, `h:indices`
- Thematic subsections: `h:DESCRIPTIVE-SLUG` (e.g., `h:file-handling`, `h:entry-processing`)
- Individual symbols (when they have their own heading): `h:SYMBOL-NAME` (e.g., `h:ebib-extras-download-use-vpn`)

### Texinfo index entries

Place these directives immediately after the `:END:` of the PROPERTIES drawer, before the prose:

- `#+findex: FUNCTION-NAME` for every documented function and command.
- `#+vindex: VARIABLE-NAME` for every documented variable and user option.

When multiple functions are documented under one subsection, place all their `#+findex:` entries together at the top of that subsection.

### Cross-references

Link liberally within the same doc file using Org internal links:

```org
This function uses ~ebib-extras-get-file~ internally ([[#h:file-handling][File handling]]).
```

Whenever you mention a function, variable, or concept documented elsewhere in the same file, add a cross-reference. This is a key quality marker of the Denote style.

For references to other extras packages, use verbatim markup: `=PACKAGE-NAME.el=` (cross-file Texinfo links are not practical for these packages).

### Markup conventions

- `~tildes~` for Lisp symbols: function names, variable names, package names, keywords, nil, t.
- `=equals=` for literal values: file paths, key sequences, format strings, literal strings.
- `*bold*` sparingly, for emphasis in important notes.
- `/italic/` rarely, only for introducing technical terms.

## Quality standard

The documentation must match the rigor of Protesilaos Stavrou's Denote manual. The key attributes are:

### Overview

Do not merely list features. The overview should:

- Explain the *purpose* of the package: what problem it solves and why it exists as a separate file.
- Describe its relationship to the underlying Emacs package it extends (e.g., "=ebib-extras.el= extends =ebib=, the Emacs BibTeX database manager").
- Mention key external dependencies.
- Provide a thematic map of capabilities organized by workflow, not just a flat bullet list.

### User options

Each `defcustom` must include:

- Its default value.
- The type of value it accepts (boolean, string, integer, list, choice, etc.).
- A clear explanation of what the option controls.
- *When* and *why* a user would want to change it from the default.
- How it interacts with other options or commands, if relevant.

### Commands

Each interactive command must include:

- A contextual explanation of *when* and *why* a user would invoke the command.
- The context in which it operates (e.g., "while in the Ebib index buffer", "with point on an Org heading").
- Its arguments, prefix argument behavior, and DWIM branches.
- What happens in edge cases (empty region, no file found, etc.).
- Cross-references to related commands, options, and functions.

Do NOT just paraphrase the docstring. Add value by explaining usage context, workflows, and relationships that are not obvious from the docstring alone.

### Functions

Document public (non-`--` prefixed) non-interactive functions when they are:
- Useful for customization or hooks.
- Part of workflows that users might want to extend.
- Called by documented commands (helps the reader understand the architecture).

Internal (`--` prefixed) functions should NOT be documented.

### Prose style

- Write in a direct, informative, second-person tone. The Denote manual says things like "The user option ~denote-directory~ specifies..." — do the same.
- Explain *why* before *how*. Provide context before reference details.
- Use complete sentences. No telegram-style fragments.
- Keep paragraphs to 2-4 sentences.
- Use Elisp code blocks (`#+begin_src emacs-lisp ... #+end_src`) when an example clarifies usage.
- Refer to Emacs commands in the standard way when mentioning interactive invocation: =M-x command-name=.
- When a command has a noteworthy implementation detail (e.g., uses `el-patch`, calls an external process, depends on a specific mode), mention it briefly — the reader should know what's happening under the hood without reading the source.

## Example

Here is an example of a well-documented command section, showing the expected level of detail and style:

```org
** Switching to the last window
:PROPERTIES:
:CUSTOM_ID: h:switch-to-last-window
:END:

#+findex: window-extras-switch-to-last-window
A common workflow involves editing in one window, briefly switching to
another for reference, and then wanting to return. The command
~window-extras-switch-to-last-window~ selects the most recently used
window in the current frame, providing a quick toggle between two
windows without relying on directional movement or window numbers.

If the minibuffer was the last selected window and is currently active,
the command will switch to it. This makes it easy to return to an
ongoing minibuffer session after checking something in a file buffer.

Internally, the command uses ~window-extras-get-last-window~ to
determine the target window ([[#h:utility-functions][Utility functions]]).
```

And a well-documented user option:

```org
** ~ebib-extras-download-use-vpn~
:PROPERTIES:
:CUSTOM_ID: h:ebib-extras-download-use-vpn
:END:

#+vindex: ebib-extras-download-use-vpn
When set to non-nil, download commands such as ~ebib-extras-download-book~
route their requests through a VPN connection ([[#h:downloading][Downloading files]]).
This is useful when accessing resources that are geo-restricted or when
you want to avoid revealing your IP address to file hosting services.

The VPN integration relies on the =mullvad= package. You must have
Mullvad VPN installed and configured separately.

The default value is ~nil~, meaning downloads use your direct internet
connection.
```
