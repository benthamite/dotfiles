---
name: add-to-emacs-packages
description: Add an Emacs package to the my-emacs-packages note and create its documentation note. Use when the user wants to register a new package in their public packages list.
argument-hint: "[package-name]"
---

# Add to Emacs packages

Register one of the user's Emacs packages in the public packages list and create an accompanying documentation note.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

## Key paths

- **Packages list**: `/Users/pablostafforini/My Drive/notes/pablos-miscellany/my-emacs-packages.org`
- **Notes directory**: `/Users/pablostafforini/My Drive/notes/pablos-miscellany/`
- **Elpaca repos**: resolve via `emacsclient -e 'init-current-profile'` → `~/.config/emacs-profiles/<profile>/elpaca/repos/<package>/`

## Execution steps

### Step 1: Determine the package name

If `$ARGUMENTS` is provided, use it as the package name. Otherwise, infer the package name from the basename of the current working directory (e.g., if the user is in `~/My Drive/repos/pangram/`, the package is `pangram`).

If neither yields a plausible Emacs package name, ask the user.

### Step 2: Check for duplicates

Read `my-emacs-packages.org` and verify the package is not already listed. If it is, inform the user and stop.

### Step 3: Locate the readme

Resolve the elpaca profile:

```bash
PROFILE=$(emacsclient -e 'init-current-profile' | tr -d '"')
REPO_DIR="$HOME/.config/emacs-profiles/$PROFILE/elpaca/repos/<package>"
```

Look for a readme file in this order of preference:
1. `readme.org`
2. `README.org`
3. `<package>.org` (some packages use their own name, e.g. `wikipedia.org`)
4. `README.md`

If none is found, ask the user for the readme location.

### Step 4: Read the readme and extract information

Read the readme file. Extract two things:

1. **Top-level section headings** (lines matching `^* ` in org, or `^# ` / `^## ` in markdown). These will become `#+INCLUDE` directives.
2. **A short description** (1–3 sentences) summarizing what the package does. Write this yourself based on the readme content. Match the tone and style of existing entries in `my-emacs-packages.org` — concise, factual, using `=package-name=` formatting for Emacs packages and `[[url][name]]` for links to external tools or concepts.

### Step 5: Create the documentation note

Create the file `<package>.org` in the notes directory with this structure:

```org
#+title: =<package>= manual
#+hugo_base_dir: ~/My Drive/repos/stafforini.com/

* =<package>= manual
  :PROPERTIES:
  :EXPORT_FILE_NAME: <package>
  :EXPORT_HUGO_SECTION: notes
  :EXPORT_DATE: <today's date in YYYY-MM-DD format>
  :END:

#+INCLUDE: "<path-to-readme>::*<Section 1>" :minlevel 2
#+INCLUDE: "<path-to-readme>::*<Section 2>" :minlevel 2
...
```

Where `<path-to-readme>` uses the `~/.config/emacs-profiles/<profile>/elpaca/repos/<package>/` format (with the literal profile name, not a variable), and each `#+INCLUDE` line references one of the top-level headings extracted in step 4.

**For markdown readmes**: Since `#+INCLUDE` only works with org files, instead of using `#+INCLUDE` directives, convert the markdown content into org-mode format and write it directly into the note.

Then generate an org ID for the top heading:

```bash
emacsclient -e '(with-current-buffer (find-file-noselect "<filepath>") (goto-char (point-min)) (org-next-visible-heading 1) (org-id-get-create))'
```

Capture the returned ID value (strip surrounding quotes) — this is the `NOTE_ID`.

### Step 6: Add the package to my-emacs-packages.org

Insert a new entry in `my-emacs-packages.org` in **alphabetical order** among the existing `** =<name>=` entries. The entry should follow this structure:

```org
** =<package>=
:PROPERTIES:
:END:

<short description from step 4>

[[id:<NOTE_ID>][Full documentation]]

```

Then generate an org ID for the newly inserted heading:

```bash
emacsclient -e '(with-current-buffer (find-file-noselect "<packages-list-path>") (goto-char (point-min)) (search-forward "** =<package>=") (beginning-of-line) (org-id-get-create))'
```

### Step 7: Add the package to the GitHub profile README

Clone the `benthamite/benthamite` repo (the GitHub profile README) into a temporary directory, add a card for the new package in the "Packages I've developed" section, commit, and push.

```bash
TMPDIR=$(mktemp -d)
gh repo clone benthamite/benthamite "$TMPDIR/benthamite"
```

In `$TMPDIR/benthamite/README.md`, locate the `<!-- PACKAGES:END -->` comment (or, if absent, the closing `</p>` tag right before the end of the "Packages I've developed" section). Insert a new card **in alphabetical order** among the existing cards:

```html
  <a href="https://github.com/benthamite/<package>"><img width="400" src="https://github-readme-stats-fast.vercel.app/api/pin/?username=benthamite&repo=<package>&hide_border=true&theme=transparent" /></a>
```

Then commit and push:

```bash
cd "$TMPDIR/benthamite"
git add README.md
git commit -m "add <package>"
git push
```

Finally, clean up the temporary directory:

```bash
rm -rf "$TMPDIR"
```

### Step 8: Commit the notes changes

Create a single commit with both files in the notes repo:

```
claude: add <package> to emacs packages list
```
