---
name: add-to-emacs-packages
description: Add or register one of the user's Emacs packages in my-emacs-packages.org, create its documentation note, and add its GitHub profile README card. Use when the user wants to update the public Emacs packages list for a specific package.
argument-hint: "[package-name]"
---

# Add to Emacs packages

Register one of the user's Emacs packages in the public packages list, create an accompanying documentation note, and add the matching GitHub profile README card.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

Do not use this for general Emacs package documentation, README generation, or release work unless the user also wants the package registered in the public packages list. Use `doc-elisp`, `generate-readme`, or `release` for those narrower workflows.

## Key paths

- **Packages list**: `/Users/pablostafforini/My Drive/notes/public/my-emacs-packages.org`
- **Notes directory**: `/Users/pablostafforini/My Drive/notes/public/`
- **Elpaca sources**: resolve via `emacsclient -e 'init-current-profile'` → `~/.config/emacs-profiles/<profile>/elpaca/sources/<package>/`

## Safety boundaries

- Step 7 creates externally visible GitHub changes. Only push the GitHub profile README change when the current user request explicitly authorizes updating the profile README, or after asking for and receiving confirmation.
- Clone `benthamite/benthamite` only when Step 7 is authorized. The Step 2 `gh api` read-only check is fine before confirmation.
- Clean up temporary directories with `trash`, not destructive recursive deletion.

## Execution steps

### Step 1: Determine the package name

If the user supplied a package name, use it. Otherwise, infer the package name from the basename of the current working directory (e.g., if the user is in `~/My Drive/repos/pangram/`, the package is `pangram`).

If neither yields a plausible Emacs package name, ask the user.

### Step 2: Check what already exists

Check **both** of the following:

1. Whether the package is already listed in `my-emacs-packages.org`.
2. Whether the package already has a card in the `benthamite/benthamite` GitHub profile README (check with `gh api repos/benthamite/benthamite/contents/README.md --jq .content | base64 -d | grep -F "repo=<package>"`).

If the package exists in **both** places, inform the user and stop. If it exists in only one, skip the steps for the place where it already exists and proceed with the remaining steps.

### Step 3: Locate the readme

Resolve the elpaca profile:

```bash
PROFILE=$(emacsclient -e 'init-current-profile' | tr -d '"')
REPO_DIR="$HOME/.config/emacs-profiles/$PROFILE/elpaca/sources/<package>"
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
  :EXPORT_HUGO_CUSTOM_FRONT_MATTER: :unlisted true
  :END:

#+INCLUDE: "<path-to-readme>::*<Section 1>" :minlevel 2
#+INCLUDE: "<path-to-readme>::*<Section 2>" :minlevel 2
...
```

Where `<path-to-readme>` uses the `~/.config/emacs-profiles/<profile>/elpaca/sources/<package>/` format (with the literal profile name, not a variable), and each `#+INCLUDE` line references one of the top-level headings extracted in step 4.

**For markdown readmes**: Since `#+INCLUDE` only works with org files, instead of using `#+INCLUDE` directives, convert the markdown content into org-mode format and write it directly into the note.

Then generate an org ID for the top heading:

```bash
emacsclient -e '(with-current-buffer (find-file-noselect "<filepath>") (goto-char (point-min)) (org-next-visible-heading 1) (prog1 (org-id-get-create) (save-buffer)))'
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
emacsclient -e '(with-current-buffer (find-file-noselect "<packages-list-path>") (goto-char (point-min)) (search-forward "** =<package>=") (beginning-of-line) (prog1 (org-id-get-create) (save-buffer)))'
```

### Step 7: Add the package to the GitHub profile README

If authorized by the user request or by a confirmation in this session, clone the `benthamite/benthamite` repo (the GitHub profile README) into a temporary directory, add a card for the new package in the "Packages I've developed" section, commit, and push. If the GitHub profile update is not authorized, ask for confirmation before this step and do not clone or push yet.

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
trash "$TMPDIR"
```

### Step 8: Verify and commit the notes changes

Before committing, verify that:

1. `<package>.org` exists in the notes directory.
2. The package list contains the new `** =<package>=` heading in alphabetical order.
3. Both generated org IDs were saved to disk as `:ID:` properties.
4. The `[[id:<NOTE_ID>][Full documentation]]` link points to the documentation note ID.
5. If Step 7 was completed, the GitHub profile README contains the new card and the temporary clone was moved to the trash.

Create a single commit with both files in the notes repo:

```
claude: add <package> to emacs packages list
```
