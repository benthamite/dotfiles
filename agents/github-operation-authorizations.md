# Scoped GitHub contribution authorization

Use this route after the user explicitly authorizes creating a particular fork
or upstream PR that the repository allowlist does not cover. Do not edit the
hooks or add broad upstream repository access for a contribution.

Authorization is a user decision. The helper records its scope; it cannot prove
consent from a supplied explanation. A fix request alone does not authorize
publication. An existing applicable authorization needs no second confirmation.

## Prepare and record

Prepare the reviewed branch, PR title/body and intended local retention change
first. Recheck the published head before creating the PR. Write the exact command
to a private file outside Drive, with one optional final newline. Supported forms:

```sh
gh repo fork OWNER/REPO --clone=false
gh pr create --repo OWNER/REPO --base BASE --head ACCOUNT:BRANCH --title "TITLE" --body-file /absolute/path/body.md
```

PR creation optionally accepts `--draft`. Use explicit targets and literal shell
arguments. Shell operators, expansions, extra commands, duplicate flags and
other operations are rejected. Forks are created in the authenticated personal
account; organization forks and cloning are outside this route.

After user authorization, generate one inert record:

```sh
bin/github-operation-authorization prepare \
  --command-file /absolute/path/command.txt --account ACCOUNT \
  --authorization "Shareable reference to the explicit user authorization" \
  --hours 4
```

Add the resulting object to `grants` in
`agents/github-operation-authorizations.json`. Preserve unrelated records and
commit only the owned grant. The hooks read this file from the committed Git
revision; working-tree and staged-only records confer no authorization.

The manifest is public dotfiles content. Use shareable authorization references,
titles and paths, without credentials or private transcript excerpts. The PR
body itself stays in its original file; only its SHA-256 hash is recorded. If
public provenance would expose restricted information, this route is unsuitable.

## Execute and retire

Run the exact command while its grant is valid. The hooks check its text, expiry,
authenticated `github.com` account and PR body hash. Default lifetime is four
hours; the maximum is 24 hours. A different body or command requires a reviewed
replacement record, within existing authority or new user authorization when
the scope changes.

Verify the resulting fork or PR through its explicit URL. On a timeout, inspect
GitHub before retrying; a grant is not permission to create duplicate PRs. Remove
the completed grant and commit that removal. Keep grants empty when idle.

These checks occur before command execution. They do not make body files,
authentication or branch tips immutable, and they are not single-use tokens.
Keep the reviewed artifacts unchanged through publication. The surrounding
workflow still verifies the PR's actual head and local loaded revision.

Pushes to the user's fork use the existing repository allowlist. Grants do not
authorize upstream pushes, merges, PR edits, comments, issues or settings changes.
The normal guard still evaluates commands that have no matching valid grant.
Malformed matching records fail closed; repair or remove them rather than
weakening the guard.

## Checks

`tests/test_github_operation_authorization.py` exercises committed and uncommitted
grants through both hook entrypoints, including Codex orchestration. It uses a
disposable repository and a fake account lookup; it never publishes to GitHub.
