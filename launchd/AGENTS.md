# Launchd Jobs

This directory contains dotfiles-managed macOS `launchd` material.

Public-safe files can live directly under this directory or under
`agents/public/`. Private LaunchAgent plists belong under `agents/private/`.
Anything that records private operational inventory, local machine details,
service names that should not be public, command arguments with tokens, or
security/remediation notes belongs under one of the git-crypt encrypted
subtrees:

- `launchd/agents/private/`
- `launchd/registry/`
- `launchd/plans/`

Use full absolute paths in docs and plans when describing live locations such
as `~/Library/LaunchAgents`. Do not load, unload, replace, or symlink live
LaunchAgents without explicit confirmation.

## Intended Layout

- `agents/public/`: public-safe LaunchAgent plists.
- `agents/private/`: sensitive LaunchAgent plists.
- `registry/`: private inventory of expected jobs and review state.
- `plans/`: private implementation and remediation plans.
- `bin/`: helper scripts, when they are public-safe.

## Audit

Run the read-only audit with:

```sh
python3 /Users/pablostafforini/My\ Drive/dotfiles/launchd/bin/launchd-audit.py --registry /Users/pablostafforini/My\ Drive/dotfiles/launchd/registry/jobs.json
```

Use `--no-launchctl` when you only want to compare the encrypted registry with
`/Users/pablostafforini/Library/LaunchAgents`.

## Privacy Rule

If in doubt, put the file under an encrypted subtree first. It can be moved to a
public location later after review.
