# Codex GitHub admin authorization design

## Problem

The Claude and Codex GitHub write guards use different authorization rules.
Claude permits a repository when the authenticated `gh` user has GitHub admin
permission, then falls back to the shared repository allowlist. Codex consults
only the allowlist.

This makes Codex deny writes to repositories owned by the authenticated account,
including newly created forks such as `benthamite/yasnippet`, even though GitHub
reports `.permissions.admin = true`.

## Design

Port Claude's existing live admin-permission check to the Codex guard. For every
repository-scoped GitHub write:

1. Resolve and normalize the target repository using the existing logic.
2. Ask GitHub whether the authenticated `gh` user has admin permission.
3. Allow the operation when the response is exactly `true`.
4. Otherwise consult `agents/github-write-allowlist.txt`.
5. Deny the operation if neither check authorizes it.

The live check must fail closed. If `gh` is missing, unauthenticated, offline,
or cannot access the repository, the result is not treated as permission; only
an explicit allowlist entry can then authorize the write.

This avoids hardcoding the `benthamite` username and preserves correct behavior
if the active GitHub account changes. It also makes the Claude and Codex guards
follow the same policy.

## Tests

Add a focused behavioral parity test that runs both guard scripts with their
real payload formats and a fake `gh` executable. It must establish that:

- both guards allow a non-allowlisted repository when GitHub reports admin
  permission;
- both deny a non-allowlisted repository when GitHub reports no admin
  permission;
- both fail closed when the GitHub query fails; and
- the existing allowlist remains a fallback when the GitHub query fails.

The exact `benthamite/yasnippet` push target should be covered in the Codex
admin-permission case.

## Documentation

Update the Codex and shared agent documentation, the AI configuration sync
manifest notes, and the allowlist comments to describe the common
admin-permission rule and the allowlist's fallback role.

## Non-goals

This change does not weaken repository target resolution, authorize
organization-wide secrets or variables, allow gists, or change the
self-protection around the guard and allowlist files.
