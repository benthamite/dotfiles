# `.env.op` is a reference template, not a secrets file

Decision record for the sensitive-read and secret-leak guards. Written after a
diagnosis on 2026-09-18; the history below is from `git log` and the Epoch
session logs, not from memory.

## Symptom

Any Bash command whose text contained `.env.op` was denied by the
sensitive-read guard unless it took one of a few allowlisted shapes: `cat`,
`git show HEAD:.env.op`, a `perl`/`sed` edit of an Org note that mentions the
file, a `python3 -` heredoc whose program text mentions it, a `git commit -m`
whose message mentions it. Epoch has 24 checkouts with a `.env.op` (20 tracked
in git), so the string appears in ordinary prose, commit messages and scripts
constantly. Friction was logged on 2026-07-27, 2026-08-28 and 2026-09-10 and
captured to the learning inbox twice; on 2026-09-18 it cost three retries in
one session.

## What the file is

`NAME=op://vault/item/field` lines that `op run --env-file` resolves at
runtime. A survey of all 24 Epoch checkouts on 2026-09-18 found 65 `op://`
assignments and 2 plaintext ones (a credentials *path* and a Cloudflare account
id); no values. The file exists precisely so that no secret is ever written to
disk, and 20 copies are committed to GitHub, where every collaborator already
reads them. Blocking an agent from reading a tracked file protects nothing.

## Why the guard blocked it anyway

Nobody decided `.env.op` was a secrets file. The first commit of
`block-sensitive-read.sh` (`11040e0b2`, 2026-04-09) classified `.env.*` as
"environment secrets file" to cover `.env.local`, `.env.production` and
friends; `.env.op` matched by suffix accident. The secret-leak hook's first
commit the same day (`cbaffca67`) exempted `.env.op` from its pattern scan as a
"known secret file", the same accident on the write side.

Every later change treated the classification as given and repaired the
symptom of the day:

- `5ae447281` (2026-08-28) admitted the `op-automations` / `op-desktop`
  brokers to the allowlist after a deadlock with the secret-leak guard.
  The same session's log said plainly: "`.env.op` is misclassified and this is
  still unfixed", and left the exemption as an open question wanting a
  "spoofing-resistant shape". That question was captured to the learning
  inbox (Candidate 2 in `2026-08-28-claude-5153194e.md`) and never promoted.
- `20df5b4a5` (2026-09-02) masked heredoc bodies fed to data sinks, so a note
  that names the file could be written with `cat <<EOF`, but not edited with
  `perl`, nor mentioned in an interpreter-fed program.
- `4dbf328ac` (2026-09-10) widened the label regex so `HEAD:.env.op` and
  `--env-file=.env.op` count as naming the file, and added `git show
  HEAD:.env.op` to the deny tests. This hardened the accident: the agent
  fixing a staff-data failure read the gap as "the guard does not recognise
  the file" rather than "the guard should not care about this file".

Category: structural gap. The mechanism misclassified a whole file class, and
the learning pipeline recorded the correct diagnosis twice without a path to
act on it.

## Decision

1. The sensitive-read guards (standalone Claude copy, standalone Codex copy,
   inlined copy in `pretooluse-bash.sh`) mask the exact basename `.env.op`
   before classification. `.env`, `.env.local`, `.env.op.bak` and every other
   `.env.*` name stay covered. The Read and Grep branches exclude the same
   basename.
2. The secret-leak guards stop exempting `.env.op` from the secret-pattern
   scan (Bash content and the Write tool). A literal token written into the
   template is denied like anywhere else. This is what makes step 1 safe
   against the agent itself: the template cannot be turned into a secrets file
   through any agent tool, so reading it can never print a value the agent
   put there.

A negative control written for this change found that no copy recognised a
quoted or parenthesised name (`open('.env')`, `"$DIR/.env"` inside a program)
as naming the file, so the label regex now also accepts `(`, `"` and `'` in
front of `.env` / `.envrc`. This tightens the guard; it does not touch the
template exemption.

## Follow-up from the learning-inbox review (same day)

Reviewing the 29 inbox records that mention the template surfaced two more
items of the same class, both implemented in the follow-up commit:

3. Tracked `.env.example` and `.env.op.<name>.example` templates (16 in
   `paid-service-savings` alone) are exempt on the same grounds as `.env.op`.
   Two inbox candidates (2026-06-13, 2026-07-24) had asked for exactly this.
   `.env.op.finance` without the suffix stays covered: it may be a real local
   file.
4. A quoted `git commit -m` / `--message=` argument is masked before the
   sensitive-path scan (`mask_git_commit_messages` in `lib-heredoc.sh`). The
   message is data git stores, never a file git reads, so naming `.env` or
   `.env.local` in it is inert. The mask applies only to a quoted message that
   follows a `commit` word in the same simple command and holds no command
   substitution, so `git commit -m "$(cat .env)"`, `git commit -F .env`,
   `less -m '.env'` and `git commit -m x && cat .env` still deny. This was the
   residual friction left after item 1: the first two attempts to commit item 1
   itself were refused because the message named the real dotenv files.

Residual risk, accepted: a human commits a plaintext secret into a tracked
`.env.op`. The file is then already exposed to every repository collaborator,
the output redactor still masks well-formed tokens in tool output, and the
repo's own secret hygiene is the right place to catch it.

The "key the exemption on being the `--env-file` argument" shape considered on
2026-08-28 was rejected: it would keep denying the prose, commit-message and
editing cases that made up most of the friction, and the write-side guard
closes the spoofing concern more directly.

## Verification

`tests/test_env_op_template_policy.py` runs all three sensitive-read copies
against read, edit, commit-message and heredoc shapes on both sides, and both
secret-leak copies plus the dispatcher against token and reference writes.
`tests/test_block_sensitive_read.py` keeps the `op-automations run` loader
shapes and now expects the template reads to pass.
