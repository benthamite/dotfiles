# Context-delivery remediation (2026-10-05)

Status: partly implemented; under independent review before the rest proceeds.

## Problem

Pablo's principle (`agents/instruction-placement.org`): an agent should have
the context it needs exactly when it needs it, and not otherwise. An incident
on 2026-10-05 showed it failing: a Claude agent raised a Chrome window with
AppleScript and stole Pablo's focus, although `claude/context/service-access.md`
forbade it. Causes found:

1. Nothing delivered the browser rules at browser-use time. The only route was
   a CLAUDE.md pointer triggered by services (Gmail, GitHub, ...), and the ban
   was worded as Codex-only.
2. CLAUDE.md's pointers say `context/<file>.md`, which resolves to
   `~/.claude/context/`, a path that does not exist (the files live in
   `~/My Drive/dotfiles/claude/context/`; `bin/ai-config-sync` deliberately
   rewrites `claude/context/` to `context/` around line 2237).
3. The agent's own session-learning record proposed "or bring it forward" as a
   remedy for a background tab.

A four-part read-only audit (always-loaded files, context docs, skills, hooks)
also found dead hook files, stale docs, contradictions and guard false
positives. Pablo dislikes new hooks and growth of CLAUDE.md/AGENTS.md.

## Approved items and status

Done (dotfiles `main`, local, unpushed):

- 1 `a6c66d7f7` New paired `chrome-use` skill; browser rules moved there from
  service-access.md, stated for all agents, contradictions resolved (retry
  policy follows `chrome-bridge-recover.py`; the `open-chrome-window.js` step
  is skipped); declares precedence over the vendor Chrome skills.
- 2 `04fd578a3` `bin/chrome-profile-open --background` (`open -g`), used by
  `chrome-bridge-recover.py`. Not tested against live Chrome.
- 3 `848034ebc` Codex browser docs: CLI uses Playwright MCP, desktop app
  control-chrome; download-paper and end-to-end point to chrome-use.
- 5 `b619ded4f` CLAUDE.md/AGENTS.md: dropped "or a manually opened Chrome
  profile" from the service-access trigger.
- 6 Session-learning record amended (gitignored inbox; no commit).
- 10 `7ec2efecf` paste-via-kill-ring skill matches CLAUDE.md (stage, then show).
- 11 `c60863dc6` proofread no longer pins `model: sonnet`.
- 14 `67abcafa7` agent-guard: on an internal guard crash, Read/Grep/Glob are
  allowed with a warning; other tools stay denied.
- 15 `237889b17`, `c457d0b3b` CLAUDE.md/AGENTS.md: agent-status bullet cut to
  one sentence (details in `bin/agent-status --help`); agent-log-find bullet
  replaced by a `find-past-session` skill.

Incident during implementation: the agent doing item 12 edited the live
`lib/python/agent_guard/policies/github_write.py` and left a missing import,
which made the guard deny every Bash/Edit in every session until Pablo fixed it
by hand. The work-in-progress was saved as a patch and the file restored to
HEAD; `bin/test-guards` passes (621).

Remaining:

- 1b Correct `chrome-use` (edit refused by the auto-mode classifier as
  self-modification; needs Pablo's approval): obtaining the agent's own tab or
  tab group via the extension's tools (`tabs_context_mcp` with `createIfEmpty`,
  `tabs_create_mcp`) is the normal, allowed start; the window ban covers only
  out-of-band routes (AppleScript/osascript, `open -a`, agent-run
  chrome-profile-open, links that open windows, raising/reordering windows);
  agents work only in their own tabs (Claude in Chrome cannot adopt the user's
  tabs, per code.claude.com/docs/en/chrome); for per-tab sign-ins (1Password
  web) the agent navigates its own tab and asks Pablo to sign in there, naming
  the tab and its Chrome profile window. Prompted by a live session that
  refused to start, then opened a new tab instead of the user's.
- 4 Make CLAUDE.md's `context/` pointers resolve: a symlink
  `~/.claude/context` -> `~/My Drive/dotfiles/claude/context`, created by
  whatever already manages `~/.claude` links, so no instruction text changes.
- 7 Trash 12 unregistered hook files (7 Trajectory/CR hooks, 4 Emacs-notify
  copies the agent package ships itself, `session-learning-capture.sh` in
  both runtimes) and their manifest/README entries.
- 8 Fix docs that list disabled skills as active (skill inventory, READMEs,
  `instruction-placement.org` naming the disabled
  `optimize-agent-instructions`).
- 9 Stale-content cleanup: google-services.md (`.zshenv-secrets` token steps,
  duplicate tables, claude.ai integrations), mcp-servers.md (missing
  `.claude-personal2` in sync scope, broken README reference), secrets.md
  (wrong path, Epoch "see below" contradiction).
- 12 github_write false positive (`cd repo && git push` denied as having no
  repository; ~106 log hits): bind a write segment to a preceding literal
  `cd DIR &&`, treat text-only stages (printf/echo/kill-ring-put) as inert,
  stay fail-closed when ambiguous. Develop and test on a copy of the guard
  (e.g. a temp checkout), never the live tree.
- 13 Delete `remind-ai-config-sync.sh` and `remind-claude-readme-update.sh`
  (both runtimes) and their registrations; the commit gates already enforce
  the same.
- New project-local skill `context-delivery-audit` (`.claude/skills`,
  `.codex/skills` in dotfiles) that reruns this four-part audit; its
  description distinguishes it from the existing `config-audit`, which audits
  rule wording rather than delivery.
- 16 Deferred: redesign the secret/process-output guard classifiers.

Constraints: commit each item separately with explicit pathspecs; leave
foreign uncommitted changes (`codex/config.toml`, `codex/hooks.json`,
`emacs/config.org`, `shell/.zshenv-secrets`) untouched; keep Claude/Codex
counterparts in sync (`bin/ai-config-sync audit`, `bin/docs-audit audit`);
no new hooks; no new lines in CLAUDE.md/AGENTS.md; no push.

## Review questions

1. Do the completed commits do what their item says, without collateral
   damage (check the diffs)? In particular: does chrome-use now forbid normal
   extension tab creation; does item 15 lose any rule; is item 14 safe?
2. Is each remaining item the right, minimal fix for its cause, and is
   anything missing or harmful (e.g. item 4's symlink approach vs the
   ai-config-sync rewrite; item 7's dead-file claims)?
3. Does the plan follow `agents/instruction-placement.org`?
