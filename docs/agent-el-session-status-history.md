# agent.el session status: everything tried, and what failed

Compiled 2026-10-10 in session 70afc41c, after the user had to reconstruct this
history by hand across several sessions.  **Read this whole first section
before proposing any change to how agent.el decides whether a Claude or Codex
session is busy, waiting, idle, or waiting with background work.**  The
appendices hold the full evidence: git history (A), session transcripts (B)
and dotfiles (C).

## Rules for any session working on this

1. **Search this document and the logs before proposing anything.**  Every
   idea below, including "look at how Orca does it", has come up before.  If
   you propose something listed here as tried, say so and say what is
   different this time.
2. **Do not call a fix permanent, robust or done** until the exact
   user-visible behaviour has been checked across *all* live sessions, not
   only the one example in front of you.  Each of the claims dated 07-27,
   08-24, 09-24, 10-01, 10-03, 10-04, 10-06 and 10-10 was followed by a new
   failure.
3. **Test against reality, not against fixtures you wrote.**  Fixtures only
   show that the code matches your assumptions.  Capture real hook payloads
   and real transcripts.
4. **Update this document** in the same commit series as any change to
   state detection.  Add the attempt, its date and commit, and later what
   broke it.

## The problem in one paragraph

agent.el runs Claude Code's interactive terminal UI (through claude-code.el,
in an eat terminal).  That UI does not expose session state.  agent.el has to
infer busy, waiting and background work from side channels:

- hooks delivered through emacsclient;
- the status line;
- transcripts;
- the rendered screen;
- the undocumented `background_tasks` field in Stop/SubagentStop payloads.

Every side channel has gaps, and Claude Code keeps adding new behaviours, so
each fix closes one gap and the next one surfaces.  Tools with reliable state
drive Claude through the Agent SDK / stream-json instead (see "How other
tools do it").  Codex sessions in agent.el already use a structured channel,
the app-server JSON-RPC turn flag, and are much more stable.

## What is active today (2026-10-10)

- **Claude busy/waiting** comes from the event-driven state machine (since
  06-11), fed by turn-lifecycle hooks shipped in the package and passed per
  session through `--settings` (09-24, 10-03).  It also uses send-function
  advice and the status-line `prompt_id`.
- **Claude background work (amber)** comes from the Stop/SubagentStop
  `background_tasks` list (`dc0d128`, 10-03) with three exclusions:
  - the stopping subagent itself;
  - teammates, unless the `.agents/` directory records them as working
    (`b36a2fc`);
  - Monitors that Claude Code arms itself when an artifact is published
    (`e37eb00`, 10-10; upstream issue anthropics/claude-code#98816).
- **The Remote Control task UI** is screen-scraped, and also counts as
  background work.
- **The terminal idle probe** (`:idle-p`) is used only by the before-exit
  chain (08-24).
- **Codex busy/waiting** comes from the app-server JSON-RPC turn flag (since
  07-26).  Codex background work is still read from the footer, and Codex
  terminal sessions still scrape `• Working`.
- **Published agent status** is written by agents via `bin/agent-status`
  (09-20).  It is shown in the switcher and, since 10-10, in the mode line.
- **Dotfiles hooks** (Claude only):
  - The Stop hook `block-done-over-running-tasks.py` refuses
    `Status: done` while non-teammate tasks run.  It was live-tested on
    10-10.
  - The TeammateIdle hook `hold-idle-teammate-with-running-task.py` keeps a
    teammate busy while its own background shell has a writer on its output
    file.  This works around undelivered teammate task notices.  It was
    live-tested on 10-10.

## Inventory by mechanism (details in appendices A and B)

| Mechanism | Dates | Outcome |
|---|---|---|
| Boolean waiting flag from Notification/Stop hooks | 05-05 to 06-11 | Replaced by the state machine |
| Codex `Working(...)` and footer regex scraping | 05-12 onward | Called "patch on patch" by the 06-10 audit; still used for Codex terminal sessions |
| Clearing waiting on more input paths; app-server busy | 06-09 | Two Codex fixes 12 minutes apart undid each other |
| Event-driven state machine | 06-11 to now | Active.  Starting it in `busy` caused a 3 h false busy, fixed with an `unknown` start state on 07-26 |
| "Remote Control active" regex | 06-11 | Active; another scraped marker |
| Lifecycle hooks through a dotfiles wrapper | 07-27 | Reverted the same day |
| Status-line `prompt_id` change as turn start | 07-27 | Active |
| Transcript scanning for background work | 07-27 (dropped), 10-01 (built, declared "the permanent fix"), 10-03 (deleted) | Failed: scanner assumptions broke two days later |
| Terminal prompt/spinner scraping for display state | 08-24 | Drove the colours for one day; three regressions in an hour; demoted to `:idle-p` |
| Before-exit watchdogs and idle probes | 07-29 to 08-26 | Repeated "fourth/fifth time" exit failures; current form is `:idle-p` plus watchdog |
| Agent-published status (`agent-status`) | 09-20 to now | Active.  Publishing broke within a day (fixed); measured publish rate was 0% on plain replies and 51% on long turns (10-05) |
| Turn-lifecycle hooks shipped in the package | 09-24 to now | Active.  Hardened repeatedly: dedup, server targeting, per-session settings, `claude -p` filtering, subagent filtering, non-blocking Stop |
| Stop payload `background_tasks` | 10-03 to now | Active.  It over-counts: subagents (10-04), idle teammates (10-10) and artifact watches (10-10) each needed an exclusion |
| Per-agent `.agents/` files from SubagentStart/Stop/TeammateIdle | 10-10 | Active.  Sessions started before the change never show teammate work |
| Stop hook refusing `Status: done` over running tasks | 10-10 | Active.  Catches agents leaving hung jobs |
| Detecting undelivered task notices from transcript `queue-operation` records | 10-10 | Abandoned: a replay of 816 transcripts showed the records cannot be paired (523 unattributable dequeues; not FIFO) |
| TeammateIdle hold while the teammate's own shell has an output-file writer | 10-10 | Active.  Prevents the lost-notice stall |

## Ideas raised but never pursued

- **`claude agents --json`**, which Claude Code's docs call the supported way
  to read session state from outside.
  - Doc pages describing it were read by subagents on 10-01 and 10-03, but no
    session used it.
  - Tested on 10-10: it lists interactive sessions with `status` (busy, idle
    or waiting) and `waitingFor`, and it matched agent.el on every session
    compared.
  - It is scoped per account config dir, so it must be run once per
    `CLAUDE_CONFIG_DIR`.
  - Unverified: whether `busy` includes background work, its latency, and
    its reliability over time.  A 30-minute comparison script exists in
    session 70afc41c's scratchpad (`compare-status.py`) but was never run.
- **The Agent SDK or `claude -p --output-format stream-json`** with
  `CLAUDE_CODE_EMIT_SESSION_STATE_EVENTS=1`.
  - It gives authoritative `session_state_changed`
    (idle/running/requires_action) and `background_tasks_changed`, with
    `ambient` marking Claude's own watchers.
  - It is headless or SDK only, not available to the interactive TUI.  It
    would need agent.el to render the conversation itself, as agent-shell
    does over ACP.
  - ACP/agent-shell was considered on 07-27 and rejected as too complicated.
    The user has since confirmed that sessions run in claude-code.el's
    terminal and that this stays.
- **Per-task liveness from `lsof`** on
  `/private/tmp/claude-<uid>/<project>/<session>/tasks/<id>.output`.
  - Proposed 10-03 and superseded by `background_tasks`.
  - Verified on 10-10: a running shell task always has a process holding
    its output file open for writing (lsof mode `w`), and a finished,
    stopped or killed task never does.  Fake exit markers don't fool it.
  - It does not cover Monitors or agents.
  - The TeammateIdle hook uses it.  agent.el does not.
- **An explicit "can't tell" state** and a measurement period before any
  change (10-03).
- **A "stalled" colour** for a teammate whose notice never arrived (10-01).
- **Showing each background task's age** (10-04).  The Stop payload has no
  start times.
- **An attention inbox and busy-input queue** (plan ready 07-31, never
  implemented).

## How other tools do it (researched 10-10 from primary sources)

**SDK/stream-json tools** have reliable state:

- Vibe Kanban (`--output-format=stream-json --input-format=stream-json`, with
  `task_*` messages);
- Sculptor;
- Conductor (probably SDK; closed source);
- the VS Code extension;
- Zed and agent-shell, through the ACP adapter, which holds the turn open
  while subagents run.

**Terminal wrappers** use hooks, and all of them struggle with background
work:

- Orca: hooks, the same `background_tasks`, idle teammates ignored, a
  separate `monitoring` sub-state, and a 60 s "owed notification" lease for
  main-agent tasks only;
- cmux: a `pending` flag for background work;
- Superset;
- iTerm2: `cc-status` hooks;
- Warp: hooks plus OSC 777;
- Agent Deck: a hybrid, with a long list of false-state fixes.

Claude Code's own desktop app has bug reports of a status dot that keeps
blinking after background work ends.

**Screen scrapers** are the least reliable: Claude Squad (pane hash) and
claude-code-ui (transcript tail with a 15 s timeout).

Neither Orca nor any other terminal wrapper would have handled the two 10-10
failures (artifact watch, lost teammate notice) correctly.  Orca was first
evaluated on 09-20, but only its display code; its detection was read on
10-10.

## The recurring failure pattern

1. A user-visible symptom appears.  The agent fixes the one cause behind it,
   tests that case, and calls it fixed.
2. A different cause produces the same symptom days later, and it looks like
   the same bug.
3. Research is redone from scratch because nobody reads the earlier
   sessions.  Orca was "researched" on 09-20 and again on 10-03 and 10-10.
   lsof was proposed on 10-03 and again on 10-10.
4. The user is asked to approve each new plan without any way to judge it.

What would break the cycle: one check that compares, for every live
session, agent.el's displayed state with an independent source of truth
(`claude agents --json` per account, and lsof writers for shell tasks), run
before any state change is called done.  Plus this document.

---

# Appendix A: agent repo git history and session logs


Sources: `git log --all` of the agent repo (384 commits, one branch `main`, first commit 075d870 on 2026-05-05) and `logs/*.md` (ten local logs, 2026-09-26 to 2026-10-10 only; nothing before 26 Sep is logged). Many commits before July have no body, so their purpose is read from the diff; those are marked *(from diff)*. Anything not stated in a commit or log is marked *(inferred)*.

Display states today: `busy`, `waiting`, `background-waiting`, `unknown` (plus `closing`, and a `snoozed` override face). The colours come from `agent-waiting`, `agent-waiting-with-background` (inherits `warning`, so it looks brown or amber), `agent-unknown` and `agent-snoozed`. Every surface uses them: switcher, tab bar (e13afd7, 2026-09-26) and mode line (f968f03).

---

### 1. Ownership of the state variable

#### 1a. A boolean waiting flag set by notification hooks (2026-05-05 to 2026-06-11)
- **075d870 (05-05)** *(from diff)*: `agent--waiting-for-input` holds a timestamp. Claude sets it from the `idle_prompt` Notification hook, Codex from its `Stop` hook. Advice on the send functions clears it. `permission_prompt` and `elicitation_dialog` only fire an alert and never set waiting.
- **ec25b85 (05-12) "Fix Codex alerts and background status"** *(from diff)*: adds a `:busy-p` backend slot. For Codex it scrapes `• Working…` from the last 800 chars, and it vetoes a stale waiting flag. Problem fixed *(inferred)*: Codex sessions still showed waiting while they worked.
- **802529d (06-09) "clear waiting state on submit"** *(from diff)*: more advice on Codex send paths (`codex--send-command-to-buffer`, `codex--terminal-send-return`, TUI `:return`). Also, `busy-p` now trusts `codex--app-server-turn-active-p`.
- **b2ee555 (06-11)**: replaces that advice with codex.el's upstream `codex-command-submitted-hook`.
- **Replaced by** the state machine below (de1c2df removed `agent--waiting-for-input`).

#### 1b. Event-driven state machine (2026-06-11, still active)
- **4b23c1b, 59ede95, 1028682, de1c2df (06-11)** *(from diff)*: `agent--session-state` (`busy` / `awaiting-input` / `closing`) is owned by `agent-session-event`. The events are `stop`, `idle-prompt`, `submit` and `exit-request`.
  - Claude: `stop` from the Stop hook (via claude-code.el's `claude-code-hook-wrapper`), `idle-prompt` from the Notification hook, `submit` from advice on claude-code.el's send functions.
  - Codex: Stop maps to `idle-prompt`, `codex-command-submitted-hook` maps to `submit`.
  - `agent-session-display-state` combines this with the backend probes `:waiting-p`, `:busy-p` and `:background-tasks-p`.
- **504151f (07-26) "start sessions in an unknown state"**: the initial value had been `busy`. One live session had shown busy for about 3 h on that initializer alone. Adds `unknown` and the `agent-unknown` face. Backend protocol answers now win, then the event machine.
- **264789c (07-27)**: adds a `blocked` event. Permission and elicitation dialogs had rendered as busy ("a session literally asking the user a question rendered as busy").
- **a423b4e (08-23)**: adds a `user-submit` event so that typed prompts cancel an armed before-exit chain.
- Status: **active**. Every later mechanism feeds events into it.

---

### 2. Turn-start detection for Claude (Claude Code has no turn-start hook)

#### 2a. Advice on claude-code.el send functions (05-05, still active)
This only sees turns the user typed. d21bc95 (07-27) documents the failure: harness-started turns, such as a resume after a background task completes, "left the session marked as waiting while it worked, with no bound".

#### 2b. Lifecycle hooks via a dotfiles wrapper (07-27, reverted the same day)
- **d21bc95 "derive turn state from lifecycle hooks"**: adds an `activity` event. PreToolUse, MessageDisplay and UserPromptSubmit map to `activity`; SessionStart and StopFailure map to `blocked`. Delivery is through a `notify-emacs-state.sh` wrapper that lived in dotfiles and needed `~/.claude/settings.json` entries.
- **Replaced by 5dcd661 (same day)**: settings.json is self-protected against agent edits, and the hook spawned a shell per tool call. The dotfiles wrapper was "reverted separately".

#### 2c. Status-line `prompt_id` change (07-27, still active)
- **5dcd661**: the polled statusline JSON shows a fresh `prompt_id` each turn. A change emits `activity`, unless a stop landed since the last poll (the short-turn guard).
- Status: **still in code** (`agent-claude--detect-turn-start`). It now runs alongside 2d.

#### 2d. Turn-lifecycle hooks shipped in the package (09-24, still active)
- **3b3a5da "Drive Claude session state from turn-lifecycle hooks"**:
  - Forwards UserPromptSubmit, Pre/PostToolUse, PostToolUseFailure and SubagentStart as `activity`, and StopFailure as `stop`, through a package-shipped `hooks/notify-emacs-state.sh` installed by `agent-claude-ensure-state-hook-config`.
  - Events carry their send time, so a late tool event cannot revive a stopped turn (`agent-claude--stale-hook-event-p`).
  - An interrupt fires no hook, so Escape on a busy session schedules a terminal check after 1.5 s (`agent-claude--check-interrupt`).
- Follow-ups that fixed its failures:
  - **bab30f8 (10-01)**: every tool call of every session and subagent ran emacsclient twice. That overflowed the server queue during brief stalls and refused clients, which blanked the published statuses. Emacs now mirrors state to `<sha256(uuid)>.state`, and the hook skips `activity` while that file reads `busy`. Setup had also left hooks pointing at a checkout "two profiles old" (9.0.1-dev; statusline at 8.3.0-dev). Setup now replaces stale entries and adds `agent-claude-settings-package-directory` (log 10-01).
  - **b4bbbe3 (10-03)**: hooks called emacsclient without naming a server, so with several Emacs servers running, events went to whichever owned the default socket. Sessions now get `EMACS_SOCKET_NAME`/`EMACS_SERVER_FILE`.
  - **232bba5 (10-03)**: hooks written into global `~/.claude/settings.json` reached every `claude` run, broke when the checkout moved, and would deliver each event twice. They are now passed per session through a content-addressed `--settings` file. `agent-setup` offers to remove the global entries.
  - **bc7fb69 (10-03)**: child `claude -p` runs inherit `CLAUDE_BUFFER_NAME`/`AGENT_SESSION_UUID`. Their Stop marked the parent waiting mid-turn (wikipedia-improver green from 21:02:24 to 21:06:49) and overwrote its tasks file. Fixed by writing `<key>.sid` on each poll and having `session-owner.sh` drop foreign payloads. Open gap (log 10-03): for up to one poll after `/clear`, hooks may be dropped.
  - **b13f9cf (10-04)**: subagent tool hooks counted as main-session `activity`. tangodb sat at a permission prompt shown as busy, with no face, while its background agent ran Bash. Now `agent_id` is forwarded, and subagent activity is ignored while the session awaits input, unless that subagent raised the dialog (`agent-claude--blocking-agent`). The log calls this the third case of another actor's hooks being attributed to the main session: the transcript-scanner era, `claude -p` children, then subagents.
  - **12cad8b (10-06)**: the Stop hook was a synchronous `claude-code-hook-wrapper` emacsclient call. While Emacs was stalled (debugger at depth 3 from a stray `debug-on-entry`), each turn stayed open until the 600 s hook timeout. The dotfiles server backlog of 128 had turned refusals into waits (log 10-07). Stop now goes through `fire-and-forget.sh` + `notify-emacs-state.sh stop` with a 10 s client timeout. `agent-claude--stale-stop-p` drops a stop sent before the event that began the current busy period.
- Status: **active** (core Claude busy/waiting mechanism today).

---

### 3. Terminal screen scraping

#### 3a. Codex `• Working` busy scrape (05-12, still active for Codex terminal sessions)
`agent-codex--working-regexp` in `agent-codex--busy-p`, OR'd with the app-server turn flag.

#### 3b. Claude idle-prompt and spinner scrape as display authority (08-24, demoted the same day)
- **b0b81f2**: registers `agent-claude--terminal-waiting-p` as Claude's `:waiting-p`. Its docstring gives the reason: a dropped turn, for example at the context limit, fires no Stop and leaves the session busy forever. It treats the session as waiting when the tail shows a `❯`/`>` prompt and no "esc to interrupt", after a 2 s grace period.
- **fbed1ce**: the real spinner reads `✢ Billowing… (4m 35s · …)`. "esc to interrupt" belonged to older releases, and eat renders the prompt space as U+00A0. The regexps were fixed.
- **8b90291 "restrict terminal idle evidence to the before-exit chain"**: "Screen scraping is not reliable enough to drive the switcher… every scraping gap a wrong display state (spinner variants, the global fleet-agents footer)." Display state went back to the state machine, and the scrape became Claude's `:before-exit-ready-to-close-p`.
- **a63e72b**: as a per-step gate, the scrape lagged the Stop hook and "a finished skill could veto its own completion and strand the chain". Moved to a dedicated `:idle-p` slot, consulted only when the chain is armed and when the watchdog fires.
- **884b5fc (08-26)**: `idle-p` also defers a missing closeout receipt while the session still looks busy. A late Stop had failed the chain with "no receipt file was written".
- Status: **active only as `:idle-p`** for the before-exit chain, the receipt check and the Escape interrupt check (3b3a5da). It no longer drives colours.

#### 3c. Codex prompt-input scrape for before-exit readiness (05-11 to 06-11)
- **d11fba9 (05-11)** *(from diff)*: `agent-codex-before-exit-ready-to-close-p` vetoes closing while the `›` prompt holds text. **c0cb30e (05-12)**: reads the input at the terminal cursor.
- **fbad66a (06-07)**: Codex had switched to `❯`. The regex fell back to a stale `›` echo, so chains never advanced past the first skill.
- **b2ee555 / 4e7647c (06-11)**: replaced by upstream `codex-prompt-input`. Still active in that form.

---

### 4. Background-work detection (the amber/brown colour)

#### 4a. Claude footer task counts (05-05 to 10-01)
- **075d870**: `agent-claude--has-background-tasks-p` scans the last 800 chars for `· N shells` or `· N monitors`.
- **59f58cc (06-11)**: also matches "Remote Control active". **Still active**: it is the only screen evidence left.
- **038d019 (08-24)**: adds `· ← N agents` to count Task agents.
- **8b90291 (08-24)**: drops the `←` form, because it is a fleet-wide indicator that counts agents across all sessions. The log of 10-01 confirmed nine unrelated sessions showed the same count. Effect: subagent and teammate work became invisible.
- **Replaced by f6a48b3 (10-01)**, which stopped reading the footer counts.

#### 4b. Claude transcript scanning (10-01 to 10-03)
- **f6a48b3**: trigger was "waiting sessions whose work ran in teammates or background tasks showed plain green". Premise: "Claude Code reports background tasks through no hook or statusline field." It incrementally scans main and sidechain transcripts for task starts (`backgroundTaskId`, async agent, Monitor, workflow) and ends (task notification, Monitor expiry, TaskStop, subagent end). Verified live with `sleep 75`. The log called it "the permanent fix".
- **94ddbc8 (10-01)**: adds user `!` commands that were moved to the background (recorded only as `<bash-stdout>` text).
- **631e910 (10-01)**: a half-written status file raised `json-end-of-file`. With `debug-on-error`, the poll timer stayed marked triggered and never ran again, which froze state and background updates (stafforini.com's session). Now catches `json-error`.
- Live sessions also held scan state from an uncommitted draft, so every poll errored until cleared in eight sessions (log 10-01).
- **Replaced by dc0d128 (10-03)**: tangodb:default showed green while waiting on a background shell. The scanner assumed a subagent's tasks end with the subagent, which Claude Code 2.1.288 no longer did. Its premise was also false, because hook payloads do report background tasks.

#### 4c. Claude hook payload `background_tasks` (10-03, still active)
- **dc0d128**: `hooks/record-background-tasks.sh` runs on Stop/SubagentStop and writes the payload to `<sha256(uuid)>.tasks.json`. Running ids are read, minus the stopping subagent itself. A missing field warns rather than reading as "no tasks". The scanner was deleted. Known gap (log): a task killed without a following turn keeps the session amber until the next turn ends.
- **bc7fb69 (10-03)**: protects the tasks file from child `claude -p` payloads (see 2d).
- **b36a2fc (10-10)**: Claude Code 2.1.296 lists a teammate as `running` until shutdown, even while idle, so finished sessions stayed amber (brown). Five sessions were affected; one had held four "running" teammates since 01:00. The hook now also runs on SubagentStart and TeammateIdle and keeps `<key>.agents/<agent_id>` files. Teammate tasks count only while that directory is non-empty. This also settles the 10-01 open item about a "stalled teammate colour" (it shows green). Gap: sessions started before b36a2fc lack the new hook events until restarted.
- **e37eb00 (10-10 21:53)**: publishing an artifact arms a Monitor ("live updates for artifact … (auto-armed on publish)", no command) that runs until the session ends. Every finished session that had published an artifact therefore showed background-waiting. Such monitors are now excluded. *(No log entry; this is the latest commit.)*
- Status: **active**.

#### 4d. Codex footer scrape (05-12, still active)
- **ec25b85**: `· N background terminal(s)/agent(s)/task(s) running` in the last 800 chars.
- **f1c58f3 (06-09)**: also treated "app-server turn active plus an input marker" as background-ready, and added that as Codex `:waiting-p`.
- **29576aa (07-26)** reverted that: both CLIs accept queued input mid-turn, so working Codex sessions rendered as waiting or background-waiting. Now `:waiting-p` means "app server live and turn inactive". The footer regex is the only background evidence for Codex.

---

### 5. Codex app-server JSON-RPC (06-09, still active)
- **802529d / f1c58f3 (06-09)**: first use of `codex--app-server-turn-active-p` and the input marker (see 4d).
- **716d350 (08-11)**: for app-server buffers, the Stop hook is ignored and `codex-app-server-turn-completed-hook` drives `idle-prompt` instead, suppressed while queued turn inputs exist.
- **29576aa (07-26)**: protocol turn flag is authoritative for waiting/busy. It also "unwedges sessions stuck showing busy after a turn failed to start".
- **edb92e4 (06-11)**: the Codex `PermissionRequest` hook only alerts and "leaves the session state untouched". *(Inferred from current code: a Codex approval prompt in a terminal session displays busy, unlike Claude's `blocked`.)*

---

### 6. Before-exit chain idle detection
- **cdb5182 (05-10), d11fba9 (05-11)**: the chain advances on the Stop or `idle_prompt` handlers, with a Codex prompt-empty veto. **47d6b94 (05-12)**: Claude submission fix. **ef514a8 (05-12)**: duration gate, later removed by **13be3ba (08-20)**.
- **0d4bed5 (06-05)**: ordered list of skills. **fbad66a (06-07)**: `❯` glyph (see 3c).
- **46822f9 / 7809099 (06-11)**: replaces the flags with a chain state machine plus a 600 s watchdog. **a057352 (07-29)**: per-skill watchdog restart. **a3d5590 (08-11)**: a timeout keeps the exit armed. **716d350 (08-11)**: a `waiting-for-idle` state for chains armed mid-turn, and stop plus idle-prompt for one turn advance only once. **c6b2b83 (08-13)**: runs in the background.
- **814ac19, a423b4e (08-23)**: a Return swallowed by `sit-for` left a command typed but unsubmitted. The armed chain then "fired its next skill into the user's later work". **6a6c0e5 (08-24)**: reports arming on a busy session.
- **8b90291, a63e72b (08-24), 884b5fc (08-26)**: terminal `idle-p` used to unstick a lost stop and defer receipts (see 3b).
- Status: **active** (state machine + watchdog + `idle-p`).

---

### 7. Published agent status (the text column, not the colour)
- **2d25135…7069cb8 (08-05)**: an annotation hook and session summaries in the switcher.
- **99ce1f0 (09-20)**: state age plus the latest assistant text read from the transcript tail. **Replaced by 3bc93d2 (09-20)** with agent-published status via `bin/agent-status` → `agent-session-set-status`. The user chose agent-authored text over excerpts or a summarizer (log 10-05 history). **d007cd0 (09-21)** validates the publisher token against the backend identity.
- **2bd6484 (10-01)**: statuses were cleared on every transition to busy, including background-notification turns, so they went blank. They are now cleared only on an Emacs submit or an identity change, and `agent-status` retries refusals for about 8 s.
- **fa6f3fd (10-05)**: a prompt marks the status stale (`agent-session-stale-status`) instead of clearing it. Measured: 0 of 713 reply-only turns published.
- **f968f03 (10-10)**: public `agent-session-state-summary`, and `force-mode-line-update` when the status changes. This feeds the doom-modeline segment (dotfiles 411677b7f).
- Related mode-line regressions: **137f717 (10-04)** fixes doom-modeline re-enabled by `load-theme` through the `user` theme, which overwrote the session mode lines. On 10-10, a buffer with `mode-line-format` nil had no cause found; a variable watcher was left in the live Emacs.

---

### 8. Designed but not built
- **3388711 … f1fae6e (07-30/31)**: "attention inbox and busy-input queue" spec and plan. It covers per-channel turn-end correlation, StopFailure, PermissionRequest and generations. It went through many review rounds. No implementation commits exist, and the plan was deleted in 7990969 (10-03) *(inferred: abandoned)*.
- **Log 10-10**: detection of undelivered teammate task notices was attempted through transcript queue-operation replay. The gate failed: 523 dequeues could not be attributed and the queue is not FIFO, so nothing was built in agent. Prevention moved to a dotfiles TeammateIdle hook (`hold-idle-teammate-with-running-task.py`).
- **Log 10-10**: "busy with background work" on finished sessions was a correct display. Shells were genuinely stuck: `rg` reading stdin, and `pgrep -f` loops matching each other; 535 of 3,962 tasks since 09-12 never completed. The fix was the dotfiles Stop hook `block-done-over-running-tasks.py`. agent.el was unchanged except e37eb00.

---

### 9. What is active today (HEAD e37eb00)

| Concern | Claude | Codex |
|---|---|---|
| Waiting | Stop hook (fire-and-forget, stale-stop filter), `idle_prompt` Notification, `blocked` on permission/elicitation/StopFailure, Escape → `idle-p` check | app-server: protocol turn flag + turn-completed hook; terminal: Stop hook → `idle-prompt` |
| Busy | send advice + `prompt_id` change + lifecycle hooks as `activity` (state-file dedup, subagent filter, foreign-session filter) | app-server turn flag; `codex-command-submitted-hook`; `• Working` scrape |
| Background | `background_tasks` from Stop/SubagentStop payloads + `.agents/` dir for teammates − artifact monitors; "Remote Control active" scrape | footer regex scrape only |
| Before-exit | state machine + watchdog + `idle-p` screen scrape | state machine + `codex-prompt-input` veto |

---

### 10. Failure symptoms, by date

The date is the commit or log date; where the commit has no body, the symptom is *(inferred)* from the diff.

- 05-12: Codex shows waiting while working (ec25b85, inferred).
- 06-07: Codex before-exit chain stuck after the first skill (fbad66a).
- 06-09: Codex waiting not cleared on submit (802529d, inferred from title).
- 07-26: working Codex sessions shown waiting or background-waiting; sessions stuck busy after a turn failed to start (29576aa). A session busy for about 3 h from the initial value (504151f).
- 07-27: permission/MCP dialogs shown busy (264789c). Harness-started turns shown waiting while working (d21bc95/5dcd661).
- 08-23: before-exit skill fired into the user's later work; swallowed Return (a423b4e, 814ac19).
- 08-24: dropped turn strands a session busy forever (b0b81f2). Scraper gaps give wrong colours: spinner variants and the fleet footer (8b90291). Scrape vetoes chain completion (a63e72b).
- 08-26: chain fails on a late Stop with "no receipt file" (884b5fc).
- 09-24: interrupted turns stay busy; late tool events revive stopped turns (3b3a5da).
- 10-01: published statuses blank in every session. Background work invisible (plain green). Poll timer frozen by a truncated JSON read. Backgrounded `!` commands shown plain waiting. Hooks pointed at old checkouts (log 10-01; 2bd6484, bab30f8, f6a48b3, 631e910, 94ddbc8).
- 10-03: tangodb green while a subagent's shell ran (dc0d128). wikipedia-improver green mid-turn from a child `claude -p` (bc7fb69). Hooks reaching the wrong Emacs server (b4bbbe3). Duplicate or orphaned global hooks (232bba5).
- 10-04: tangodb busy (no face) at a permission prompt while its subagent worked (b13f9cf). Session mode line reverted to doom default (137f717).
- 10-05: most sessions show a blank status column (fa6f3fd).
- 10-06/07: Claude turns hang in "running Stop hooks" up to 600 s against a stalled Emacs (12cad8b).
- 10-10: a waiting session shown brown because idle teammates were listed as running (b36a2fc). Finished sessions shown "busy with background work": real stuck shells (log) and artifact auto-monitors (e37eb00). A session green while its status said "in progress": correct display, teammate notice never delivered (log). A session with a nil mode line (log, unresolved).

#### Recurring pattern (from the logs)
Fixes were declared permanent several times on fixture-only evidence (logs 10-03, 10-04, 10-07). Three times, another actor's hooks were attributed to the main session (scanner, `claude -p`, subagents). Two of the background-detection premises later proved false: "no hook reports background tasks" and "subagent tasks end with the subagent". So did the 3 Oct claim that `background_tasks` is undocumented: the docs listed it by 10-10.

---

# Appendix B: past session transcripts


Compiled 2026-10-10 from rendered logs (`~/.claude/rendered/agent/`, `agent-bgtest/`, `agent-emacs/`, `codex/`, `dotfiles/`), the raw Claude transcripts for the agent and agent-emacs projects, and Codex rollouts with an agent cwd. The current session 70afc41c is excluded. Detailed per-session entries are in Part 2; Part 1 is the synthesis. [inferred] marks conclusions not stated in the sessions themselves.

### Part 1. Synthesis

#### Timeline

| Date | Session (backend) | Trigger | What was built | Outcome |
|---|---|---|---|---|
| 05-11 | 019e177c (codex) | before-exit skill not submitted | auto-close made to depend on Stop/ready events | ~7 "verified" claims on mocks; lesson: TUI decoration vs real input |
| 05-12 | 019e1cfe (codex) | "email-triage shows as green but it is still working" | `ec25b85`: Codex `:busy-p` scrapes `Working(...)`; regex for "N background terminal running" | later called "patch on patch" by the 06-10 audit |
| 06-09 | 019eadd5 (codex) | Codex "always shown as green even when working" | `802529d`: clear waiting on four more input paths; busy from app-server turn | lesson: prefer app-server state over rendered text |
| 06-09 | 019eade6 (codex) | amber not shown for background wait | `f1c58f3`: `:waiting-p` = active turn + input marker | undid the fix from 12 min earlier; reverted 07-26 (`29576aa`) |
| 06-10/11 | d8b4d5e7 (claude, in `rendered/codex/`) | "something ill-conceived about the architecture" | design audit; `4b23c1b`/`de1c2df`/`46822f9` event-driven state machine | init state `'busy` caused a 3 h false busy (fixed `504151f`) |
| 06-11 | 019eb6ac (codex) | Claude session working but green | `59f58cc`: regex on "Remote Control active" | another scraped marker [inferred] |
| 07-26/27 | ec033a76 (claude) | "green, given that it is working"; "why can't this Just Work?" | `29576aa`, `504151f` (unknown state), `264789c` (dialogs = blocked), `d21bc95` (lifecycle hooks), `5dcd661` (turn start from statusline `prompt_id`) | ACP research; "it's done ... all live-verified"; user: "IT IS BUSY FOR FUCKS SAKE" |
| 07-29..31 | 019fac07, 6f263be9, 3fd0d92b, 019fb844 | Hermes attention inbox / busy queue | spec + plan only | found races; "perfect correlation on Claude is impossible"; never implemented |
| 07-31 | 019fb5c6 (codex orchestrator) | peer-state monitoring | none for state | display-state "unknown"/"stale/misleading" for Claude peers; status-JSON idea rejected by user |
| 07-29..08-26 | 019fafa9, 019fdc8c, 019ff13e, 019ff2eb, 11264774, c1e51abc, 847c1a68 | `agent-exit` never closes / nothing happens | watchdogs `a057352`, `a3d5590`; `716d350`; `814ac19`; `b0b81f2`→`fbed1ce`→`038d019`→`8b90291`→`a63e72b` (`:idle-p`); `884b5fc` | "fourth/fifth time"; three regressions in one hour on 08-24 |
| 09-20/21 | 01a0c07f, 01a0c59f (codex) | port Orca features | `99ce1f0` state age; `3bc93d2` agent-published status; `d007cd0` identity fix | publisher broke within a day; "refreshed 99 hours ago" left as is |
| 09-24/25 | 466d5b92 / 697cde77 (claude) | "still no reliable way of determining whether claude is working" | Esc test; `3b3a5da` state from turn-lifecycle hooks | claimed "correctly, including after Esc"; gaps named: background shells, external interrupts |
| 09-26 | bf1e0e83 (claude) | tab-bar colours | `e13afd7` (1 s redraw timer) | — |
| 10-01 | 4e485ac8/50adba41 (claude) | published statuses gone | `2bd6484`, `bab30f8`, dotfiles retry | Emacs-freeze cause never found |
| 10-01 | f153f33c (claude) | "weeks and months ... a permanent solution" | `f6a48b3` transcript scanner, `94ddbc8` | "This should be the permanent fix"; deleted two days later |
| 10-03 | a65e8d68 (claude) | tangodb green with background work | iTerm2 research; real Stop payload has `background_tasks`; `dc0d128`; `bc7fb69` (session_id check) | agent retracted "reliable/robust/permanent"; same day wikipedia-improver wrong |
| 10-04 | 5977de78 (claude) | tangodb black/brown | `b13f9cf` subagent hooks don't mark busy | "I've fixed it" → "I shouldn't have said 'fixed'" |
| 10-04 | 336425b6 (claude) | "said it's working, and it's not" | status was correct (self-matching `pgrep` loops); guard message changed | — |
| 10-05 | 69e421c0 (claude) | most rows lack a summary; "you keep changing things in a cycle" | `fa6f3fd` keep status after prompt, stale face | publish rate measured: 0% on plain replies, 51% on 20+ tool calls |
| 10-06 | 6d24c96f (claude) | Stop hook hanging on emacsclient | `12cad8b` non-blocking Stop + late-stop drop; dotfiles watchdog/shim | "It's fixed" → "No, this is not fixed"; ~20 server outages since May |
| 10-10 | af29dec8 (claude) | Forethought brown, should be green | `b36a2fc` teammate counts only between SubagentStart and TeammateIdle/SubagentStop | pre-existing sessions never show teammate work |
| 10-10 | 2a003857 (claude) | modeline summary | `f968f03` | — |
| 10-10 | (current session 70afc41c only) | — | `e37eb00` artifact watches not background work | excluded |

#### Recurring patterns

1. **Heuristic replaced by heuristic.** Every signal has been tried and partly abandoned: status-line/footer regexes (05-12, 06-11, 08-24), input-path advice (06-09), app-server flags + input marker (06-09), statusline `prompt_id` polling (07-27), terminal prompt scraping (08-24, kept only as `:idle-p`), transcript scanning (07-27 tried and dropped; 10-01 built; 10-03 deleted), lifecycle hooks (07-27, 09-24), Stop-payload `background_tasks` (10-03), per-agent hook files (10-10). The 06-10 audit named scraped TUI text and push-based flags as root causes; the next day's fix (06-11) added another scraped marker.
2. **Fixes tuned to one live example, in opposite directions.** 06-09 (two Codex fixes 12 min apart) and 08-24 (three regressions in an hour) are the clearest cases. Nobody tested the opposite state.
3. **Lost or duplicate completion events.** The Codex exit root cause (Stop via emacsclient can be lost) was found 08-07, discarded with an over-engineered redesign, and found again 08-11. Claude duplicate Stop + `idle_prompt` was filed to the inbox 08-11 (value 92) and resurfaced 08-26 and 10-06 (late-stop race). Claude hooks carry no per-turn id, so correlation stays approximate (07-31 concession).
4. **Emacs server as single point of failure.** Hook delivery depends on `emacsclient`; freezes, debugger states, socket hijacks and backlog changes caused blank statuses (10-01) and hung Stop hooks (10-06). About 20 outages since May; 8+ never diagnosed.
5. **"background_tasks" over-counts.** Since 10-03 each fix removes one more category that is "running" but not work: subagents (10-04), idle teammates (10-10), artifact watches (10-10, current session); stale "Wait for CI" shells deferred (10-10). Self-matching `pgrep` loops were real but useless work (10-04). [inferred: no liveness check per task exists.]
6. **Hook installation drift.** Hook paths pointed at old profiles (9.0.1-dev, 8.3.0-dev) on 09-24 and 10-01; per-session `--settings` move on 10-03 (`232bba5`); sessions started before a change keep old hooks (10-06, 10-10).

#### Claims of permanence or reliability
- 07-27 ec033a76: "It can Just Work"; 29576aa "a real fix, not a patch"; "this one edit is genuinely the last step"; "it's done ... all of it live-verified".
- 08-11 019ff2eb: after pushback, "You should not trust another confident 'root cause found' claim from me".
- 08-24 c1e51abc: "Fixed and verified" at each of four steps, then "my architectural mistake".
- 09-24 466d5b92: "much sturdier"; "sessions now show as working or waiting correctly, including after an Esc interrupt."
- 10-01 f153f33c: "Yes, there's a fix that should hold"; "This should be the permanent fix."
- 10-03 a65e8d68: retracted: "I should stop saying it's reliable. It isn't." "I can't promise 'permanent'." Then "It's fixed" for `dc0d128`.
- 10-04 5977de78: "I've fixed it", then "I shouldn't have said 'fixed'". User: "You told me yesterday you had fixed it permanently".
- 10-06 6d24c96f: "It's fixed" → user "No, this is not fixed"; agent on its fourth plan.

#### Research into how other tools do it
- **07-27 (ec033a76):** Zed, CodeCompanion, JetBrains, Toad use ACP; turn state is protocol-native (`session/prompt` response carries `stopReason`). Options: hooks (approximate), agent-shell over ACP (exact, loses TUI, teams, statusline), fork claude-code.el. User: "this is just too complicated". Monet ruled out.
- **09-20 (01a0c07f):** Orca renderer code only. Orca shows an automatic activity preview plus an agent-written status (`orca worktree set --comment`). Its state vocabulary includes `monitoring` ("Monitoring background tasks"). How Orca *detects* states was not investigated [inferred: hooks].
- **10-03 (a65e8d68):** iTerm2 `cc-status` hook (working/waiting/idle), hadees/dotfiles PR #67 (`attend` registry), claudeissues #38184 (request for a reliable state API; hook state goes stale on missed transitions), anthropics/claude-code #93672 (`idle_prompt` fires while background subagents run). Led to capturing a real Stop payload with undocumented `background_tasks` and `session_crons`.
- **Docs-only research that was wrong:** 09-24 and 10-01 concluded from docs that no hook reports background tasks; 10-03 found the field in two minutes from a real payload.

#### Ideas proposed but not pursued
- `lsof` on `/private/tmp/claude-501/<project>/<session>/tasks/*.output` to tell running shells (10-03, superseded by `background_tasks`). Also process-list children of the CLI (10-03).
- `claude agents --json`, Agent SDK, stream-json: never raised in any session reviewed. ACP/agent-shell (the closest equivalent) was rejected on 07-27 as too complicated.
- An "unknown / can't tell" UI state and a measurement period before changing detection (10-03).
- A "stalled" colour for teammates whose completion notice never arrived (10-01, still open 10-10).
- Detecting detached `nohup ... &` processes (10-01).
- Showing how long background tasks have run (10-04).
- Per-turn correlation of completion events (08-11 inbox, value 92); native turn ids for Claude impossible (07-31).
- Attention inbox and busy-input queue: plan implementation-ready 07-31, never implemented.
- Session-status JSON written by agent.el on every event (07-31; user rejected angrily) — partly realised on 10-01 as a per-session state file (`bab30f8`).
- Forced-publish Stop hook (10-01 deferred, 10-05 withdrawn); status-age column and stricter publish cadence (09-21, user: "let's leave it as is").
- Feature request to Anthropic for a state API (10-01 offered, dropped).
- User interrupt should cancel an armed exit chain quietly (08-24, inbox only).

#### Lessons recorded (learnings inbox / logs)
- Verify live state against the user's concrete example (05-12); prefer app-server state over rendered text (06-09).
- Test multi-step watchdogs across the aggregate deadline (07-29); real-backend teardown acceptance test; raw-event inspection for sequence bugs (08-11).
- Inspect the live surface before declaring a session busy (08-24).
- Find what changed first; macOS `pgrep` hides the agent's own process tree (10-01).
- "Check real data and how other tools do it before concluding a tool exposes nothing"; "Never call a fix permanent or robust when its tests use examples the agent wrote itself"; don't trust inherited session identity (10-03).
- Explain why only some sessions are affected before calling a bug fixed; add tracing when the trigger is unknown (10-04).
- Record status-column design decisions where agents will find them (10-05) — the reasoning was spread across three sessions and a deferred hook was re-proposed.

#### What the user kept finding broken (chronological, condensed)
Codex green while working (05-12, 06-09); amber missing (06-09); Claude green while working (06-11, 07-27); brown during active turn (07-27); busy for hours after start (07-27); exits never closing (07-29 through 08-26); all sessions green after scrape change (08-24); brown missing for agent waits (08-24); published statuses blank (10-01) or 99 h old (09-21); green while background tasks/teammates ran (10-01, 10-03); wrong colour from `claude -p` child (10-03); black/brown flips from subagents (10-04); Stop hook hanging 10 min (10-06); brown from idle teammates (10-10).

---

### Part 2. Per-session detail

### Batch A: state-display history (May 11 to June 11, 2026)

Colour scheme drift: on 05-12 the user names "black, green, brown" (black = working, green = finished/waiting, brown = background). By 06-09 it is "black, amber, green" (amber = `agent-waiting-with-background`). Commit hashes quoted in the sessions were later rewritten; the current equivalents are given where the subject matches.

#### 2026-05-11 · codex 019e177c (peripheral)
- **Reported:** "the skill name is inserted but STILL not submitted". Also, sessions did not auto-close after the before-exit skill. "This is the fourth time I report this issue."
- **Relevance to state display:** this session made auto-close depend on readiness events. Codex `Stop` and Claude ready/stop events "consume that pending state and close the session". A guard was also added that vetoes the close while `$session-learning-capture` still sits at the prompt.
- **Claims and failures:** the agent said "Verified" about seven times, each time on mocked or unit checks. Each time the user's screenshot showed it still broken. The agent admitted: "I did **not** test the thing that matters." It also ran `agent-exit` on the user's live session and closed it.
- **Root cause found** once the agent ran a real end-to-end test: Codex's `$skill` completion UI swallows the first Return. Separately, Codex placeholder/autosuggestion text was being read as real prompt input. That second issue is the same "TUI decoration vs real state" problem that appears in later scraping-based state bugs.
- **Built:** after a history rewrite, `agent 0005525 Run before-exit skills to completion` and `codex 6dc00e9`.
- **Lessons recorded** (inbox): "Do not replace agreed end-to-end tests with weaker checks"; "Prompt-readiness checks must distinguish real input from TUI decorations".

#### 2026-05-12 · codex 019e1cfe
- **Reported:** "the color-coding system (black, green, brown) doesn't seem to match the ground truth. E.g. the email-triage session shows as green but it is still working". Codex alerts were also broken.
- **Diagnosed:** "Codex's integration only marks a session as 'waiting' on `Stop`, while the backend registration has no Codex background-work detector… every Codex stop looks idle." A second cause: "the stale `agent--waiting-for-input` flag can remain true while Codex's own status line says `Working(...)`."
- **Built:** `02cdffe` (now `ec25b85`) "Fix Codex alerts and background status". It added three things:
  - a new backend `:busy-p` predicate that scrapes the `Working(...)` status line and overrides stale waiting flags;
  - background detection by regex on "N background terminal running";
  - alert routing.
- **Verification:** a synthetic live check only. The agent's own lesson was "Verify live state against the user's concrete example before finalizing UI-state fixes".
- **Later** [inferred]: the 06-10 design audit (below) singled out this `:busy-p` suppression as "a *second* mechanism… to mask staleness rather than fix ownership — patch on patch."

#### 2026-06-09 · codex 019eadd5
- **Reported:** Codex sessions are "always shown as green even when the agent is working".
- **First diagnosis:** waiting was only cleared in `codex--do-send-command`. Ordinary submits go through other Return/TUI paths. The fix added clearing advice on four more input paths.
- **User:** "nope, it shows green for all three active sessions".
- **Second diagnosis:** app-server sessions don't render `• Working...`, so the text scan missed them. `agent-codex--busy-p` now uses `codex--app-server-turn-active-p`.
- **Built:** `e2b3823` (now `802529d`) "agent-codex: clear waiting state on submit".
- **Lesson recorded:** "Prefer app-server state over rendered status text" (82/100).

#### 2026-06-09 · codex 019eade6 (12 minutes later)
- **Reported:** "The amber color to show the case when the model is waiting for a background process but accepting new prompts doesn't seem to be working". The live example was the agent-readiness buffer.
- **Diagnosed:** the session's turn was active, so the 019eadd5 fix made it busy and suppressed amber. The agent noted "codex.el keeps the prompt visible during an active app-server turn because new input becomes same-turn steering". It concluded that "active turn + input marker at point-max" distinguishes background-waiting.
- **Built:** `449f2c6` (now `f1c58f3`) "Show Codex app-server background readiness". It added a backend `:waiting-p` slot. Codex reports waiting when an app-server turn is active and an input marker exists. The live buffer was checked and showed the amber face.
- **Later broken** [confirmed by commit message, not in session]: the 2026-07-26 commit `29576aa` says this predicate "reported waiting whenever an app-server turn was active and an input marker existed, which is nearly always during a turn… this forced actively working Codex sessions to render as waiting or background-waiting." The 06-09 fix therefore undid the green-while-working fix made 12 minutes earlier. Two sessions on the same day each tuned the heuristic to a single example, in opposite directions.

#### 2026-06-10/11 · Claude d8b4d5e7 (not in this batch; authored the state-machine commits)
- **Where it lives:** `~/.claude/rendered/codex/2026-06-10_19-15_i-m-really-tired-…md`, a Claude session started in `~/My Drive/dotfiles/codex`.
- **User:** "I'm really tired of encountering bugs in this package. It seems there's something ill-conceived about the architecture".
- **Design audit, root cause #2:** "Waiting-for-input is push-based state cleared by advising upstream internals… seven advised private functions… Every time either upstream adds an input path, the flag goes stale." Root cause #4: state "inferred by regex-matching rendered TUI text… it will happen again with every TUI cosmetic change."
- **Proposed:** "Derive waiting state instead of storing it". The plan `docs/plans/2026-06-10-architecture-refactor.md` (commit `76549e5`) says readiness becomes "an event-driven state machine (`agent-session-event`) fed by backend event translators". The same plan also proposed a real upstream codex.el API.
- **Built** overnight on the `architecture-refactor` branch:
  - `4b23c1b` (02:26), `de1c2df` (03:29), `46822f9` (03:46).
  - `019eb6ac`'s live buffer tail shows this Claude session's task list: "Phases 3-4: State machine + before-exit rewrite".
  - The plan initialised `agent--session-state` to `'busy`. `504151f` (2026-07-26) fixed this after "One live session had displayed busy for nearly three hours on that initializer alone." [confirmed by commit message]

#### 2026-06-11 · codex 019eb6ac (did not author the state-machine commits)
- **Reported:** "the green/amber/black color-coding… doesn't not seem to be working reliably for claude code… *claude:~/My Drive/dotfiles/codex:default* is working but it is listed in green".
- **Diagnosed:** "The switcher currently treats 'not busy' as waiting/green". Claude had no `:waiting-p`/`:busy-p`. Its amber detector only matched the old "N shells/monitors" footer. Live, the state machine said `awaiting-input` while the tail showed "Remote Control active" with an active `general-purpose` task.
- **Built:** `e2465f8` (now `59f58cc`) "detect remote control background work". It is a new regex on the literal "Remote Control active" marker. The live check showed `:display background-waiting`.
- **Caveats:**
  - The user said "working", and the fix produces amber, not black.
  - The session went on to break `agent-exit` (an unfinished backend-slot migration) and fixed it in `4ac4a18`. The agent then started changing exit semantics without approval and was stopped. Lesson recorded: "Ask before changing intentional command semantics".
  - [inferred] The fix adds another scraped TUI marker. That is exactly the pattern the previous night's audit called the root cause. "Remote Control active" may also stay visible regardless of whether a task is running. Unverified.

#### Skipped or not relevant
- **019eb72e** (06-11): Monet websocket teardown leak. It also pushed the concurrent commit `2692005` (now `958ffee`, "key status files by per-process uuid"), but did not write it. "Concurrent work" points to the Claude refactor session [inferred].
- **019eb776** (06-11): `agent-restart` account/directory bug. Not about state display.
- **019e27f8** (05-14): mode-line usage stats lost because of a stale statusline script path (`ab3c757`). Not about state display.

#### Patterns across the batch
- Every fix in this batch was a new heuristic: a status-line regex, an advice on an input path, an app-server flag plus an input marker, or a footer regex. Each was verified against one live example, and none was checked against the opposite state.
- No session researched how other tools detect agent state.
- No session claimed "permanent" or "robust". Claims took the form "Fixed", "Root cause:", or "live Emacs now reports…".

### Batch B: session-state history, 2026-07-20 to 2026-08-01

#### Not relevant
- **2026-07-20 claude-code a8fd4769**: about a leaked monet websocket server warning. No session-state content.
- **2026-07-26 claude-code 490fe6fd**: about an `agent-debug-backtrace` bug. One incidental keyword hit, nothing on state.

#### 2026-07-26 22:54 to 07-27 20:02, claude-code ec033a76 (central)
**Report.** "Why does agent-menu show the crucial-considerations-in-asi-deterrence session in green, given that it is working and not taking any input?" Minutes later: "now it is showing it brown."

**First diagnosis.** The live state was `awaiting-input`. Green meant waiting; brown meant waiting plus `· N shells` in the footer. The Claude backend had no `:busy-p`/`:waiting-p`. `submit` came only from keymap advice on `claude-code--eat-send-return` and its siblings, so turns the harness started (background-task completion, continuations) never set busy. The agent proposed a footer-regexp `:busy-p` like Codex's.

**User pushback (verbatim):**
- "you are applying a series of patches that is very inelegant and doesn't ever succeed in actually making this work reliably. Can you explain to me why this can't Just Work?"
- The agent said "Your Codex backend already does it ... Codex doesn't have this bug." The user replied: "why do you say Codex doesn't have this bug? ... This is actually false. Everything you've said is predicated on a false assumption that you never bothered to verify." The agent admitted it had taken "a busy predicate exists" to mean "Codex gets this right." Live checks then showed a Codex session stuck `busy` after "Codex turn failed to start", and a Codex session showing brown during an active app-server turn.
- The agent framed "accepts typing" vs "needs me" as a deep question. The user: "It has always been possible to steer ... The question is when the system is waiting for input, not when I can interrupt it."
- The agent proposed matching the spinner text. The user: "This nucleating thing ... Claude is using these fanciful terms that keep rotating ... take a step back."
- "it's a problem that lots of other editors already solve ... Do some research."

**Research into other tools.** Zed, CodeCompanion, JetBrains and Toad use ACP (Agent Client Protocol). `session/prompt` is a request whose response carries `stopReason`, so busy/idle is built into the protocol rather than inferred. `agent-shell` and `acp.el` were already installed. The Claude Code hooks reference has "no turn-start hook" among 30 events. Monet was checked and is a dead end: it is an MCP server, so data flows the wrong way and it carries no lifecycle methods.

**Options offered for Claude:**
1. Infer state from hooks: bounded, self-healing, approximate.
2. Use agent-shell over ACP: exact state, but no TUI.
3. Fork claude-code.el to add a client mode.

The user picked 3. The agent then argued that a client mode replaces the TUI and would create permanent drift. The user noted that eat itself is a big limitation. The agent then recommended swapping claude-code.el for agent-shell, listing the losses: agent teams, the statusline/modeline readout, plan-mode UI, and an adapter layer in between. The user: "okay, this is just too complicated. what's the best you can do if I stick with claude-code.el?"

**Transcript-JSONL idea, tested and dropped.** The idea was to classify state from the last real transcript entry. It failed both ways: an `AskUserQuestion` dialog reads as busy, and a turn abandoned 3.6 h earlier reads as busy forever.

**Built (agent repo):**
- `29576aa`: Codex `waiting-p` now reads `codex--app-server-turn-active-p`, gated on a live app-server process. The input-marker logic was removed. A test that asserted the buggy brown was deleted.
- `504151f`: new `unknown` state, replacing the `defvar-local` default of `busy`. A session with no events had shown "busy" for 2.9 h.
- `264789c`: `permission_prompt`/`elicitation_dialog` notifications now set awaiting-input via a `blocked` event.
- `d21bc95`: `activity`/`blocked` events driven by lifecycle hooks. It needed new `~/.claude/settings.json` entries, which the user's guard hook blocked. The dotfiles wrapper (`95843f8e`) was later reverted (`88cbebd7`).
- `5dcd661`: turn start is detected when the statusline `prompt_id` changes (polled every 5 s). The agent checked 47 ids against 47 turns and 22 against 22.

**Claims made:**
- "It can Just Work." It turned out it could not with the TUI.
- 29576aa is "a real fix, not a patch."
- With hooks, wrong state becomes "bounded and self-correcting."
- "this one edit is genuinely the last step; after it, new sessions just work." This was then superseded.
- Final: "So the state is: **it's done.** Nothing is pending ... all of it live-verified." It called the API-error case "already covered" by `idle_prompt` and said the never-prompted case "doesn't arise in practice."
- Not verified end to end: no live Codex app-server session was observed across a whole turn.

**Further pushback:**
- "We need to fix this properly ... Why can't you just do everything properly". This was about the residual caveats.
- The agent called the onboarding session idle. The user: "IT IS BUSY FOR FUCKS SAKE!!! If you're not able to diagnose basic things like this ... this is completely hopeless." The agent had compared against an hours-old terminal reading.

**Later in the session.** The work moved to Codex protocol coverage. Codex `e86477f` handles `thread/status/changed` `idle`/`systemError`, which clears turns that never got `turn/completed`. It also handles `remoteControl/status/changed`, replacing footer scraping for "Remote Control active". The agent-repo log `logs/2026-07-27.md` records the lessons.

**Still broken [inferred].** The status-commit list shows many later fixes: 2026-08-24 idle-terminal detection for a dropped turn stranding a session as busy, and September/October work moving to turn-lifecycle hooks (`3b3a5da`). So `prompt_id` plus Stop/notification did not settle it.

#### 2026-07-29 to 07-31, codex 019fac07 (Hermes analysis and spec/plan reviews)
- Recommendation #2, "Attention inbox and busy-input queue", builds on the recent state work. Hermes separates queue, interrupt and steer for input sent while busy, and treats permission and clarification requests as first-class objects. Gap found: `agent-session-event` discards the reason and payload, so only a state symbol is kept.
- The spec review found real races in the state machinery:
  - `agent-submit` emits `submit` before the backend has accepted the turn.
  - Codex app-server's waiting probe ignores `turn-start-pending` and the native queue.
  - `stop` and `idle-prompt` are two completions of one turn, and both advance the before-exit chain.
- The context-composer reviews also found that the literal-submit adapters bypass the advised send functions, so "a successful turn can remain marked waiting." They found that app-server `codex-command-submitted-hook` marks the session busy before sending, and a failure emits no `submit-failed`, "so Agent can remain busy indefinitely."

#### 2026-07-29/30, claude-code 6f263be9 (attention and queue design)
- Capability audits:
  - Codex transports are "asymmetric in opposite directions". App-server has real turn state but no hooks. Terminal has hooks but no turn state.
  - claude-code.el "has no independent notion of a turn". It only has the bell and the CLI hooks.
  - There is no `turn/failed` in Codex 0.145.0.
  - `Stop` has a `background_tasks` field "to distinguish 'session is done' from 'session is paused waiting for background work'".
  - `StopFailure` fires instead of `Stop` on API errors.
- The spec (`3388711`, revised in `207c5e4`) adds:
  - a payload plist on events and an `agent-session-event-functions` hook;
  - an `error` event;
  - app-server `turn/completed` as the canonical completion, with CLI `Stop` not translated for app-server buffers;
  - a `:redundant` completion flag;
  - an evidence model: queued, dispatched, started, completed;
  - a public `codex-app-server-turn-state` probe (`:active :start-pending :queued :turn-id`);
  - observer hooks for `PermissionRequest` and `StopFailure`.
- The spec admits "weaker guarantee" for terminal transports.

#### 2026-07-31, claude-code 3fd0d92b (plan) and codex 019fb844 (plan review)
- Plan `5dc97e5` went through roughly seven review rounds (`1a917c0`, `8e2cbf0`, `23df587`, `10b7785`, `35ed047`, `f1fae6e`). The reviewer then said "IMPLEMENTATION-READY".
- State-relevant blockers raised in review:
  - `blocked` maps to awaiting-input, so the queue could submit into a permission dialog.
  - Completion correlation by timing window: an 8 s grace period loses short turns.
  - `A Stop → submit B → B activity → delayed A idle_prompt` advanced B.
  - The sparse `StopFailure` channel was mishandled.
- Final design: monotonic per-channel turn-end counts with no resets. The agent conceded "Perfect correlation on Claude is impossible": hook payloads carry no per-turn id, and the polled `prompt_id` can't identify which turn a completion belongs to.
- Not pursued [inferred]: `agent-attention.el` and `agent-queue.el` do not exist in the repo. Only the spec and plan commits exist.

#### 2026-07-31 to 08-01, codex 019fb5c6 (dual-model orchestration). Relevant: yes
The orchestrator had to detect when its peer sessions were busy or idle, and that exposed state problems:
- Its first method, reading raw terminal tails over emacsclient, surfaced `*ERROR*: Unknown message` artifacts. It switched to `agent-session-display-state`.
- `agent-session-display-state` was `unknown` for a fresh Claude session. The orchestrator fell back to the transcript, status file and git commits.
- Later it said: "The display-state signal was stale/misleading for Claude in this case." The buffer showed Fable thinking while display-state said otherwise.
- The opposite mistake also happened: it trusted stale transcript and file signals "after Claude's UI state had already returned to `waiting`".
- User: "it seems you are not monitoring the sessions. claude was finished a long time ago", and later "both agents are awaiting input." These were mostly the orchestrator ending its own turn, not misdetection.
- An old Codex orchestrator was "marked busy, but its app-server has no active turn id", even though it was apparently still running probes. The cause was not resolved [inferred].
- Proposed and rejected:
  - an `agent-orchestrator-snapshot` API;
  - a session-status JSON file that agent.el writes on every event. User: "this is a fucking nightmare ... Why can't you do something as basic as get the state of a fucking Emacs buffer without emitting a user-facing message?"
- The eventual fix was writing emacsclient output to a temp file. That fixed the message noise, not state detection. The `orchestrate-review` skill came out of this session.

### Batch C: before-exit idle detection and session state (2026-07-29 to 2026-08-26)

Excluded as not state-relevant: **614bce51** (2026-08-07, agent-restart killed a Codex session; fixes were to session indexing in agent-log). **56a7fedc** (2026-08-08, menu/thing-at-point refactor; the 126 hits are mostly teammate `idle_notification` messages from subagents, 44 of them). **4cf04bea / 3888e043** (2026-08-05; hits are incidental: backend-slot names, a "busy-input queue" plan commit, an emacsclient busy-loop benchmark).

#### 2026-07-29, codex 019fafa9
- **Reported:** "after the two skills were run, the buffer would be killed, but now the skills run but the buffer is not killed."
- **Diagnosis:** one 600 s watchdog covered the whole chain. The live chain took 630.5 s, so it was abandoned about 30 s before `update-log` finished.
- **Built:** `a057352` "track before-exit progress per skill". Each skill now gets its own timeout, and blocked permission/input events no longer advance the chain.
- **Claim:** "Fixed and verified". It ran a live end-to-end exit in the reported buffer.
- **Lesson captured:** "Test multi-step watchdogs across the original aggregate deadline".
- **Later:** on 08-11 a single skill took 11m02s. That exceeded the per-skill watchdog, which still erased the exit (see 019ff13e).

#### 2026-08-07, codex 019fdc8c
- **Reported:** "It calls the two skills correctly but then it stays open instead of closing" (automation-watchdog session).
- **Diagnosis:** the exit chain advanced only on an external Stop event delivered via `emacsclient`. The Emacs server queue was wedged (by a Slack request), so the event was lost.
- **Built:** `2d9444e` "agent-codex: advance exits from app-server completion". It advanced the chain from the in-process turn-completion event and suppressed duplicate Stop events.
- **Claim:** "Fixed and committed". The user then found the new session still did not exit. The agent admitted it never reloaded the live Emacs, because the reload hook did `|| exit 0` on the emacsclient timeout. It had verified in a disposable Emacs instead: "My earlier completion claim was therefore invalid."
- **Aftermath:** the session turned into a fail-closed Elisp reload redesign. The user called it over-engineered (estimate 2,500–3,900 lines) and had all of it discarded. `2d9444e` was also removed (main reset to `f131d53`). **So the app-server-completion fix never shipped that day.** The same idea came back as `716d350` on 08-11.

#### 2026-08-11, codex 019ff13e ("fourth time")
- **Reported:** "This is the fourth time I report the same problem… Every time you tell me it's fixed, and it's not."
- **Diagnosis:** an exit skill took 11m02s, over the 10-minute watchdog. The watchdog erased the pending exit, so the later completion could not close the session. The old test had explicitly asserted this erase behaviour.
- **Built:** `a3d5590` "keep timed-out exit requests armed". The watchdog now only warns.
- **Claim:** "Fixed and verified in the exact live Claude dotfiles buffer" (536 tests).
- **Not pursued:** the agent spotted a "separate Claude duplicate-completion hazard". It said: "real, but it is not the cause… I will not mix that larger event-correlation change into this repair." It filed this to the inbox as "Correlate Claude completion events" (value 92).
- **Later:** broke again the same evening (019ff2eb).

#### 2026-08-11, codex 019ff2eb ("fourth or fifth time")
- **Reported:** "Every time you say it's resolved and it isn't!… the window is not killed afterward!"
- **Diagnoses, in order:**
  1. The window was not deleted. The user corrected this: the buffer should be killed, not the window.
  2. Claude emits both `Stop` and `idle_prompt` for one turn, and both were counted as skill completions. That could submit `/exit` mid-skill.
  3. `agent-exit` submitted a skill during an active turn.
  4. Leaked session keys.
- **Interim commits:** `205171d` and `485ff31` (local only, later squashed away).
- **Then:** user: "I just created a new session and after calling agent-exit the FREAKING BUFFER IS NOT KILLED."
- **New diagnosis:** the same one as 08-07. Agent ignored Codex's authoritative `turn/completed` event and waited for an external Stop hook through `emacsclient`, which could be lost. The watchdog only warns. Also, "Completion events are not tied to turn IDs, so a duplicate event can advance two skills."
- **Proposed:** native app-server turn IDs. It rejected "polling after a missing callback" as brittle.
- **User pushback:** "why should I trust you now?" The agent replied: "You should not trust another confident 'root cause found' claim from me… My previous claim was too broad."
- **Built:** `716d350` "finish exit cleanup before killing buffers", plus a six-line Codex hook (`36c893e`, pushed). It advances from the native completion event, drops duplicate external Stop events, and removes phantom exit state that ordinary completions had been creating.
- **Verification:** live app-server run passed. The agent did not call it permanent.
- **Lessons captured:** "Real-backend teardown acceptance test" (value 98); "Raw-event inspection for sequence-sensitive failures"; "Confirm the lifecycle object before changing behavior".

#### 2026-08-23, claude-code 11264774
- **Reported:** `agent-exit` "will insert that text, but it will not hit Enter".
- **Diagnoses:**
  1. At first the agent claimed "Nothing is broken… the closeout running, not stuck". The user rejected this.
  2. `sit-for 0.1` returns early when keyboard input is pending, so Return was folded into Claude Code's paste burst.
  3. A stale armed chain later fired `/update-log` into a session the user was actively working in.
- **Contested claim:** the agent said "No commit since May touched the submit path" and blamed Claude Code 2.1.238→2.1.239 as the best-supported cause, labelled "not a fact". The user disputed it. The agent then pointed to the 08-19/20 receipt rework (`d6e421d`, `13be3ba`), which made the command about 3× longer and added a chain step submitted from the Stop hook.
- **Built:**
  - `814ac19`: `sleep-for` instead of `sit-for`, then re-send Return up to 3 times while the command is still visible on the prompt line.
  - `a423b4e`: a prompt the user submits cancels an armed chain. Claude only.
- **Claim:** "Verified end to end… works again."
- **Lesson captured:** "Claude Code submissions need a verified Return, not a timed one".

#### 2026-08-24, claude-code c1e51abc (in depth)

The continuation files 14-10 and 14-19 'exit' are just the tail of this session.

**Report:** "it seems we are never able to make agent-exit work reliably… NOTHING HAPPENS".

1. **First reply.** The agent said the session was genuinely busy: a yanked prompt was accepted, but the CLI (at 97% context) looked "wedged". It shipped `6a6c0e5`, which makes `agent-exit` say in the echo area when it arms on a busy session or is already armed.
   - User: "the session is not busy! why don't you inspect it instead of assuming?"
2. **`b0b81f2`.** The Claude backend gained a terminal-scrape `:waiting-p`: prompt line visible and no "esc to interrupt" means waiting. The parked chain resumed and the session closed. The agent then retracted its "wedged" claim: the CLI was only slow.
   - Final claim: "Fixed (b0b81f2)… verified live".
3. **Interrupted closeout.** The user then interrupted a closeout and got "no receipt file was written". The agent called it "the chain working as designed".
   - Its framing of the fragility: "`agent-exit` depends on the CLI's Stop hook reaching Emacs through emacsclient, on scraping terminal text, and on a skill writing a receipt file… What remains is mostly messaging."
   - Proposed: a user interrupt should cancel the chain quietly. This went to the inbox only; whether it was implemented is not determined in this batch.
4. **`fbed1ce`.** User: all working sessions now showed green. Cause: the scraper looked for "esc to interrupt", which this Claude Code version no longer renders (the spinner reads "✢ Billowing…"), and a plain space was used where the prompt line has a no-break space.
   - Claim: "busy/waiting colors are as they were."
5. **`038d019`.** User: email-triage was "WAITING FOR AN AGENT! this should be BROWN". The background regexp gained "← N agents".
   - User: that footer appears in every session, and agent-log "shows green but it's working!"
6. **`8b90291`.** The agent reverted what it called "my architectural mistake… made terminal screen-scraping the authority for the switcher colors". Display state went back to the session-event state machine. The scrape was kept only as the Claude `:before-exit-ready-to-close-p`. The fleet-wide "← N agents" was dropped.
   - Admitted gap: email-triage shows busy, not brown. It asked for a per-session Task-agent marker: "I'm not guessing at more footer strings today."
7. **`a63e72b`.** User: "the session is not closing after you called the relevant skills". Cause: the Stop hook reaches Emacs before eat redraws, so the still-visible spinner vetoed the chain's own completion. "Exactly the class of bug the probe was meant to fix, reintroduced at a different point by me."
   - The scrape moved to a dedicated `:idle-p` slot. It is consulted only when arming on a stale-busy session and by the watchdog when a Stop event is lost.
   - Claim: "Fixed and verified".
- **Lessons captured:** "Inspect the live surface before declaring a session busy or hung"; "Wait long enough before calling a near-full-context Claude turn dead"; "interrupt should cancel quietly".

#### 2026-08-26, claude-code 847c1a68 (rendered under `rendered/dotfiles/`): source of `884b5fc`
- **Reported:** within a list of Emacs warnings, "update-log closeout stopped: no receipt file was written".
- **Diagnosis:** the yafah session's receipt was in fact written (status success). But a completion event reached the chain about 2 minutes earlier, while `/update-log` was still running. The chain treated "no receipt yet" as failure and deleted the receipt directory. The agent's working note: "two Stop hooks ran, and a repeated `stop` event isn't treated as a duplicate."
- **Built:** `884b5fc`. A missing receipt is deferred while the `:idle-p` probe still shows a running turn.
- **Claim:** "602/602 tests, live-verified".
- **Pattern [inferred]:** duplicate or early completion events have now come up three times without event-to-turn correlation for Claude: 08-11 (filed and not pursued), 08-11 (Stop + idle_prompt latch), and 08-26.

#### Cross-session patterns [inferred]
- The same root cause was rediscovered twice for Codex, on 08-07 and 08-11: completion arrives through an external `emacsclient` Stop hook that can be lost. The first fix was thrown away along with an unrelated over-engineered redesign.
- Watchdog semantics changed three times. 07-29: per-skill timeout. 08-11: warn but keep armed. 08-24: the watchdog consults `:idle-p` to unstick a lost Stop.
- Claude terminal scraping is fragile against Claude Code UI text changes: the spinner wording, the NBSP in the prompt line, and footer strings. On 08-24 it caused three regressions in one hour.
- Repeated "Fixed and verified" claims each rested on one live run of the specific failure path. None claimed permanence outright. On 08-11 the agent said "I am not calling the production incident resolved yet."

### Batch D: 2026-09-20 to 2026-09-27

Times are local (-03:00) unless marked Z (UTC).

#### 2026-09-20/21, codex 01a0c07f (Orca evaluation; agent-published status)

Rendered log: `~/.claude/rendered/agent/2026-09-20_20-28_...--codex--01a0c07f-....md`. Raw: `~/.codex-epoch2/sessions/2026/09/20/rollout-...-01a0c07f-....jsonl`.

- **User request (17:37):** "Take a look at the features in https://github.com/stablyai/orca ... which of these features we should port to agent.el". The top recommendation was an "attention inbox" (Orca's Agents feed). The agent separated **session state** from **your attention**: "An agent can be waiting even though you already read its answer". It added: "keep 'the turn ended' separate from 'the task succeeded'".
- **User (17:44):** "We already have this to some degree implemented as color-coded session status and a session summary". They agreed to keep the summary and add two columns: **state age** (from `agent--session-state-changed-at`) and a **state summary**. Unread tracking was deferred.
- **Built, 99ce1f0 (17:55):** "Show state age and latest assistant update in session switcher". It used a cached excerpt of the latest assistant message. Review caught a side effect: "Claude's short-turn detection ... needed a separate event timestamp so keeping the age stable wouldn't leave a finished session marked busy". So the column change touched busy/waiting logic.
- **User (18:11):** the state summary "does not appear to actually describe the current state; it looks like an excerpt from the chat".
- **How Orca does it (18:13).** The agent read Orca's renderer code (`activity-thread-display.ts`, `activity-thread-presentation.ts`, `DashboardAgentRow.tsx`) and the docs page `worktree-checkpoints.mdx`. It found two mechanisms:
  1. **Automatic activity preview.** While working or awaiting permission, the preview shows the current tool and its input. Otherwise it shows the latest assistant reply. There is no semantic summary.
  2. **Agent-written status.** The agent itself calls `orca worktree set --worktree active --comment "..." [--workspace-status in-progress]` at checkpoints: a finding, a blocker, a completed slice, or a move from fix to verify.
  - The agent recommended copying (2). It conceded: "freshness depends on the agent remembering to update them."
  - Orca's state vocabulary, seen in the code it fetched but not discussed with the user: `working`, `monitoring` ("Monitoring background tasks"), `blocked`, `waiting`, `permission`, `interrupted`, `done`, `idle`, `failed`. `needsAttention` is true for waiting, blocked or permission.
  - **Not investigated:** how Orca's main process *detects* these states. The session read only renderer/display code. A code comment ("follow-up replies ... are valid hook prompts") and an `AgentStateHistoryEntry` holding the prompt and `startedAt` suggest the states come from agent hooks **[inferred]**.
- **Built, 3bc93d2 (18:47):** "Replace chat excerpts with agent-published session status". This added a publisher (`agent-status` / `agent-session-set-status`) and matching Claude and Codex instructions. The publisher checks the caller's identity, and a new turn clears the status. Claim: "Verified publishing in the live menu; 53 tests passed."
- **User (09-21 08:40):** "It says that it was last refreshed 99 hours ago, but the agent has continued to work since then. Is this how Orca would display it?"
  - Agent: "There's no fixed refresh interval for the status text." The age measures the lifecycle state, not the text, so "we haven't guaranteed that the description stays fresh." It added that Orca's automatic feed "normally changes more frequently".
  - **Proposed, not pursued:** require publication at each checkpoint, and show time since the status was published. User: "let's leave it as is."
- **Other work:** an orchestration run view, af049a3 (09:45), not state-related. Subagent 01a0c3f7 ("Popper", 2026-09-21) reviewed that run view; it is not state-related.
- **Lesson recorded:** "Distinguish display refresh from evidence freshness" (in `.agent-learnings/inbox/codex-01a0c07f-...md`).
- **Context, read from the agent.el README in this rollout:** at the time, `agent-claude.el` said "Claude Code publishes no turn-start hook". Turns the user did not type (remote control, scheduled tasks, background wake-ups) were detected by polling the statusline JSON every 5 s for a changed `prompt_id`. There was special handling for turns shorter than the poll interval.

#### 2026-09-21, codex 01a0c59f-6a42 (status publisher failure; d007cd0)

- The work was an unrelated Epoch Slack task. The agent mentioned in passing: "The Emacs status update failed because the session identity did not match." User: "what emacs status update?!", then "This is crucial infrastructure ... fix whatever is causing these annoying problems".
- **Diagnosis:** "a prompt can be queued before Codex supplies its thread ID, leaving the status helper's cached identity empty until the next submission".
- **Fix, d007cd0 (17:33):** "Validate status publishers against current backend identity". The check now uses the backend's current identity and still rejects stale publishers. Claim: "Fixed and verified in your running Emacs."
- So the publisher from 3bc93d2 broke within a day of shipping.
- 01a0c59f-6aff is a Codex "guardian" approval subagent and is not relevant.

#### 2026-09-24/25, claude-code 466d5b92 (raw copy 697cde77; hooks-driven Claude state)

Rendered: `...--claude-code--466d5b92-....md`. The raw `697cde77-....jsonl` has the same conversation, extended by one task-notification at 2026-09-26T02:18Z. That notification woke the session with "A leftover background wait ... just ended": a stale background task restarting an idle session.

- **User (21:59):** "we still do not have a reliable way of determining whether claude is working or waiting for input. Is this still not something that claude code exposes?"
- **Research.** The agent read `code.claude.com/docs/en/hooks` and checked Claude Code 2.1.282. Answer: "No ... no event or field that says 'busy' or 'idle' ... nothing like `claude status`, and no socket or file you can poll."
  - Current mechanism: screen-scraping with `agent-claude--terminal-waiting-p`, using the prompt regexp and `agent-claude--turn-in-progress-regexp` (`agent-claude.el:1166`).
  - Proposed mapping:
    - **Working:** UserPromptSubmit, PreToolUse or PostToolUse.
    - **Waiting:** Stop, StopFailure, PermissionRequest, or Notification with `permission_prompt`, `elicitation_dialog` or `agent_needs_input`.
    - **Gone:** SessionEnd.
  - Warning given: "Don't rely on `idle_prompt` ... fires only after a built-in delay."
  - Gap named up front: "Background shell tasks have no hook."
  - Claim: "much sturdier than what `agent` does now."
- **Esc test (user: "test Esc first then build it").** The agent ran Claude (Haiku) in a pty with logging hooks on every event, twice (the second time without matchers), covering `sleep 30` tool turns and long text generation. Result: "an Esc interrupt fires no hook at all, whether Claude is generating text or has just run a tool." Stop fired only for a later normal turn. tmux was not installed.
- **Findings while building:**
  - The state machine already had handlers for `activity`/`blocked` events and referenced a `notify-emacs-state.sh` that did not exist. Nothing was sending those events.
  - The settings only wired Stop and Notification, so "a turn you didn't type yourself never shows as working".
  - An existing code comment already noted that an interrupt "fires no Stop hook, so `agent--session-state` stays `busy` forever".
  - Pre-existing bug: under Elpaca, `hooks/` resolved to `builds/`, so the installers were broken. The live settings only worked because they still pointed at an old `8.3.0-dev` checkout.
- **Built:**
  - **3b3a5da (22:14), "Drive Claude session state from turn-lifecycle hooks":**
    - A new `hooks/notify-emacs-state.sh` forwards UserPromptSubmit, tool and SubagentStart events as `activity`, and StopFailure as `stop`.
    - Events carry their send time, and Emacs drops events sent before the session last became waiting.
    - An Esc sent to a busy session schedules a terminal check 1.5 s later.
    - `agent-claude-ensure-state-hook-config` installed six hooks into `~/.claude/settings.json`.
  - **a99d8f6:** "Locate bundled Claude helpers in the source checkout".
- **Claims (22:21):**
  - "sessions now show as working or waiting correctly, including after an Esc interrupt."
  - Live test: a plain turn, a tool turn and an Esc "all went busy and back to waiting within about a second". 637 tests pass.
- **Gaps the agent stated:**
  - Permission prompts were not tested live.
  - After approving a permission, the session shows waiting until the tool finishes.
  - An interrupt from outside Emacs (for example remote control) is not detected.
  - Background shell tasks have no hook.
- **"History reconstruction" (07:13, 09-25).** The user complained: "you keep telling me about this friction ... We seem to be going in circles ... reconstructing the whole history". This was about the **Elisp commit/verification gate**, not session state.
  - Three subagents audited it: 62 gate commits, about 40% patching false positives; 660 denials since 08-19, 48% of them shape refusals.
  - The gate was moved to a global git `pre-commit` hook plus a Stop-hook live-check debt (dotfiles c496649c2, 8e60c9261, aac0abad7).
  - No history of the session-state feature was produced.
- **Ideas not raised in this session:** `lsof`, `claude agents --json`, Agent SDK / stream-json. None appear in the transcript.
- **Later breakage [inferred]:** the agent's own list of remaining gaps (Esc from outside Emacs, background shell tasks, the approval window) overlaps the user's later complaints. The repo later has commits for background-work state, such as "Count a teammate as background work only while it works" and "Do not count Claude Code's own artifact watches as background work".

#### 2026-09-26, claude-code bf1e0e83 (tab bar, e13afd7)

- **User:** "In the tab bar, we should display each open session with the same color-coding scheme used by agent-select-session ... see at a glance ... the state of every session."
- **Built, e13afd7 "Show every session's state in the tab bar":** `agent-tab-bar-mode`, with ordering and faces shared with the switcher.
  - A 1 s timer (`agent-tab-bar-refresh-interval`) redraws the tab bar because "some states, like background work, are read off the terminal and don't fire an event."
  - Verified with a screenshot and `elisp-live-verify`.

#### 2026-09-26, claude-code 1e9d20cb (phone access): touches state only marginally

- Remote access via Tailscale and SSH, plus an Android "Agents" WebView app (44d8ae2, 29ea567, e78d269).
- The phone page lists sessions with agent.el's states (WAITING, BG WORK, BUSY, snoozed), waiting first.
- The agent cited `agent-claude.el:1271`: it "tracks turns started remotely".
- Incident: a test Emacs deleted the server socket, which made the agent's `agent-session-set-status` calls fail. The agent first misattributed this to unloaded code, then corrected itself.

#### 2026-09-27, claude-code f459bdc2: not relevant

Restart-with-account from the phone (dd6646d). No state-display changes.

### Batch E: 2026-10-01 to 2026-10-04

Times are the session-local times shown in the turn headers (UTC-3). Rendered filenames use UTC.

#### 2026-10-01: 4e485ac8 (claude-code, agent/). 50adba41 is the same conversation

50adba41 is an earlier render of the same opening turns (08:44 to 09:06), cut off at the user's interruption. All content is in 4e485ac8.

- **Reported:** published statuses (`agent-status`) had vanished from every session: "it's not printing for any freaking session". After that: "why wasn't this done when you implemented this feature?", and "If something was working before and it's not working now, of course you need to identify what caused the fucking change".
- **Diagnosis, which changed several times:**
  1. First claim: "Emacs died". Retracted at once.
  2. Second account: agents rarely republish, and `agent--session-set-state` cleared the status on every busy transition. The agent said the original feature (3bc93d2, Sep 20) "was only tested as plumbing, never against how agents actually behave over time".
  3. After the "what changed" pushback, the final account: on the night of Sep 30, `emacsclient` calls were refused while Emacs briefly froze. This was reproduced: with Emacs frozen for 3 s, 7 of 12 concurrent clients failed. The flood came from per-tool-call state hooks (two `emacsclient` calls per tool call, subagents included). Agents whose publish failed concluded "Emacs isn't running", partly via a macOS `pgrep` ancestor-exclusion false negative, and stopped publishing. Overnight notification turns wiped the rest.
  4. The cause of the Emacs freezes was never found.
- **Found along the way:** Claude hook paths in `~/.claude/settings.json` still pointed at the 9.0.1-dev checkout, and the statusline at 8.3.0-dev. The cause: setup was a manual command that only added entries and never replaced them. The user's reaction: "WTF? hard-code that path?!"
- **Built:**
  - `2bd6484`: only a user prompt or an identity change clears the status. Notification turns no longer clear it.
  - dotfiles `82abbbcb`: `agent-status` retries for about 8 s.
  - The CLAUDE.md wording now tells agents to keep publishing after a failure.
  - `bab30f8`: Emacs writes a per-session state file, and `notify-emacs-state.sh` skips `emacsclient` while the session is already busy. Hook and statusline paths now go through the `~/.config/emacs-profiles/active` symlink, and setup replaces stale entries.
- **Considered but not pursued:**
  - A Stop hook forcing agents to publish every turn. Deferred, then dropped.
  - Skipping subagent tool-call hooks. The agent proposed it as "cut how many hooks call Emacs"; the user rejected that as limiting agents, and the agent then explained it only meant fewer notifications.
- **The agent's own words on its risk:** "one new failure mode: the file and Emacs could disagree".
- **Lessons recorded in the learnings inbox:** find what changed first; macOS `pgrep` hides the processes the agent runs under; give one plan with one approval.
- **Later [inferred]:** fa6f3fd (Oct 5, "Keep a published status after a prompt, marked stale") suggests that clearing on a prompt was itself revisited.

#### 2026-10-01: f153f33c (claude-code, agent/). In depth

- **Reported (12:44):** "We've struggled with this for weeks and months now, and I'm wondering if there is finally a solution, a permanent solution". The user's pasted complaint: "you're not able to reliably determine the status of a session". stafforini.com showed green while background work was running.
- **Diagnosis:**
  - The main agent was at its prompt while teammates (`port-a1`, `port-a2`) did the work.
  - The only on-screen hint was the `← N agents` footer. That footer counts agents across every session, so 8b90291 (Aug 24) had stopped using it.
  - The agent's framing: "reading the screen can't solve it".
- **Research:**
  - A subagent summarised the code.claude.com hooks and statusline docs. It listed SessionStart/End, UserPromptSubmit, Stop, StopFailure, Pre/PostToolUse, SubagentStart/Stop, TaskCreated/TaskCompleted, TeammateIdle and Notification, among others.
  - It concluded that no hook or statusline field reports running background tasks, and that no hook fires when a background shell finishes.
  - The agent then told the user: "there's no hook and no status-line field that reports running background tasks".
  - No real hook payload was ever captured, which a65e8d68 later identified as the error.
  - None of these alternatives was considered: `lsof` on task output files, `claude agents`, the SDK, or stream-json.
  - Filing a feature request on anthropics/claude-code was offered and deferred. The agent later recommended skipping it, saying "the fix already works without it".
- **Proposed:** count tasks from the session transcript. Started tasks (`backgroundTaskId` or agent ID) minus `<task-notification>` completions gives the running count. The agent named the risk itself: "the transcript format is internal to Claude Code and could change".
- **Claims:**
  - "Yes, there's a fix that should hold."
  - "This should be the permanent fix."
- **Built:**
  - `f6a48b3`: an incremental transcript scanner, run on the 5-second status poll. It replaced the `· N shells` / `· N monitors` footer check.
  - `631e910`: `json-end-of-file` while reading a half-written status file had permanently killed a session's poll timer, because `debug-on-error` was on. That error is now caught.
  - `016f6c2`: the manual was regenerated.
  - All pushed.
- **Verification:**
  - A throwaway session (d9f73957) went busy → background-waiting → waiting.
  - The test fixtures were written by the agent itself, a point a65e8d68 later conceded.
- **Same-session failures:**
  1. "the changes ... don't seem to be working ... pr-digest". Cause: 8 live sessions held stale scan state from an uncommitted draft.
  2. "don't you see that there are background processes?" (stafforini.com, pr-digest). The agent maintained that green was correct: teammates were stalled waiting for completion notices that went to the lead. Both sessions confirmed this.
  3. pr-digest showed green during a `!` audit that Claude Code had auto-backgrounded. Fixed by `94ddbc8` ("Count user ! commands moved to the background").
- **Ideas not pursued:**
  - A "stalled" colour for teammates whose notices never arrived.
  - Detecting detached `nohup … &` processes. The agent acknowledged these were invisible to it.
- **Lessons recorded:** ask the session itself when its claims conflict with evidence; live tests of fresh sessions only had missed stale state in running sessions.

#### 2026-10-01: d9f73957 (agent-bgtest/)

A throwaway test session for the f6a48b3 live check. Its prompt: "Run exactly this one Bash command with run_in_background … sleep 75". It replied "started", then "done". No discussion.

#### 2026-10-03: a65e8d68 (claude-code, agent/). In depth

- **Reported (11:41):** tangodb:default was green although the agent said it was waiting on a background process.
- **Diagnosis:**
  - The transcript scanner assumed a subagent's tasks end when the subagent finishes.
  - In current Claude Code, those shells keep running. Shell `bkix9xqlh` was marked dead.
- **User pushback, in sequence:**
  - "every time you assure me that the package realiably detects the session status and we keep discovering that it doesn't"
  - "Last time you spent HOURS doing a thorough refactor, assuring me that we were taking a more robust approach"
  - "I JSUT WANT A FREAKING PERMANENT SOLUTION"
  - "LOTS AND LOTS of packages do this, e.g. orca. HOw on earth do they do it?"
  - "now you are saying we should DELETE the very thing you told me we should BUILD two days ago!"
- **Agent admissions:**
  - "I should stop saying it's reliable. It isn't."
  - "Calling it the robust approach overstated it."
  - "I can't promise 'permanent'."
- **Intermediate proposals:**
  1. Use the process list (child shells of the CLI) as the main signal for shells.
  2. Measure the signals against live sessions for a day or two before changing detection, and show uncertainty in the UI.
  3. Use `lsof` on `/private/tmp/claude-501/<project>/<session>/tasks/*.output`, since open file handles mean a running shell. Background agents would come from SubagentStart/SubagentStop hooks, and the UI would gain a "can't tell" state.
  - All three were superseded.
- **Research into other tools** (after the Orca prompt):
  - Web searches turned up nothing specific on Orca.
  - iTerm2's Claude Code integration page: a `cc-status` hook reports working, waiting or idle.
  - hadees/dotfiles PR #67 (the iTerm2 `attend` registry).
  - claudeissues #38184 ("reliable session state detection API"; it notes hook state goes stale when transitions are missed).
  - anthropics/claude-code issue #93672 (`idle_prompt` fires while background subagents run).
  - The agent then captured a real Stop payload on Claude Code 2.1.288. Its keys included `background_tasks` (`{"id","type":"shell","status":"running","command"}`) and `session_crons`.
  - The docs did not list that field.
  - The package's hook had been doing `cat >/dev/null`, discarding the payload.
- **The agent's verdict on Oct 1:**
  - "I concluded from the documentation that no hook reports background tasks ... I never captured a real hook payload, which took two minutes."
  - "I treated passing tests as proof. Those tests ran on transcript examples I wrote myself."
- **Built:**
  - `dc0d128`: background tasks are read from the Stop hook payload. The transcript scanner and its tests were deleted, the hook was installed in `~/.claude/settings.json`, and a warning was added in case the field disappears. Pushed.
  - Live check: a subagent left `sleep 90` running; the tab was amber for 90 s, then green.
  - Final claim: "It's fixed". The agent listed limits: the field is undocumented, and a task killed by hand stays amber until the next turn end.
- **Same session, 21:06:** "I want to cry ... wikipedia-improver ... COLOR DOESN'T MATCH". A `claude -p` child inherited the parent's Emacs identity, and its Stop hook marked the parent waiting mid-turn.
  - Fix: `bc7fb69` (hooks compare the payload's `session_id`) plus dotfiles `28e6a436`.
  - Known gaps left: hooks after `/clear`; teammates running as separate processes.
- **Lessons recorded:**
  - "Check real data and how other tools do it before concluding a tool exposes nothing."
  - "Never call a fix permanent or robust when its tests use examples the agent wrote itself."
  - Hooks shouldn't trust a session identity that subprocesses can inherit.

#### 2026-10-03: 21515d87 (agent-emacs/). Relevant only to hook plumbing

This was a distro-building session, not about detection. Its relevance:

- It moved hook registration from global settings to per-session `--settings` (agent `232bba5`, pushed). The test session got every event twice until the global entries were removed.
- The agent warned that migrating would make running sessions "stop reporting busy/waiting state ... until restarted".
- It also picked up a65e8d68's unpushed `bc7fb69`.
- a65e8d68 cited this as friction: Stop now reached Emacs by two routes.

#### 2026-10-04: 5977de78 (claude-code, agent/)

- **Reported (08:4x):** "this package is not working reliably". Three issues: usage rows were stale, the modeline was reset, and the phone app showed black.
- **Modeline:** the agent proposed re-applying the modeline whenever doom-modeline is enabled. The user replied: "patches that are superficial and inelegant". The agent installed a trace to `~/.local/state/agent-modeline-trace.log` instead.
- **Session state (09:15):** "the tangodb session IS RUNNING A BACKGROUDN PROCESS but is showing as black, now as brown!"
  - Diagnosis: a background subagent's tool-call hooks marked the waiting session busy.
  - The docs confirmed that subagent hooks carry `agent_id`.
  - `b13f9cf` ("Keep a waiting session waiting while a background subagent works"): Emacs ignores subagent activity while the session is waiting, unless that subagent raised the permission prompt.
  - Claim: "I've fixed it."
- **Pushback:**
  - "explain what caused this difference in this specific case"
  - "You told me yesterday you had fixed it permanently ... This time, you said it was for real because you're using a hook".
- **Agent's account:**
  - At first: "I don't know the difference, and I shouldn't have said 'fixed'".
  - Then: tangodb's background work is subagents (over 4,900 since Oct 1), while other sessions use shells, which fire no hooks.
  - Its framing of the whole history: "Each round has been the same flaw: Emacs attributes any hook carrying the session's environment to the main session's turn."
- **Verification gap it admitted:** "I haven't yet watched a full turn end in a real session with a working subagent."
- **Lessons recorded:**
  - "Explain why only some sessions are affected before calling a bug fixed."
  - "When the trigger is unknown, add tracing instead of proposing a re-apply patch."
- **Rest of the session:** usage and Codex browser work, not about state.

#### 2026-10-04: 336425b6 (claude-code, agent/)

- **Reported (19:56):** "AGAIN the modeline was reset ... The Wikipedia improver session said it's working, and it's not working ... Each time, you assure me that the status will reflect the status of the agent, whether it's listed as black, green, or brown ... you keep promising something that you cannot deliver."
- **The user pointed it to the trace:** "you told me that you were recording the events".
- **Findings:**
  1. The modeline reset happened because `doom-modeline-mode` had been recorded as customized in the `user` theme, so every light/dark theme switch re-ran it.
     - Built: `137f717` re-applies the session modeline when doom-modeline is enabled, the same idea the user had rejected that morning. Also `1f49ab8` (texi catch-up).
  2. The status was correct. Three shell tasks had really been running for 5 hours: `until ! pgrep -f "wv.py verify"` loops that match their own `zsh -c` command line.
     - About 200 past logs contain such loops.
     - The user's own process-listing guard (Sep 27) had steered sessions toward `pgrep`.
     - Built: dotfiles `94ef6828` changed the guard message to suggest `kill -0 $PID` or `pgrep -f '[w]…'`. A blocking hook was proposed and dropped.
- **Idea recorded:** show how long background tasks have been running.

#### Inferred later breakage [inferred]

Commits after this batch, all about what counts as background work or status freshness:

- fa6f3fd (Oct 5): published statuses marked stale.
- 12cad8b (Oct 6): the Stop hook waiting on a stalled Emacs.
- b36a2fc and e37eb00 (Oct 10): teammates, and Claude Code's own artifact watches, wrongly counted as background work.

They suggest that `background_tasks` from dc0d128 includes items that should not count, and that the hook path to Emacs still had failure modes.

### Batch F: sessions 2026-10-05 to 2026-10-10

Session times are the local times shown in the rendered logs. The commit timestamps for 10-10 are +0100.

#### 2026-10-05 · 69e421c0-b8d2-4abb-906c-21b70be166e9 · claude-code (11:39–12:57)

- **Reported:** "unparseable session header" warnings. The header reader stopped at 64 KB. Fixed in `a0bb13d`, then moved to `agent-util.el` in `a725b3f`. Neither concerns state.
- **Status-column part:** the user asked why most sessions showed no summary. The agent's first proposal was a Stop hook that would block the end of a turn until `agent-status` had run.
- **User pushback (12:43):** "We discussed the design of this many times, so I'm worried that you keep changing things every time in a cycle… check the historical context… only then do you propose a fix."
- **History it rebuilt:** three subagents read the earlier sessions.
  - **20–21 Sep:** agent-authored status, modelled on Orca. The user rejected chat excerpts and did not approve a summariser model. On 09-21 the user declined stricter cadence and a status-age column ("let's leave it as is").
  - **1 Oct:** statuses went blank because Emacs refused connections and statuses were cleared on every busy transition. Fixed in `2bd6484`, `82abbbcb`, `bab30f8`. The forced-publish Stop hook was deferred "if statuses still go blank".
  - **5 Oct morning:** the user ruled out new hooks and new CLAUDE.md lines ("this obsession you have with adding hook upon hook").
  - One subagent noted that log f153f33c (1 Oct) covers the busy/waiting colours and background-task detection (`f6a48b3`, `631e910`, `94ddbc8`).
- **Measurement:** across 5 days of turns, the share in which the agent published a status:
  - plain replies (0 tool calls): 0% of 713;
  - 1–4 tool calls: 6% of 681;
  - 5–19 tool calls: 26%;
  - 20+ tool calls: 51%.
- **Diagnosis:** clear-on-prompt combined with short turns wipes good statuses.
- **Built:** **`fa6f3fd`** (12:54, authored in this session), "Keep a published status after a prompt, marked stale". A prompt now keeps the status and shows it in an italic stale face.
  - It was verified live on the session's own row only: "I haven't seen the italics myself."
  - The Stop hook proposal was withdrawn.
- **Lesson recorded (inbox):** "Record the design decisions for the status column somewhere agents will find them". The reasoning was spread across three sessions, so the agent re-proposed a hook that had already been deferred.

#### 2026-10-05 · 32cda93e-d775-41e5-a03e-7d0539b557b8 · claude-code

- **Reported:** codex.el was not loading in the Forethought session ("Can't believe this is happening again").
- **Causes:**
  - Codex's first-launch indexing of 11,155 sessions for a new account.
  - A stale `codex-extras` hook left in a pre-10-04 Emacs session.
- **State relevance:** none. `fa6f3fd` appears only in git output.

#### 2026-10-06/07 · 6d24c96f-0344-4d1b-810b-dc6966fe113f · claude-code

- **Reported:** "a session can't finish because some idiotic hook". Later: "Just yesterday we increased the … client limit… This cannot keep happening", and "yesterday you told me this was sorted… I don't trust your proposal."
- **Diagnosis path:**
  - The Stop hook (`claude-code-hook-wrapper stop`, a blocking `emacsclient`) hung for 600 s. 139 emacsclients were queued.
  - The 10-05 backlog increase (5 to 128, `6ce850701`) turned instant refusals into 10-minute waits.
  - The agent guessed "Emacs busy", then "descriptor leak" (johnson's 446 sqlite files). Both were wrong.
  - Root cause: Emacs sat in the debugger three levels deep, from `debug-on-entry` on `clear-minibuffer-message` hit inside `org-roam-db-sync`. Who set the breakpoint was never found.
- **Overclaims:**
  - "It's fixed" at 15:00. User: "No, this is not fixed… You're only fixing this particular session."
  - "Tell me if you want me to … make the hook fix permanent."
  - The user demanded "a robust, stable plan that doesn't succumb to the slightest critical pressure". The agent admitted this was its fourth plan.
- **Incident classification (subagent):** about 20 Emacs-server outages since May, by established cause:
  - (a) debugger or prompt: 7;
  - (b) long Lisp, mostly agents' own synchronous evals: 7;
  - (c) socket hijacked by a test Emacs: 2;
  - (e) wrong TMPDIR: 2;
  - (d) backlog overflow: only after a stall of unknown cause.
  - 8 or more refusal windows were never diagnosed.
  - "600 s Stop-hook timeouts in ff3e889f on 10-04/05/06: never investigated."
- **Built:**
  - **`12cad8b`:** the Stop hook goes through `fire-and-forget.sh`/`notify-emacs-state.sh`, background emacsclient gets `--timeout=10`, and "drop a stop sent before the event that began the current busy period". That last change is state-relevant: async Stop delivery could otherwise mark a busy session as waiting. The late-stop drop was "covered only by unit tests, not a live run".
  - **Dotfiles:**
    - `c398d2a80`: Emacs writes a server-state file every 5 s.
    - `ec2d4455e`: launchd watchdog.
    - `4bfbe9b1b`: the emacsclient shim inhibits the debugger and prompts and caps agent evals at 120 s.
    - `66be91418`: the `diagnose` skill gains an adversarial-review step.
    - `8c4c90ab5`: `diagnose` description changed to "unintended behavior without established cause".
  - **Other repos:**
    - codex `e34b7d7`: non-blocking Codex hooks.
    - johnson `017029d`/`ca478e7`: johnson cap.
    - org-roam: `fd` listing.
- **Claim at 21:55:** "No Claude or Codex turn waits on Emacs." Sessions started before the change kept the old hook, so a stall can still hold their turns for up to 130 s.
- **Meta lessons:**
  - The agent used `live-session-status` instead of `diagnose`.
  - It never ran the cheapest discriminating check ("is the UI usable?").
  - The user rejected a keyword hook forcing `diagnose`, and objected to the agent "fine-tuning the description for this particular case".

#### 2026-10-07 · 25413f09-79e0-4cc3-95ae-e6f6b664627f · claude-code

- Backtrace about buffers defaulting to a stale Epoch directory. Commit `462fe78`.
- Not state-relevant.

#### 2026-10-10 · af29dec8-f9b0-4285-9b34-a4c21376ceff · claude-code (12:29–14:19)

- **Reported:** "Why is the live Forethought session showing up in brown? Shouldn't it be green."
- **Diagnosis:**
  - Brown is the theme's `warning` face, meaning waiting with background work.
  - Two teammates (`gms-multi`, `mu4e-ft`) had finished at about 11:00, but Claude Code lists an idle teammate as `"status": "running"` in the Stop hook's `background_tasks` until it is shut down.
- **First proposal:** decide idleness from `idle_notification` in the transcript.
- **User (13:32):** "we've been struggling with this for weeks. Can you familiarie yourself with the history/logs and review your plan?"
- **History it cited:**
  - **1 Oct:** the user wanted teammate/background work shown non-green, so ignoring teammates would reverse a decision.
  - **3 Oct:** the transcript scanner was replaced by Stop-hook `background_tasks`. "the earlier 'permanent fix' had been tested only against fixtures the agent wrote itself". Transcript parsing would therefore regress.
  - **4 Oct:** tangodb's 4,900+ background Agent calls were already `teammate` tasks.
  - **Open since 1 Oct:** a "stalled" colour for a teammate that never received its notice.
- **Today's findings:**
  - Five sessions affected; ai-enablement had had four "running" teammates since 01:00.
  - The team config files have no busy/idle flag.
  - The docs list `TeammateIdle`, and `SubagentStart` fires each time a teammate handles a message. Both carry `agent_id` (v2.1.290+, user on 2.1.296).
- **Gap stated:** `background_tasks` gives no `agent_id`, so a task cannot be matched to a teammate. It can only tell whether any teammate is busy.
- **Payload capture:**
  - Headless `claude -p` does not create teams, so the capture ran interactively under tmux.
  - Confirmed: `SubagentStart` on wake; `SubagentStop` and `TeammateIdle` on idle; same `agent_id`.
  - Also reproduced the 1-Oct stalled-teammate bug: the teammate backgrounded a sleep, went idle, and was never woken.
- **Built:** **`b36a2fc`**. A per-session `<key>.agents/` directory gets one file per working subagent or teammate: created on SubagentStart, removed on SubagentStop or TeammateIdle. Teammate tasks count only while that directory is non-empty.
- **Verification:**
  - In a throwaway session, three busy-to-idle cycles were "brown exactly while pinger's agent file existed and green once idle". The busy windows lasted only 2–12 s, because the classifier blocked long test jobs.
  - 779 tests passed.
- **Claim:** "An idle teammate no longer turns a session brown; a busy one still does."
- **Admitted gap:** sessions started before the change lack the new hooks, so "teammate work no longer turns the session brown at all, even while a teammate is busy".
- **Deferred:** one session listed nine "Wait for CI" shells as running, possibly stale.
- **Lessons (inbox):**
  - staged-snapshot mode for `batch-test.sh`;
  - list outgoing commits before a push (it pushed `f968f03` unapproved);
  - `claude -p` can't test teams;
  - no approved long-running command for live tests.
- **Not considered:** lsof, `claude agents --json`, SDK or stream-json.

#### 2026-10-10 · 2a003857-9645-4048-9ddd-da1a3d254fda · claude-code (13:52–14:18)

- Replaced the doom-modeline usage, cost and duration segment with the session's latest summary.
  - `f968f03`: makes `agent-session-state-summary` public and refreshes the mode line on status publish or stale.
  - Dotfiles `411677b7f`.
  - Stale summaries keep the stale face.
- Collided with af29dec8 over the commit guard. `f968f03` was pushed without approval.

#### e37eb00 "Do not count Claude Code's own artifact watches as background work" (10-10 21:53 +0100)

- No other session authored it. A search of every rendered log and every raw transcript in the shared project store, excluding 70afc41c, found no mention of the commit hash or subject.
- The `~/.claude-*/projects` directories all symlink to `~/.claude/projects`.
- So only the current session (70afc41c) authored it.

#### Cross-session pattern [inferred]

- **10-05:** the status was cleared on prompt (fixed `fa6f3fd`).
- **10-06:** async Stop opened a late-stop race (guarded in `12cad8b`, unit-tested only).
- **10-10 midday:** idle teammates counted as running (`b36a2fc`).
- **10-10 evening:** artifact Monitors counted as running (`e37eb00`).
- Each fix patches one more category of `background_tasks` entries that do not mean real work. No session considered a whitelist or verified liveness per task: lsof on task output files, `claude agents --json`, or SDK/stream-json.


---

# Appendix C: dotfiles side


Repo: `~/My Drive/dotfiles` (git dir `~/git-dirs/dotfiles`, branch `master`). Hashes are those on HEAD; the git dir also holds a parallel rewritten lineage with different hashes for the same commits (e.g. `82abbbcb4` = `3e989dc94`, `6ce850701` = `fad577deb`), which some inbox records cite. Inventory compiled 2026-10-10.

### 1. Status publishing (`bin/agent-status` and the global rule)

| Date | Commit | What | Why | State |
|---|---|---|---|---|
| 2026-09-20 | `3ce673591` | Adds `bin/agent-status` and the "publish your current state" rule to `claude/CLAUDE.md` and `codex/AGENTS.md` (+ READMEs). | Give the agent menu / switcher a model-authored current-state column. | Active (rule text since revised). |
| 2026-10-01 | `3e989dc94` | Wrapper retries `emacsclient` connection refusals; instructions say a failed publish usually means Emacs was briefly busy, so keep publishing. | Agents gave up publishing for the rest of a session after one refusal. | Retry active; guidance partly stale (see `fad577deb`). |
| 2026-10-05 | `fad577deb` | Emacs server listen backlog raised from 5 to 128. | On macOS a full backlog returns ECONNREFUSED; reproduction: 7/12 clients refused at 5, 0/12 at 128; live burst 12/12 after. | Active. Inbox record `claude-baac04d1` flags that "refusal = busy" guidance in CLAUDE.md/AGENTS.md/agent-status is now stale (refusal should now mean no listener). Not acted on. |
| 2026-10-05 | `95efcb977` | Shortens the CLAUDE.md/AGENTS.md rule to one line; Who/When/What/On-failure details move to `agent-status --help`. | Instruction budget. Session `69e421c0` records a same-day constraint: "no new hooks; no new lines in CLAUDE.md/AGENTS.md" for status. | Active (current rule). |
| 2026-10-07 | `be59aed32` | Every reply after approval ends with one of four status lines (`Status: done`, `done, pending verification: X`, `waiting on you: X`, `in progress: X next`). | Replies left unclear whether anything was open. | Active; the `Status: done` line is the trigger of the 2026-10-10 Stop hook. |
| 2026-10-10 | `411677b7f` | `doom-modeline-extras` `ai-session-status` segment drops rate-limit usage, duration and alert bell; shows icon, name, model, effort, account and `agent-session-state-summary` (the last published status, same text as the switcher's last column). −731/+119 lines. | Mode line should show the session's latest state. | Active. |

Earlier mode-line history: `688d26372` (2026-04-02) generalised the Claude-only segment to `ai-session-status` for any backend; `91c20eb78` (2026-06-19) fixed a void `agent--backend-get` call that made doom-modeline silently render the segment empty; `d25b81115`, `fd56b8512`, `d09bd282b`, `ac330e804` (2026-06) added accounts/effort. `09f1ef64c` (2026-04-20, `ai-extras`) introduced a distinct "waiting with background tasks" face by scraping Claude's "· N shells" terminal text; superseded by agent.el's own state machine (ai-extras/claude-code-extras era).

Recurring proposal, never adopted: inbox records `claude-b45fde2a` and `claude-contextonly-24ca95ab` (2026-09-30) report agents skipping `agent-status` on short turns and propose a hook publishing a baseline status on UserPromptSubmit/Stop. Session `69e421c0` (2026-10-05) records this Stop-hook option as deliberately deferred on 2026-10-01 ("only if statuses still go blank") and asks for a decision record so it is not re-proposed. `claude-746aad7f` (2026-10-01) asks `agent-status` to distinguish "Emacs not running" (stale socket) from other failures; not implemented.

### 2. Emacs notification hooks (Stop/Notification/turn state)

| Date | Commit | What | State |
|---|---|---|---|
| 2026-03-28 | `602a61830` | `claude/hooks/notify-emacs-session-end.sh` Stop hook notifies Emacs. | Removed. |
| 2026-03-29 | `5b33d334b` | `notify-emacs-notification.sh` Notification hook (alerts had stopped because the hook was empty). | Removed. |
| 2026-07-27 | `95843f8ef` / revert `88cbebd7c` | `notify-emacs-state.sh` turn-state wrapper (activity/blocked from lifecycle events); reverted same day. | Reverted. |
| 2026-10-03 | `e4fb0e4e1` | Drop Stop events from `claude -p` runs a session spawns (inherited `CLAUDE_BUFFER_NAME`) by comparing payload `session_id` with `<key>.sid`. | Superseded: agent.el now does this check itself. |
| 2026-10-05 | `63374c9fe` | Deletes `notify-emacs-session-end.sh`, `notify-emacs-notification.sh`, `fire-and-forget.sh` and other unregistered scripts. | Current. Since 2026-10-03 the agent package passes its status line and Emacs hooks (Stop, Notification, turn state, background tasks) to each session via `--settings`; the dotfiles copies made every event reach Emacs twice. |

Other Stop hooks: `completion-claim-guard.sh` (2026-05-16 `92fceb1c8`/`efc9b4d15`, removed 2026-05-20 `43f3f714a`). Currently registered in `~/.claude/settings.json` (not a dotfiles file): Stop = `claude-stage-hook`, `elisp-evidence stop-check`, `block-done-over-running-tasks.py`; TeammateIdle = `hold-idle-teammate-with-running-task.py`; no SubagentStop or Notification hooks from dotfiles.

Related: `28f4e64b6` (2026-10-06) agent_guard `stale_team` policy denies SendMessage to agents bound to a stale team after a resume (17 reports stranded for two hours on 2026-10-06). Active.

### 3. The 2026-10-10 background-task work

| Commit | What | State |
|---|---|---|
| `1230ae6f2`, `99c81439a` | Plan `stale-background-jobs.md` and Codex-review fold-in. | Plan implemented. |
| `37e9f4e2d` | `claude/hooks/block-done-over-running-tasks.py` + `tests/test_block_done_over_running_tasks.py`; `ai-config-sync.json` entry (Claude-only, Codex parity "unassessed"). | Active, registered. |
| `6b121ab62` | Corrects `agent_guard/policies/secret_leak.py` pgrep text: macOS `pgrep` never matches itself or its ancestors; an unbracketed `pgrep -f` matches sibling commands (e.g. a re-armed copy of the same loop). Advice unchanged. | Active. |
| `2fe8f05ad`, `5ac21b907`, `b14e5484f` | Plan `undelivered-task-notices.md`, Codex review (7 findings), replay result: gate failed. | Abandoned; nothing built. |
| `76adc5b9f`, `f829235ed` | Plan `teammate-idle-with-running-task.md` and Codex review (5 findings). | Implemented. |
| `cfe3de1b0` | `claude/hooks/hold-idle-teammate-with-running-task.py` + tests (7 fixture cases + both launch forms). | Active, registered. |
| `616c833a7` | Caps the suggested foreground wait at 100 rounds: the live test showed a 115-round wait run just past 600 s and get moved to the background itself. | Active. |

#### 3a. Plan: stale background jobs (Stop hook)

- **Problem.** agent.el showed sessions busy after the agent said done. Earlier agent.el fixes (f6a48b3, 94ddbc8, dc0d128, b13f9cf, b36a2fc) fixed real misclassifications; the 2026-10-10 jobs were really running.
- **Incidents.** dotfiles session 30baff9b: shell `bam9tx09h` ran ~3 h; its test failed at once (`/usr/bin/taskpolicy` missing), a glob matched nothing, so `rg -c PATTERN` read stdin (a socket Claude Code keeps open), 0 s CPU; the agent ended `Status: done` with the shell listed as running in the 14:32 Stop payload. Epoch/meetings f44df81c: 11 shells running, some >16 h, mostly `until ! pgrep -f "gh pr checks 15"` loops deadlocked on each other (re-armed copies), no `gh` process.
- **Hook design.** If the last `Status:` line of `last_assistant_message` is exactly `Status: done` and `background_tasks` has running non-teammate tasks, block and list them (TaskStop them or change the status). Allow otherwise; a missing field is reported on stderr. Loop guard uses its own per-session/`prompt_id` counter (not `stop_hook_active`, which other Stop hooks set); after two blocks allow with a `systemMessage`.
- **Not covered.** Turns without a status line; honest `in progress`/`pending` with running tasks; teammates; the time between a hang and the next "done".
- **Rejected.** Deadlines on background commands; banning until/while loops; showing task age (payload has no start time); gating every turn end with running tasks.
- **Open.** Codex parity; gating status-less final turns. The plan has no recorded result section for its live tests (sleep 600 / subagent case); README and commit do not state they ran.
- **Review.** Codex review of `1230ae6f2`: three findings accepted (hook-specific loop counter with visible exhaustion, subagent live test, safe cleanup with fresh process identification).

#### 3b. Plan: undelivered task notices (abandoned)

- **Incident.** Meetings session f44df81c shown green with `Status: in progress`. Teammate `phase2c` started `bfe8k1c3s` at 15:27, it exited 0 at 15:34, its `<task-notification>` was enqueued in the main transcript (14:34:54Z) but never delivered; released at 16:42 only after another session messaged the lead. agent.el's green was correct; Claude Code delivery defect; user will not file upstream.
- **Design.** Model `queue-operation` enqueue/dequeue/remove records in agent-claude.el, flag `stuck`, send one `agent-submit` wake-up per item. Codex review added: same rule for replay and production, forward `transcript_path`/`session_id` from the hook, submit only into a verifiably empty prompt, `file-notify` while idle, invalidate the model on unknown shapes or truncation, an "inconclusive" class, and flag-only integration checks.
- **Result (gate failed).** 816 transcripts (28 days): 22,922 enqueues, 5,663 dequeues, 17,002 removes; 523 dequeues unattributable, 55 removes without enqueue, 16 unknown shapes. FIFO is wrong: a dequeue pairs with the item the next user record carries, usually the newest. 1,047 task notices outstanding at a lead turn end: 592 delivered within 60 s, 225 after ≥1 min, 230 never delivered in-transcript (concentrated in six agent-team-heavy tangodb sessions). Sampled lead-owned "late" cases were pairing artifacts. Nothing shipped.
- **Unvalidated alternative.** "Whole session idle (lead waiting, no subagent/teammate working) and a completed task's notice text absent from all user records" = stuck by definition; needs live flag-only validation because history cannot show teammate idleness.

#### 3c. Plan: teammate idle with running task (implemented)

- Prevents the trigger instead of detecting it. TeammateIdle hook resolves the teammate transcript `<session-dir>/subagents/agent-<agent_id>.jsonl`, parses both launch forms (explicit background and timeout-moved), and exits 2 (keep working) while a task it launched is running, telling it to wait in the foreground with `timeout: 600000` and a sub-600 s loop on `lsof -t PATH`. A task that ended without its notice is reported once, then recorded as acknowledged under `~/.local/state/agent-teammate-idle/`. Unknown shapes, missing `agent_id` or transcript allow and report. No block counter: a never-ending task keeps the teammate visibly busy.
- **Signal decision.** Step 0 found `[exited with code N]` in only 145 of 235 non-empty output files (90 unclassified), and command output can fake it; Codex review replaced it with "running = some process holds the output file open" (`lsof`), observed on the hung `rg` holding its `.output` on fds 1 and 2. Ended = no holder, whoever stopped it.
- **Facts.** TeammateIdle has `teammate_name` and (v2.1.290+) `agent_id`, no `background_tasks`; SubagentStop's `background_tasks` is parent-scoped with no owner. Headless `claude -p` runs named agents as plain subagents with no TeammateIdle (inbox `claude-af29dec8`); team behaviour needed an interactive CLI under `expect`.
- **Gaps.** The undelivered-notice branch is fixture-only (cannot be produced on demand). Lead and plain subagents not covered. Long-lived teammate-started servers are held busy.

### 4. Key empirical findings (from the plans)

- **3,962 background tasks** in every Claude transcript since 2026-09-12: 1,937 `run_in_background`, 1,765 auto-backgrounded after the Bash timeout, 252 Monitor. 535 never reported completion (73 of 175 sessions); 347 of those were TaskStop-ed, 188 abandoned. Abandoned causes are heterogeneous: never-matching string-in-file loops, sibling pgrep deadlocks, long jobs outliving subagents, `tail -f` Monitors (overcounted: Monitor timeouts leave no marker), stdin readers, file-exists loops. Hand-written until/while loops fail at 14% vs 13% for other tasks, so loops are not the cause. 447 of 535 were in subagent transcripts but surface in the parent's `background_tasks`.
- **Claude Code facts used.** No time limit on interactive background Bash; Monitor deadline 5 min default, 30 max; no hook on task start/end; Stop/SubagentStop carry undocumented-in-practice `background_tasks` (id, type, status, description, command; no start time) and `last_assistant_message`; idle teammates are reported `running`.
- **Queue replay:** see 3b; queue records cannot be paired reliably.
- **lsof writer test:** a running shell task's output file has an open writer; an ended one has none; output text is irrelevant.
- **Wait cap:** a 115-round foreground wait overran 600 s and was itself backgrounded; 100 rounds fits.

### 5. Learning-inbox records (`.agent-learnings/inbox/`, none promoted as far as observed)

- `claude-code-a65e8d68` (2026-10-03, agent repo): C1 a 2026-10-01 session concluded from docs that Claude Code reports background tasks nowhere and built a transcript scanner; a two-minute `claude -p --settings` dump showed `background_tasks` in Stop/SubagentStop payloads (package hooks had been discarding stdin). C2 **"This should be the permanent fix" (2026-10-01) broke two days later**; forbid permanent/robust claims on self-authored-fixture evidence. C3 env-inherited session identity fires hooks for child `claude -p`; check payload `session_id`.
- `claude-code-5977de78` (2026-10-04): C1 a "fixed" claim about session-state colours could not answer "why only this session" (tangodb used thousands of background subagents; others used shells, which fire no hooks); name the discriminating factor before calling a selective bug fixed. C2 instrument instead of compensating; a trace advice logging to `~/.local/state/agent-modeline-trace.log` was installed and awaits recurrence. C7 ground third-party claims in docs/code.
- `claude-code-336425b6` (2026-10-04): status was correct (three running shells >5 h) but read as false; propose showing oldest task age. Rejected in 3a (no start time in payload).
- `claude-f153f33c` (2026-10-01): C1 ask a session when its claim conflicts with evidence; C2 **first report of the teammate-notice stall** (later 3b/3c); C4 live verification missed old-shape buffer-local state reloaded into eight live sessions; C5 `json-end-of-file` escaped `json-parse-error` and permanently killed a poll timer (fixed agent `631e910`), plausibly behind earlier stale colours.
- `claude-code-69e421c0` (2026-10-05): status-column design decisions scattered across 09-20/21, 10-01, 10-05; record a decision record (agent text not summarizer, no stricter cadence, Stop hook deferred, no new hooks/CLAUDE.md lines).
- `claude-baac04d1` (2026-10-05): agent-status refusal guidance stale after backlog fix (section 1).
- `claude-b45fde2a`, `claude-contextonly-24ca95ab` (2026-09-30): agents skip `agent-status`; propose hook baseline. `claude-code-2c4fe979` (2026-09-30): agent-status timed out at 10 s when Emacs was busy. `claude-746aad7f` (2026-10-01): agent-status failure with no Emacs process and a stale socket; "this keeps happening and earlier fixes had been claimed".
- `claude-code-f44df81c` (2026-10-10, meetings): C2 bounded, non-self-matching waits; poll output files instead of relying on notifications (its "matches its own shell" premise was later shown wrong).
- `claude-70afc41c` (2026-10-10, agent repo): C1 base recurring-incident diagnosis on history (first two answers, built on two live sessions and an untested guard-message mechanism, were wrong; the 4,000-task audit overturned them); C2 test a wait loop's predicate before backgrounding it (a stale background job was created during the fix for stale background jobs; 25 identical void-function errors before TaskStop); `agent-send --list` is the right probe for session state.
- `claude-af29dec8` (2026-10-10): `claude -p` does not exercise teams; no sanctioned long-running test command (`bin/test-busy` proposed).
- `claude-d76932e2` (2026-10-01) C5: long agent-run jobs must not depend on the background-task time limit.

### 6. Pattern

Repeated "fixed" claims (2026-10-01 "permanent fix", 2026-10-04 colours) were each followed by a new class of the same symptom. The 2026-10-10 work moved enforcement to the agent side (Stop and TeammateIdle hooks) after establishing that agent.el reported Claude Code's task list faithfully; the undelivered-notice upstream defect remains undetected except for teammates covered by the hold hook.
