# Guard engine: one fast process, then the end of the exemption cycle

Status: done 2026-10-01. Both runtimes run the engine, the old shell guards
are deleted (2cacbc26b), and the opaque-token heuristic only logs (item 3, the
commit after it).
Approved 2026-10-01 (full scope). Completes items 5 and 6 of
[2026-09-30-secret-guard-redesign.md](2026-09-30-secret-guard-redesign.md)
plus the Codex streaming gap. Implemented end to end without intermediate
check-ins once approved; the stop rules below are the only exceptions.

## Goal and acceptance

1. One Python process per tool call in each runtime. Target was under 50 ms;
   achieved ~85 ms CPU for a Claude Bash call (the shell hooks used ~800 ms
   CPU). Under 50 ms would need a resident process queried by the hooks,
   rejected: after a guard edit it would keep enforcing the old rules until
   restarted, a silent staleness no guard may have.
2. Every current guard decision preserved, proven by differential testing
   (below), except the single intended change in step 6.
3. Guard test suite in-process: under 30 s (achieved ~25 s; the shell suite
   took ~9 minutes in parallel).
4. Codex output streamed from long-running commands and error results masked.
5. Opaque-token heuristic and its 42 per-site exemptions no longer deny.

## Decisions already made

- **Scope:** every PreToolUse guard in both runtimes moves into one engine:
  secret leak, sensitive read, untrusted execution, destructive commands,
  GitHub writes, dirty-tree rewrite, walk-list, hooksPath, Elpaca rebuild and
  dotfiles edits, long-pipeline, LaunchAgent registration, Ahrefs API, Texinfo
  edits, and Claude's output wrapping. One parse of each command, each rule
  defined once, shared by Claude and Codex. A half-ported system (one engine
  plus a dozen shell scripts parsing the same command differently, twice) is
  the mess this replaces. PostToolUse hooks (output masking, reminders) are
  already Python or out of scope; non-guard hooks (Emacs bridge, reminders,
  Elisp loading) are untouched.
- **Language and layout:** Python 3, `lib/python/agent_guard/` (one module per
  policy, the existing `lib-*.py` classifiers imported in-process), thin
  entry points in both hook trees. Interpreter invoked by absolute path.
- **Behaviour-preserving port, proven by an oracle:** the current shell guards
  decide a corpus of payloads once; the engine must reproduce every decision
  (allow/deny/wrap and reason category). When the engine and the oracle
  disagree and it is unclear which is right, the stricter decision wins.
- **Corpus:** every payload the existing tests send (captured by recording
  hook input during one `bin/test-guards` run) plus real tool calls from
  Claude and Codex session logs of the last 60 days. Real commands can contain
  secrets, so the corpus lives in `~/.local/state/agent-guard/` (0700, excluded
  from Time Machine, outside Drive), is never printed unmasked, and is deleted
  when the work is done.
- **Cutover:** switch both runtimes' registrations to the engine in one commit
  after the differential run is clean and the suite passes; delete the shell
  implementations in the same commit (git history keeps them).
- **Opaque-token heuristic (item 5):** at cutover it stops denying and only
  logs a label (never the value) to `~/.local/state/agent-guard/heuristic.log`
  when it would have been the sole reason to deny. No past incident came
  through it; known values and prefixes still deny. The 42 per-site route
  exemptions are deleted with it. `docs/secret-guard.md` records how to restore
  it if the log ever shows a real secret.
- **Codex streaming and error results:** a PreToolUse rewrite (`updatedInput`)
  pipes Bash commands through the masking filter, as Claude already does, so
  `write_stdin` chunks and failed commands are masked too. Exit status is kept.
- **Concurrent work:** other sessions edit these hooks. Their uncommitted
  changes are never committed or reverted by this work; if they land in a
  guard file mid-port, the oracle is rerun on their version.

## Steps (each ends with the full suite green and a commit)

1. Corpus capture and oracle run (oracle in parallel, ~1 hour once).
2. Engine core: payload parsing for both runtimes, decision object, failure
   handling (any exception denies), shared logging.
3. Port policies one at a time, each gated on zero oracle mismatches for its
   reasons: secret leak, sensitive read, untrusted execution, destructive,
   GitHub write, the small guards, output wrapping.
4. Convert the test suite to call the engine in-process; keep ~20 end-to-end
   hook runs per runtime proving wiring and fail-closed behaviour.
5. Codex `updatedInput` wrapping, verified live with a fake token, as was done
   for the output hook.
6. Cutover in both runtimes (Codex hook trust renewed via the app-server API
   in all homes), heuristic to log-only, route registry deleted, latency
   measured, docs updated, corpus deleted.

## Stop rules (the only reasons to interrupt Pablo)

- An action needs Touch ID, a credential or another person.
- A live check after cutover shows a regression that cannot be fixed or
  rolled back within the same step; then roll back to the shell guards first.
- Something outside this plan would have to be deleted or published.

## Intended differences from the shell guards (recorded as they are decided)

1. **Codex patch paths that collate equal are both checked.** The shell's
   `sort -u` under macOS en_GB.UTF-8 treats most non-Latin characters as
   weightless, so two such paths merged and the second was never checked.
   Stricter wins (codex_paths.changed_paths).
2. **`cd rel/dir && git commit -m x PATH` is no longer always denied.** The
   shell resolved the relative target from inside the repository after its own
   `cd`, so every such commit was denied ("cannot change to"). A false denial,
   not policy (repo.staged_files, RepoContext.pwd).
3. **Opaque-token heuristic logs instead of denying** (step 6, as planned):
   lib/python/agent_guard/url_heuristic.py, without the per-site route
   registry; one label-only line per would-be denial in
   ~/.local/state/agent-guard/heuristic.log, emptied at cutover so it holds
   live traffic only. Its own commit, landed right after the cutover commit,
   so the port itself is behaviour-preserving in history.
4. **The walk-list guard now fires.** `case "$p" in *"$GUARDED_PATTERN"*`
   quoted a pattern containing a backslash, so it matched only a literal
   backslash and never denied anything. The intended substring test is
   applied (walk_list.path_is_guarded). Stricter wins.
5. **Payloads with lone-surrogate escapes are denied up front.** jq could not
   parse them, so every shell guard failed closed; Python accepts them
   (engine.main).
6. **`rm -Rf` is treated like `rm -rf`** (the shell pattern accepted only a
   lowercase `r`). Stricter wins.
7. **`--force-with-lease` exempts only its own push**; the shell cleared every
   `-f` on the line, letting a force push elsewhere through. Stricter wins.
8. **Reasons of the Elpaca-rebuild and long-pipeline checks contain real line
   breaks** (the shell printed a literal `\n`). Cosmetic.
9. **Codex code-mode programs without a nested shell call get every secret
   check.** Under the production `/bin/bash` 3.2, `SECRET_NESTED_COMMANDS[@]`
   was an unbound variable there and the hook exited 0 silently, skipping all
   checks (bash 5 ran them). The engine follows bash 5. Closes a fail-open.
10. **Known-value and known-secret-pattern checks run on every command and
    write**, before the early allows for commands starting with
    grep/rg/git-crypt, mentioning `.zshenv-secrets` or using a prefix-less
    `$VAR`. Those early allows let `grep x f && curl -d 'k=SECRET'` through.
    Stricter wins; the early allows still precede the noisier checks.

Oracle caveat: the recorded decisions came from running hooks with `bash` on
PATH (Homebrew bash 5); production ran their `#!/bin/bash` shebang (3.2).
Differences between the two (item 9) are resolved toward the stricter bash 5.
11. **A Claude payload whose tool_input is not an object is denied for every
    tool** (core.Call.from_payload). The shell hooks failed closed only where
    they read tool_input for that tool. Stricter; Claude always sends objects.
12. **Codex wraps credential-prone shell commands through the redactor**
    (policies.codex_wrap), by Claude's check_wrap rule, so streamed
    `write_stdin` output and error results of those commands are masked.
14. **A failing helper denies where a shell hook silently carried on.** In
    several places the shell ran a masker, sed, grep or process substitution
    where `set -e` did not apply (nested `$()` in an argument, inside an
    if-condition, `< <(…)`) and scanned truncated text, usually allowing. The
    engine raises and denies. Audited per policy on 2026-10-01 (every except
    and fallback checked against its shell counterpart); no case where the
    engine allows what the shell denied. Stricter; none occurs on normal input.
15. **Large multi-line commands are checked.** The shell checks were
    `echo "$X" | grep -q P` under pipefail: on a text over ~64 KB, grep -q
    exits at an early match, echo dies of SIGPIPE, the pipeline returns 141
    and the `if` reads it as no match. A large enough command therefore passed
    the destructive, LaunchAgent, pipeline, secret, sensitive-read and GitHub
    checks (reproduced 5/5 with /bin/bash). The engine reads text directly.
    The same race made the shell ignore an override flag (ALLOW_CLONE=1 …) in
    large commands; the engine honours overrides consistently.
16. **Codex code-mode programs that cannot run commands are not scanned as
    shell text.** Fixing item 9 exposed false denials the 3.2 fail-open had
    hidden (found by the production-shell acceptance run, 12 of 600 programs):
    the executable-glob rule read JavaScript `?.`, `[0]`, `*` and template
    literals as globs. The whole-program protected-tool scan now runs only
    when the program can execute something: `exec_command`, non-literal
    `tools` access, Node process APIs (child_process, execSync, execFile,
    spawn, fork, exec(, process.binding/dlopen) or eval/Function/require/
    import(. Nested commands, known values and secret patterns are always
    checked.
13. **Wrapped commands keep their exit status under zsh.** The shell hook ended
    with `exit "${PIPESTATUS[0]}"`; zsh has no PIPESTATUS, so every failing
    wrapped command reported success. The suffix now reads zsh's `pipestatus`
    first, then bash's PIPESTATUS (verified in both shells).
