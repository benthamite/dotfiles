# Plan: secret-leak guard, deny printing a 1Password secret instead of invoking 1Password

**Goal.** Remove the friction the 2026-08-31 guard change introduced, agents can
no longer run `op-automations run --env-file .env.op -- <program>` or any other
broker command from a Claude Code or Codex shell, without eroding the concern
the guard exists for: no credential value may reach agent output (the tool
result, and so the transcript).

**Deliverable under review.** This plan and the proposed hook patch committed
beside it at `docs/superpowers/plans/2026-09-02-secret-guard-op-output-policy.patch`
(read it with `git show <commit>:docs/superpowers/plans/2026-09-02-secret-guard-op-output-policy.patch`).
The patch is the *guard* half of the change only; the test and documentation
edits are described below and are written after review.

**Status.** Awaiting one Codex review pass. Nothing from this plan is applied
to the working tree; the two hook files are at HEAD.

## Diagnosis (why the guard is shaped this way today)

- Commit `875a20b95` (2026-08-31, "agents: deny direct secret-output commands")
  made `claude/hooks/block-secret-leak.sh` and its Codex twin deny any agent
  shell command whose executable is `op`, `op-automations`, `op-desktop`,
  `pass`, `security` or `pbpaste`, whatever the subcommand, including
  `op-automations run`. It rewrote `claude/context/secrets.md`, four READMEs and
  `ai-config-sync.json` to state the new rule, and added tests that pin it
  (`tests/test_secret_guard_parity.py`, `tests/test_op_automations.py`, e.g.
  `test_guards_deny_direct_op_run`).
- The commit came out of a Codex `$security-audit` session. The owner's
  instructions in that session were "proceed" on remediating critical and
  high findings, then complaints about a broken hook. The tightening itself was
  the audit agent's decision, recorded in its own words: "`op run` can launch
  an environment dumper, and `op inject` prints resolved secrets by default.
  I'm tightening the rule again: direct agent-shell secret provisioning will be
  denied entirely, leaving only audited non-printing wrappers." No user
  instruction asked for invocation to be forbidden; the standing rule in
  `secrets.md` is "never echo or print them".
- Before that commit the policy was: raw `op` denied (Touch ID routing, see
  `secrets.md`), brokers allowed, `--reveal` denied, unfiltered
  `item list|get` output denied unless piped through `jq` or redirected
  (`git show 875a20b95^:tests/test_secret_guard_parity.py`).
- Effect since: the 2026-09-02 automations-dashboard session hit the denial at
  least eight times and routed around it through `make` targets that call
  `op-automations` internally (which the guard cannot see); the email-triage
  session hit it four times and had to ask the user to run a command with `!`.
  Routing around a guard is worse for the security concern than a guard that
  understands the command.

Category: structural gap (the mechanism enforces a stricter rule than the
owner's), plus a process gap (an audit agent changed policy semantics and the
policy prose in one commit with no explicit owner decision).

## Design principle

Classify a 1Password command by **where its stdout goes**, not by which
binary runs. The brokers are the sanctioned access path; the guard denies
exactly the forms whose stdout would carry a credential into agent output.
Where the guard cannot see the subcommand (nested shell program, variable or
`command -v` indirection, executable globs, quote or backslash obfuscation) it
keeps denying, as today.

### Rules after the change

Allowed (currently denied):

| Form | Why it is safe |
|---|---|
| `op-automations run --env-file F -- <program> [args]` | `op run` masks every injected value in the child's stdout/stderr; the child sees them only as environment. |
| `op-automations read REF > file`, `… \| consumer`, `X=$(op-automations read REF)` | stdout is captured, redirected or piped; nothing reaches the tool result. Bash tool calls run in fresh shells, so a captured variable cannot leak into a later call. |
| `op-desktop item get ID --format=json \| jq '<filter>'`, `… > file` | the pre-08-31 rule; the agent's `jq` filter selects non-secret fields. |
| `op-desktop document get ID --out-file F` / `> F`, `op-automations inject --in-file F --out-file G` | written to a file, not printed. |
| `op-desktop item create/edit …` without `--format` | default output masks concealed fields; writes still go through the `store-secret` skill by convention, but the hook no longer pretends that is a printing concern. |
| `op-desktop --status`, `--stop`, `whoami`, `vault list` | no credential data. |

Denied (each because stdout is the secret):

| Form | Note |
|---|---|
| raw `op …` in any spelling the current guard catches, plus quote/backslash obfuscation (`'op' read`, `o\p read`) | unchanged; the Touch ID routing rule, message again points at the brokers. |
| bare `op-* read REF`, also with only `2>` redirected, or followed by `&&`/`;` | prints the value. |
| `echo $(op-* …)`, `` echo `op-* …` `` | prints a captured value. |
| `op-* item list|get` without `\| jq` or a stdout redirect | item summaries can carry secret-valued URL fields; JSON carries values. |
| `op-* item create|edit … --format …` unredirected | JSON output carries values. |
| `op-* document get` without `--out-file`/redirect; `op-* inject` without `--out-file`/redirect | print the document / the rendered template. |
| `op-* run --no-masking …`; `op-* run … -- env|printenv|set|export|declare|typeset` | the two fail-open paths the 08-31 review found, denied narrowly. |
| `--reveal` anywhere (extended to `op-desktop`, which the current detector misses) | prints a concealed field. |
| `op-* item share`, `service-account create`, `connect token|server create`, `events-api create` | print an access link or a freshly minted credential. |
| broker named inside `bash -c '…'`/`eval`, reached through `OP=…; "$OP" …`, `$(command -v op-*)`, `$(which …)`, `$(type -P …)`; executable globs (`op-auto?ations`) | subcommand invisible to the guard; fail closed, as today. |
| `pass`, `security`, `pbpaste` | unchanged blanket rule; out of scope (see open decisions). |

Global flags before the subcommand (`op-desktop --format=json item get …`,
`op-automations --account acct read …`) are parsed: a flag without `=` takes
the next word as its value unless that word is a known subcommand.

### Residual risks, stated

- `op run` masking is 1Password's guarantee, not ours; a child could encode a
  value before printing it. The same is true of any program an agent runs
  after `X=$(op-automations read …)`, and of the `make` targets agents are
  using today. The guard's contract is "no credential appears in agent output
  by construction of the command"; it never claimed to stop a determined
  exfiltration.
- Piping `item list` output to a `jq` filter that selects everything
  (`jq .`) prints it. Pre-08-31 accepted this; the alternative is parsing
  `jq` programs. Accepted.
- `op-* read REF | tee`/`cat`/`head` prints. "Piped" is treated as consumed.
  Pre-08-31 behaviour; accepted, with the denial list covering the common
  direct forms.

## Implementation steps

1. **Apply the committed patch** to both guard copies (`git apply
   docs/superpowers/plans/2026-09-02-secret-guard-op-output-policy.patch`).
   It: removes the op family from the blanket-deny sets in
   `contains_secret_output_command` (keeping `pbpaste|pass|security`); adds
   `contains_op_secret_output` (segment split on `;`, `&&`, `||`, `&` and
   newlines, quoted literals masked, stderr redirects stripped, `$(` marked as
   captured, global flags parsed, per-subcommand stdout rules above) and
   `contains_normalized_raw_op` (raw `op` spelled via quotes or backslashes);
   extends the `--reveal` detector to `op-desktop`; wires both into the Bash
   gates (Codex: also every literal nested `functions.exec` command); restores
   the raw-`op` message to the broker advice; new `deny_op_secret_output`
   message tells the agent how to keep the value out of output. The dispatcher
   `pretooluse-bash.sh` delegates to the standalone hook, so it needs no edit.
2. **Sensitive-read guard, one narrow allowance** (`pretooluse-bash.sh`,
   `check_sensitive_read`): a command whose only "composition" is an
   `op-automations run --env-file <file> -- <program>` invocation preceded by
   `VAR=value` assignments and/or followed by stdout/stderr file redirects is
   an environment *loader* and gets the same treatment as `source .env`
   (allowed, with the existing `ALLOW_CONTEXT` warning). Today the `2> log`
   redirect alone makes it "compound" and denied. Env dumpers as the program
   stay denied by step 1.
3. **Tests.** Rewrite the pinned expectations to the table above:
   - `tests/test_secret_guard_parity.py`: docstring; `test_op_automations_read_is_denied`
     and `test_op_desktop_read_is_denied` become contained-read *allowed*
     cases; `test_secret_provisioning_commands_are_denied` becomes
     "printing broker output is denied" (bare read, `2>`-only read, `run --
     printenv`, `run -- env`, `run --no-masking`, `inject` without out-file,
     `document get`, `item share`) plus a new "masked run and file outputs are
     allowed" test; `test_filtering_or_redirecting_op_item_output_does_not_bypass_guard`
     flips back to the pre-08-31 allowed case;
     `test_direct_broker_writes_require_an_audited_wrapper` flips to allowed
     (no `--format`) with a denied `--format=json` sibling; every
     obfuscation, nested-shell, indirection, glob, `pass`/`security`/`pbpaste`
     and `--reveal` test stays as deny. Add the global-flag cases.
   - `tests/test_op_automations.py`: `assert_denied` checks the decision, not
     the old message text; `test_guards_deny_direct_op_run` becomes
     `test_guards_allow_masked_op_run`; the nested-command deny list drops
     `read … > /dev/null` and `item list … | jq .`, keeps the rest.
   - `tests/test_security_hook_hardening.py` and `tests/test_op_routing.py`:
     unchanged; run them.
   - Decision matrix used during design (80 commands across the standalone
     Claude hook, the dispatcher and the Codex hook) is folded into the parity
     test as data.
4. **Documentation.** Revert the policy prose to "invoke through the brokers,
   never print": `claude/context/secrets.md` (the two Epoch bullets and the
   three broker paragraphs), `claude/hooks/README.org`, `codex/hooks/README.org`,
   `claude/README.org` §secret guard, `codex/README.org` §secret guard, and the
   `block-secret-leak.sh` note in `ai-config-sync.json`. Each states the
   principle (stdout destination), the denied forms, and the residual risks.
5. **Process fix for the second gap.** Add one sentence to the `security-audit`
   skill (`macos/.codex/skills/security-audit/SKILL.md` and its Claude pair if
   any): a remediation that changes what a guard *permits*, rather than fixing
   a defect in how it enforces the existing rule, is a policy decision; state
   the proposed rule and get an explicit yes before committing it or rewriting
   policy prose. No mechanism is proposed beyond this; if the reviewer sees a
   cheap enforceable one (for example a commit hook requiring a
   `Policy-Decision:` trailer on changes to `*/hooks/block-secret-leak.sh`),
   say so.
6. **Commit** as two single-purpose dotfiles commits: (a) guards + tests +
   docs + sync manifest; (b) the audit-skill sentence. Both hook copies change
   in the same commit (`ai-config-sync` pairing).

## Verification

- `python3 -m pytest tests/test_secret_guard_parity.py tests/test_op_automations.py tests/test_security_hook_hardening.py tests/test_op_routing.py -q` green.
- `bin/ai-config-sync audit` clean; `bash -n` on both hooks; `shellcheck` on
  both hooks shows no new findings versus HEAD.
- Live check of the original blocked task, from the email-triage repo:
  `op-automations run --env-file <abs>/.env.op -- python3 <abs>/claude_assignment_bridge.py --redraft …`
  runs through the Claude Bash tool without a denial and its tool output
  contains no credential (the bridge prints one JSON status line).
- Negative live check: `op-automations read op://Automations/<item>/credential`
  typed as a Bash tool call is denied with the new message.

## Open decisions for the owner

- `pass` and `security` are still blanket-denied by the 08-31 rule. The same
  argument applies to them (`X=$(pass show env/foo)` is the access pattern for
  personal secrets). Left unchanged here so this plan stays about 1Password;
  recommend a follow-up with the same stdout-based rule.
- Raw `op` remains denied. The zsh shim already routes bare `op` to the right
  broker in interactive shells, and agent Bash calls reach the `.zshenv` `op`
  function too, so the denial is belt-and-braces for the Touch ID concern. It
  is the one remaining "invocation" rule; keeping it is a judgment call, not a
  security necessity.
