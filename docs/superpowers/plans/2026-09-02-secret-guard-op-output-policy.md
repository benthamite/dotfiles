# Plan: secret-leak guard, deny printing a 1Password secret instead of invoking 1Password

**Goal.** Remove the friction the 2026-08-31 guard change introduced, agents can
no longer run `op-automations run --env-file .env.op -- <program>` or any other
broker command from a Claude Code or Codex shell, without eroding the concern
the guard exists for: no credential value may reach agent output (the tool
result, and so the transcript).

**Status.** Revision 2 after one Codex review pass of revision 1 (commit
`8c8e3e99e`, blob `02a29b02`). Revision 1 classified broker commands by
"where stdout goes" with a deny list; the review showed that model fails open
in five places (adjudication record at the end). Revision 2 inverts it: an
**allowlist** of broker command shapes, everything else denied. Approved by
the owner ("go", 2026-09-02) and implemented the same day; the revision-1
patch committed beside this plan is superseded and kept only as the reviewed
artifact. Implementation notes: the classifier is `claude/hooks/lib-op-policy.py`
(paired for Codex) with its case table in `tests/test_op_policy.py`; the
standalone sensitive-read copies keep allowing any leading broker command
(their scope is file reads), while the Bash dispatcher combines both guards,
so the "program prints its environment" denial is pinned on the dispatcher.

## Diagnosis (why the guard is shaped this way today)

- Commit `875a20b95` (2026-08-31, "agents: deny direct secret-output commands")
  made `claude/hooks/block-secret-leak.sh` and its Codex twin deny any agent
  shell command whose executable is `op`, `op-automations`, `op-desktop`,
  `pass`, `security` or `pbpaste`, whatever the subcommand, including
  `op-automations run`. It rewrote `claude/context/secrets.md`, four READMEs and
  `ai-config-sync.json` to state the new rule, and added tests that pin it.
- The commit came out of a Codex `$security-audit` session. The owner's
  instructions in that session were "proceed" on remediating critical and
  high findings, then complaints about a broken hook. The tightening itself was
  the audit agent's decision, in its own words: "`op run` can launch an
  environment dumper, and `op inject` prints resolved secrets by default. I'm
  tightening the rule again: direct agent-shell secret provisioning will be
  denied entirely, leaving only audited non-printing wrappers." No user
  instruction asked for invocation to be forbidden; the standing rule in
  `secrets.md` is "never echo or print them".
- Before that commit: raw `op` denied (Touch ID routing), brokers allowed,
  `--reveal` denied, unfiltered `item list|get` denied unless `| jq` or
  redirected.
- Effect since: the 2026-09-02 automations-dashboard session hit the denial at
  least eight times and routed around it through `make` targets that call
  `op-automations` internally, which the guard cannot see; the email-triage
  session hit it four times. Routing around a guard is worse for the security
  concern than a guard that understands the command.

Category: structural gap (the mechanism enforces a stricter rule than the
owner's), plus a process gap (an audit agent changed policy semantics and the
policy prose in one commit with no explicit owner decision).

## Design

### Principle

The brokers `op-automations` and `op-desktop` are the sanctioned access path.
The guard permits a **closed list of command shapes whose stdout provably
carries no credential** and denies every other broker invocation, including
shapes it cannot classify. Raw `op` stays denied (Touch ID routing; the shim
routes it in interactive shells anyway). `pass`, `security`, `pbpaste` keep
today's blanket rule (out of scope, see open decisions).

### Recognition, fail closed

1. Mask quoted literals and sink-fed heredoc bodies (existing helpers). Also
   classify the quote/backslash-normalized form (`normalize_shell_words`), so
   `op-'automations'` and `op\-automations` are seen.
2. Split into simple commands on every control token: newline, `;`, `&`,
   `&&`, `|`, `||`, `(`, `)`, `{`, `}`, `!`, and the reserved words `if`,
   `then`, `elif`, `else`, `while`, `until`, `do`. A simple command whose first
   word after wrapper prefixes (`env`, `sudo`, `command`, `timeout`, `nice`,
   `exec`, `nohup`, `time`, `builtin`, `VAR=value`) is a broker is an
   *invocation*. This covers `true | op-* read`, `{ op-* read; }`,
   `if …; then op-* read; fi`, `! op-* read`.
3. Every invocation is classified against the allowlist below. Then, if the
   broker word still occurs anywhere in the masked, normalized command outside
   a classified invocation (`<(op-* …)`, `>(…)`, `"$OP" …`, `$(command -v
   op-*)`, inside a `bash -c '…'`/`eval` program, as an argument to another
   command), the command is denied as unclassified.
4. Any env-prefix assignment to an `OP_*` variable on an invocation, or the
   string `OP_RUN_NO_MASKING` anywhere in the command, denies. (`op run`
   honours `OP_RUN_NO_MASKING`, CLI release 2.30.3; both brokers pass the
   environment through.)

### Allowlist

| Shape | Conditions |
|---|---|
| `run --env-file <path> [--env-file …] -- <program> [args]` | no `--no-masking`; program's basename not in {`env`, `printenv`, `set`, `export`, `declare`, `typeset`, `bash`, `sh`, `zsh`, `dash`, `ksh`, `eval`}; stdout/stderr redirects only to regular paths (below). Masking of injected values in the child's output is 1Password's guarantee. |
| `read <ref>` captured: `VAR=$(… read …)` or `` VAR=`…` `` | subject to the same-call reuse rule below. |
| `read <ref> > <regular path>` | regular path only. |
| `read <ref> \| <consumer>` | consumer's first words in {`pbcopy`, `gh secret set`, `wrangler secret put`, `op-desktop item create`, `op-desktop item edit`, `ssh-add -`, `docker login … --password-stdin`, `gpg --import`}. |
| `item get|list … \| jq <filter>` | filter references only metadata keys: `id`, `title`, `category`, `vault`, `name`, `created_at`, `updated_at`, `last_edited_by`, `tags`, `version`, `fields`, `label`, `purpose`, `type`, `section`; every `.key` and every bare key inside `{…}` must be in that set; at least one key selected; none of `.` alone, `.[]` alone, `..`, `to_entries`, `tojson`, `tostring`, `@…`, `env`, `input`, `value`, `password`. No `--fields`, no `--reveal`. |
| `item get|list … > <regular path>` | regular path only. |
| `item create|edit …` | no `--format`, no `--reveal` (default output masks concealed fields). |
| `item delete`, `document create|edit|delete`, `vault create` | write-only shapes; no credential output. |
| `document get … --out-file <regular path>`, `inject … --out-file <regular path>` | regular path only; no redirect-to-terminal. |
| `whoami`, `vault list|get`, `user list|get`, `group list|get`, `account list`, `document list`, `item template list|get`, `--status`, `--stop`, `--version` | metadata only. |

Everything else is denied as unclassified. That includes, deliberately:
bare `read`; `read 2>/dev/null`; `read \| cat|tee|head|…`; `item get --fields …`;
`item share`; `signin` (`--raw` prints a session token); `environment read`
(prints variable values); `service-account create`, `connect …`, `events-api
create`; `document get` without `--out-file`; `inject` to stdout; `--reveal`
anywhere; `run --no-masking`; `run -- env`; global flags the parser does not
consume; any subcommand not in the table (future CLI additions fail closed).

**Regular path**: a redirect or `--out-file` target that is not `-`, does not
begin with `/dev/` (except `/dev/null`), `/proc/`, `/dev/fd/`, is not `>&N`,
`&>`-to-terminal, `>(…)`, and contains no expansion of a captured variable.

**Same-call reuse**: for each capture `VAR=$(broker …)`, deny if `$VAR`,
`${VAR…}`, or `${!…}` appears in any later simple command whose first word is
in {`echo`, `printf`, `cat`, `tee`, `head`, `tail`, `less`, `more`, `xxd`,
`od`, `base64`, `jq`, `awk`, `sed`, `cut`, `tr`, `rev`, `fold`, `python*`,
`node`, `perl`, `ruby`, `bash`, `sh`, `zsh`, `eval`, `xargs`}, in a here-string
`<<<`, or as a redirect target. Bash tool calls run in fresh shells, so a
captured variable cannot cross to a later tool call.

**Global flags**: a flag without `=` consumes the next word as its value unless
that word is a known subcommand; unknown subcommands after flag parsing deny.

### Implementation form

The classifier is one Python module, `claude/hooks/lib-op-policy.py`, paired
identically as `codex/hooks/lib-op-policy.py` (`ai-config-sync` pair). Both
`block-secret-leak.sh` copies call it only when a cheap `grep` finds a broker
word, so ordinary commands pay no extra spawn. Python gives a real tokenizer
for the split-and-classify above; the bash/awk regex approach of revision 1 is
what produced the fail-open findings. The module exposes
`classify(command) -> ("allow" | ("deny", reason))` and a `--self-test` that
runs the case table in `tests/`.

### Residual risks, stated

- `op run` masking is 1Password's guarantee; a child program could transform
  a value before printing it. The same is true of any program run after a
  captured read and of the `make` targets agents use today. The guard's
  contract is "no credential appears in agent output by construction of the
  command"; it does not stop a determined exfiltration by the program.
- The `jq` metadata-key rule is a whitelist of key names, not a `jq` parser.
  Filters that compute over values without naming a key (`map(length)`) are
  denied because they select no allowed key, which is the safe direction.

## Implementation steps

1. **Classifier.** Write `claude/hooks/lib-op-policy.py` (paired Codex copy)
   with the recognition and allowlist above, plus its case table as data in
   `tests/test_op_policy.py` (every allow/deny row in this plan, the review's
   adversarial cases, and the decision matrix from revision 1).
2. **Guards.** In both `block-secret-leak.sh` copies: remove the op family
   from the blanket-deny sets (`pbpaste|pass|security` stay); call the
   classifier when a broker word is present and emit `deny_op_secret_output`
   with the classifier's reason; keep and extend the `--reveal` detector to
   `op-desktop`; restore the raw-`op` message to the broker advice. Codex:
   classify every literal nested `functions.exec` command as today. The
   Claude dispatcher `pretooluse-bash.sh` delegates to the standalone hook and
   needs no policy edit.
3. **Sensitive-read guard, all three implementations** (`claude/hooks/pretooluse-bash.sh`,
   `claude/hooks/block-sensitive-read.sh`, `codex/hooks/block-sensitive-read.sh`):
   a command of the shape `(VAR=value )* op-automations run --env-file <path>
   -- <program…>` with optional stdout/stderr redirects to regular paths is an
   environment loader (same treatment as `source .env`: allowed with the
   `ALLOW_CONTEXT` warning) when no `VAR` starts with `OP_` and the program
   passes step 1's `run` rule. Today the `2> log` alone makes it "compound".
4. **Tests.** `tests/test_op_policy.py` (new); rewrite the pinned expectations
   in `tests/test_secret_guard_parity.py` and `tests/test_op_automations.py`
   to the table above, keeping every obfuscation, nested-shell, indirection,
   glob, `pass`/`security`/`pbpaste` and `--reveal` case as deny; add the
   loader cases to `tests/test_claude_bash_pretooluse.py` and the
   sensitive-read tests for the standalone copies; run
   `tests/test_security_hook_hardening.py` and `tests/test_op_routing.py`
   unchanged.
5. **Documentation.** `claude/context/secrets.md` (the two Epoch bullets and
   the three broker paragraphs), `claude/hooks/README.org`,
   `codex/hooks/README.org`, `claude/README.org` and `codex/README.org` secret
   guard sections, and the `block-secret-leak.sh` note in
   `ai-config-sync.json`: principle (closed allowlist of non-printing shapes),
   the table, the residual risks.
6. **Process fix for the second gap.** One sentence in the `security-audit`
   skill (`macos/.codex/skills/security-audit/SKILL.md` and its Claude pair if
   any): a remediation that changes what a guard *permits*, rather than fixing
   how it enforces the existing rule, is a policy decision; state the proposed
   rule and get an explicit yes before committing it or rewriting policy prose.
7. **Commit** as two single-purpose dotfiles commits: (a) classifier, guards,
   sensitive-read allowance, tests, docs, sync manifest; (b) the audit-skill
   sentence. Both hook copies and both lib copies change together.

## Verification

- `python3 -m pytest tests/test_op_policy.py tests/test_secret_guard_parity.py tests/test_op_automations.py tests/test_claude_bash_pretooluse.py tests/test_security_hook_hardening.py tests/test_op_routing.py -q` green; `bin/ai-config-sync audit` clean; `bash -n` and `shellcheck` on both hooks show no new findings.
- Negative live checks through the Claude Bash tool: `op-automations read op://Automations/<item>/credential` denied; `X=$(op-automations read …); echo "$X"` denied; `OP_RUN_NO_MASKING=1 op-automations run …` denied; `op-desktop signin --raw` denied.
- Masking canary through both surfaces (Claude Bash tool; Codex `exec_command`): an env file in the scratchpad whose one variable references a **non-secret** field of an existing Automations item (label found with `op-automations item get <item> --format=json | jq '[.fields[] | {label,purpose,type}]'`, itself an allowed shape), run as `op-automations run --env-file <scratch>/canary.env.op -- python3 <scratch>/canary.py` where `canary.py` prints the variable to stdout and stderr. Expected tool output: `<concealed by 1Password>` on both streams. A masking failure would print a non-secret.
- Originating task: the email-triage redraft bridge runs through `op-automations run --env-file <abs>/.env.op -- python3 <abs>/claude_assignment_bridge.py --redraft …` without a denial and its output is one JSON status line.

## Adjudication record, Codex review of revision 1 (2026-09-02)

Reviewer: Codex, buffer `*codex:~/My Drive/dotfiles/:plan-review-codex*`,
run file in the session scratchpad. All eight findings accepted; none
rejected. Factual claims verified: `OP_RUN_NO_MASKING` (CLI 2.30.3 release
notes), `op signin --raw` "only return the session token", `op environment
read <id>` prints `KEY=value` lines (hidden ones masked), and the Codex
`block-sensitive-read.sh` and Claude standalone copy carry the same compound
rule as the dispatcher.

1. P0, pipe/redirect is not containment (`| cat`, `> /dev/stdout`, `>&2`,
   `--out-file /dev/stderr`, `jq .`): accepted; replaced by the regular-path
   rule, the consumer allowlist and the `jq` metadata-key rule.
2. P0, brokers outside the segment's first position fail open: accepted;
   recognition now splits on every control token and denies any residual
   broker word.
3. P0, unknown subcommands allowed: accepted; the design is now an allowlist,
   `signin` and `environment read` named as denied examples.
4. P0, same-call reuse of a captured read: accepted; same-call reuse rule.
5. P0, `OP_RUN_NO_MASKING`: accepted; any `OP_*` env prefix or that string
   denies.
6. P1, step 2 covered only the Claude dispatcher: accepted; all three
   sensitive-read implementations and their tests.
7. P1, verification missed the risky changes: accepted; adversarial case
   table, `test_claude_bash_pretooluse.py`, and a masking canary on both
   surfaces replace "the bridge printed one status line".
8. P2, apply the patch from the reviewed blob, not the working tree: accepted
   in spirit; revision 2 has no pre-built patch, the implementation commit is
   the artifact and its tests are the check.

## Open decisions for the owner

- `pass` and `security` remain blanket-denied by the 08-31 rule. The same
  stdout-based argument applies (`X=$(pass show env/foo)` is the access
  pattern for personal secrets). Left out so this plan stays about 1Password;
  recommend a follow-up on the same allowlist model.
- Raw `op` remains denied. The zsh shim routes bare `op` to the right broker
  and agent Bash calls reach the `.zshenv` `op` function, so the denial is
  belt-and-braces for the Touch ID concern; keeping it is a judgment call.
