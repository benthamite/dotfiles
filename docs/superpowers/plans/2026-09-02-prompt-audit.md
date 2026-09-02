# Plan: apply the 2026-09-02 prompt audit

**Goal.** Remove dated prompting patterns from the dotfiles agent instruction surface (CLAUDE.md, skills, context files, hook-emitted text) for Claude Fable 5.1, mirroring every edit into the paired Codex artifacts as `ai-config-sync.json` requires.

**Deliverable under review.** The audit report below (assumptions, findings with `file:line`, pattern, reason, confidence) and the proposed patch committed alongside it at `docs/superpowers/plans/2026-09-02-prompt-audit.patch` (read it with `git show <commit>:docs/superpowers/plans/2026-09-02-prompt-audit.patch`). Hunk ids in the findings tables (G1, S12, P5a, ...) map to the edits that produced the patch; each hunk is independently droppable.

**Implementation steps.**

1. `git apply docs/superpowers/plans/2026-09-02-prompt-audit.patch` from the repo root (already verified with `git apply --check` against the current tree).
2. Adjudicate review findings: drop or amend hunks the review invalidates; record each rejection reason in this file.
3. Run `bin/ai-config-sync audit` and fix any Claude/Codex pair the patch left unsynchronized.
4. Add a `claude/README.org` line for the two hook wording changes if `require-readme-update` blocks the commit.
5. Commit in single-purpose commits: global instructions (CLAUDE.md/AGENTS.md), hooks, global skills, project-local skills, context files, emacs/macos skills.

**Verification.** `git apply --check` before, `bin/ai-config-sync audit` after, `git diff --stat` matching the patch's 45 files, and a read-through of every rewritten sentence in place (each must still carry the rule and its reason). No behavioral eval exists for these skills; the highest-stakes rewrites (G5 scope of the root-cause rule; S12/S16 digest and Slack length) are called out for the user to decline or re-probe.

---

# Prompt audit: dotfiles agent instruction surface (2026-09-02)

## Assumptions

- **Scope:** whole repository prompt surface, Claude side as primary: `claude/CLAUDE.md`, `claude/context/*.md`, `claude/skills/*/SKILL.md` (42), `claude/programmatic-skills` (3), `.claude/skills` (9) + `.claude/programmatic-skills` (1), `emacs/.claude/skills` (4), `macos/.claude/skills` (4, incl. untracked `reconcile-contacts`), `claude/templates/reasoning-tasks/*`, `codex/rules/default.rules`, and the model-facing text emitted by `claude/hooks/*.sh` plus `bin/ai-config-sync`. `claude/context/voice-samples.md` is data, not instructions. Vendor-bundled `codex/skills/.system/*` (OpenAI docs) is out of scope.
- **Target model:** Claude Fable 5.1. `~/.claude/settings.json` sets `model: fable[1m]`, `effortLevel: high`; no repo file pins a different model except three skill frontmatter pins (findings below).
- **Non-Anthropic provider marker:** the `codex/` tree is a paired copy of the `claude/` tree for OpenAI Codex (GPT-5.6 per `codex/config.toml`). The Fable 5.1 reasoning does not transfer to Codex, but `ai-config-sync.json` requires every Claude edit to be mirrored in the same session, so the patch applies each hunk to the Codex counterpart wherever the text is identical. Four hunks have no Codex counterpart text (Claude-only frontmatter or tools) and are Claude-only.
- **Provenance:** all files date from 2026-01-30 onward (Opus 4.6 era through Fable 5). No retired model names, no prefill, no scratchpad/"think step by step" scaffolds, no anti-formatting or anti-narration rules, no `budget_tokens` or sampling fossils anywhere. The surface is already well-tuned; what remains is incident archaeology, numeric caps, a few shouted boosters, stale environment facts, and three model pins.

## Summary

| Group | High | Medium | Low/flag |
|---|---|---|---|
| 1a pressure language | 1 | 9 | 4 |
| 1c over-specification / duplication | 0 | 5 | 3 |
| 1d fossils / migration-relative phrasing | 4 | 4 | 1 |
| 1f numeric output ceilings | 0 | 5 | 2 |
| 2 skill files (history narratives, volatile specifics, model pins) | 5 | 9 | 5 |
| 3 tool descriptions | 0 | 0 | 0 |
| 4 request config / architecture | 0 | 0 | 0 |

Highest impact:

1. **Two security-critical skills and one text skill pin a weaker model.** `gitguardian-triage` and `publish-dotfiles` carry `model: opus`, `proofread` carries `model: sonnet`. On a Fable 5.1 account these silently run secret rotation and the publication gate on Opus-tier. No reason is recorded.
2. **Stale environment facts that Fable 5.1 will read as current.** `install-mcp-server` and `context/mcp-servers.md` tell the model to clone MCP servers under `~/My Drive/dotfiles/claude/mcp-servers/`, which no longer exists and which `CLAUDE.md` forbids. `context/google-services.md` pins gdoc v0.7.6 observations (installed: 0.21.0) and narrates a July token-migration that has finished.
3. **Incident archaeology and numeric caps in judgment skills.** `update-log`, `personalize`, `verify`, `orchestrate-review`, `emacs-freeze`, `publish-dotfiles`, `secrets.md` and `CLAUDE.md` itself carry "until DATE this sat before…", "happened once because…", "~120 words hard cap", "2-4 sentences ~100 words", "loop more than 3 times". Fable 5.1 follows numeric caps literally and already under-elaborates; the incident narratives are diffs against prompt versions it never saw.

Clean (no findings): automate, code-audit, design-audit, dotfiles-context, elisp-conventions, fix-drive-errors, humanize, interpretability-audit, lint-elisp, move-session-log, org-note-conventions, pin-elisp-pr, post-push-ci, pr-audit, profile-ai-cli-performance, record-decisions, refresh-cr-tracker, release-package, request-review, session-learning-capture, test-suite, triage-personal-todos, walk-list (both), try-hartree-skills, open-session-log (both), find-skills, skill-audit, skill-resolver, release-dotfiles, migrate-profile, rename-package, audit-mac-app, reconcile-contacts, context/mcp-servers.md (except one line), templates/reasoning-tasks, codex/rules, all deny-reason hook text, ai-config-sync reminders.

## Findings (patch hunk id in brackets; ordered by confidence)

### High

| Location | Evidence | Pattern | Why obsolete | Action |
|---|---|---|---|---|
| `.claude/skills/gitguardian-triage/SKILL.md:5` [P1] | `model: opus` | G2 pinned model / G4 fossil | Session default is Fable 5.1; the pin routes secret rotation to Opus-tier with no recorded reason. | remove |
| `.claude/skills/publish-dotfiles/SKILL.md:5` [P2] | `model: opus` | same | Publication gate runs on a weaker model than the session. | remove |
| `.claude/skills/install-mcp-server/SKILL.md:72-74` [P3] | `Clone to ~/My Drive/dotfiles/claude/mcp-servers/<name>/ … Run /nosync on any node_modules` | G2 volatile specifics contradicted by newer rule | Directory does not exist; `CLAUDE.md:58` forbids repos/deps under the Drive root. The `/nosync` step only patches the wrong location. | rewrite → clone to `~/repos/mcp-servers/<name>/`, drop `/nosync` |
| `claude/context/mcp-servers.md:19` [P3b] | same Drive path | same | Cited by the skill as the placement authority. | rewrite |
| `claude/context/google-services.md:73,91-92` [X1] | `(observed 2026-06-11)` / `observed with installed gdoc v0.7.6 … Re-check after gdoc upgrades` | G2 volatile specifics | Installed gdoc is 0.21.0 (verified); the caveat has no owner. | rewrite → version-independent verification instruction |
| `claude/context/google-services.md:275-288` [X2] | `On 2026-07-08 its publishing status was changed … Tokens issued before that date still carry the old 7-day fuse … Historical (testing-mode) failure; should no longer occur` | 1d migration-relative phrasing; G2 history | Any 7-day token from before July has long rolled over; the text narrates a finished transition. | rewrite to present-tense fact |
| `claude/context/secrets.md:20` [X3] | `An earlier version of this section claimed … That was wrong` | 1d migration-relative; G2 history | Corrects text the model never saw; the mechanism is fully stated on line 18. | remove |
| `claude/skills/update-log/SKILL.md:204-206` [S11] | `Until 2026-08-13 this step sat before the hooks…` | 1d migration-relative; G2 history | Diff against a prior version; the reason survives on lines 201-203. | remove |
| `claude/skills/verify/SKILL.md:12-14` [S13] | `IMPORTANT: When triggered, follow the execution steps below. Do NOT just describe what the skill does.` | 1a booster / 1d fossil | Workaround for older models narrating a skill instead of running it; Fable 5.1 executes invoked skills. | remove section |
| `claude/skills/personalize/SKILL.md:23-28` [S15] | `Historically this was two steps: draft, then run humanize…` | 1d migration-relative; G2 history | Describes a superseded workflow already restated at lines 8 and 44-47. | remove |

### Medium

| Location | Evidence | Pattern | Why obsolete | Action |
|---|---|---|---|---|
| `claude/CLAUDE.md:8` [G1] | `the MCP approval guard was deleted on 2026-07-31 … 42% of 7,552 historical MCP calls against 4 hard blocks` | G2 history narrative inside a rule | Rule and reason are load-bearing; the date and counts are archaeology. | rewrite: keep rule + "ask-style gates escalate far more calls than they would ever block" |
| `claude/CLAUDE.md:58` [G2] | `All other 108 repositories moved to ~/repos/ on 2026-08-03.` | G2 history narrative | Past-tense migration fact; the rule is the current location. | rewrite |
| `claude/CLAUDE.md:16` [G3] | `NEVER ask permission to remove your own temp artifacts; just do it.` | 1a caps + 1c repetition | Restates the preceding sentence and line 13. | remove sentence |
| `claude/CLAUDE.md:9-10` [G4] | line 9: `say exactly what was and was not verified`; line 10: `Say what was or wasn't verified only when it bears on my decision` | 1c near-duplicates that disagree | Two disclosure rules conflict; line 10 is the fuller one. | rewrite line 9 to the verification requirement only |
| `claude/CLAUDE.md:6` [G5] | `Treat every unintended behavior you encounter, in any context, as a prompt to diagnose and fix the underlying issue.` | keep-list 11 re-baselining / Fable 5.1 "state boundaries" | Fable 5.1 already takes unrequested-but-adjacent actions; "every … in any context" widens scope beyond the task. The root-cause intent (second sentence) stays. Decline if the literal mandate is intended. | rewrite |
| `claude/hooks/cr-skill-routing-reminder.sh:24` [H1] | `⚠️ CR ROUTING GUARD — … OWNED … INVOKED … STOP now … routing-principle violation` | 1a pressure language in model-facing hook text | Comment says it was written because "a rule in an always-on doc demonstrably fails"; the event-driven hook is the fix, the shouting is the fossil. | rewrite plain |
| `claude/hooks/remind-claude-readme-update.sh:70-71` [H2] | `REMINDER: … You MUST:` | 1a | The commit hook enforces it; reminder can be plain. | rewrite |
| `claude/skills/add-to-emacs-packages/SKILL.md:13` [S1] | same IMPORTANT/Do NOT booster as verify | 1a / 1d | as above | remove |
| `.claude/skills/install-mcp-server/SKILL.md:11` [P4] | same booster | 1a / 1d | as above | remove |
| `.claude/skills/install-mcp-server/SKILL.md:44,57,79,133,140,142,151` [P5a-g] | six CRITICAL/MUST markers; trailing-slash rule stated three times; auth-first stated three times | 1a density; 1c repetition | Every rule carries its reason in the body; stacked markers stop carrying information. | rewrite headings plain, drop repeats, keep reasons |
| `claude/skills/build/SKILL.md:42` [S2] | `Aim for thoroughness: … 20-40+ questions` | 1a thoroughness booster + 1f numeric anchor | Line 43 states the real stop condition. | remove |
| `claude/skills/handoff/SKILL.md:39-42` [S3] | `IMPORTANT: … Never … Always … BEFORE` | 1a + 1c repetition of lines 34-36 | Tool contract stays; the caps stack duplicates it. | rewrite to one plain sentence |
| `claude/skills/handoff/SKILL.md:70` [S4] | `Do NOT run this command yourself — only the user should trigger it.` | 1a emphasis without reason | Attach the reason (it closes the session). | rewrite |
| `claude/skills/google-sheets-comments/SKILL.md:81-84` [S5] | two "Do not…" pitfalls | 1c repetition of lines 8-11, 20-21 | Same rule in three places. | remove |
| `claude/skills/google-sheets-comments/SKILL.md:91-93` [S6] | `If you already sent an email reply by mistake…` | G2 recency trap | Step 9 already requires `comment-info` verification. | remove |
| `claude/skills/google-sheets-comments/SKILL.md:31-35` [S7] | `sed -n '1,220p' … google-services.md` | G2 volatile specific | File is 305 lines; the range truncates it. | rewrite to "read the file" |
| `claude/skills/generate-readme/SKILL.md:146` [S8] | `Do not leave any trace … no empty section, no "see README" pointer, no commented-out content. Do not add any note…` | 1c prohibition list / 1d patch accretion | Enumerated leftovers anchor toward them. | rewrite positively |
| `claude/skills/dx-audit/SKILL.md:10,92` [S9] | `Thoroughly explore` / `Think broadly.` | 1a thoroughness booster | Fable 5.1 over-gathers context at high effort. | rewrite |
| `claude/skills/chrome-permission-audit/SKILL.md:112-113` [S10] | `not with a 185-row table` | G2 one run's number frozen into the rule | | rewrite |
| `claude/skills/update-log/SKILL.md:221` [S12] | `up to ~6 bullets … Hard cap ~120 words` | 1f numeric ceiling | Surrounding rules already carry the goal; Fable 5.1 reads the cap literally. | rewrite |
| `claude/skills/verify/SKILL.md:84` [S14] | `If you loop more than 3 times on the same criterion` | 1f / 1b fixed cadence | Fixed 3 makes it abandon a converging fix. | rewrite |
| `claude/skills/personalize/SKILL.md:56-57` [S16] | `2–4 sentences, ~100 words max` | 1f numeric ceiling in a hard-rule block | Truncates a reply that legitimately needs five sentences. | rewrite |
| `claude/skills/orchestrate-review/SKILL.md:76-81` [S17] | `A 10-hour run of five full-cycle retries … happened once because…` | G2 history / recency trap | Rule and reason are already stated. | rewrite |
| `claude/skills/orchestrate-review/SKILL.md:39,49,74,465` [S18] | three `— HARD RULE` headers; `Violating the letter of these rules violates the workflow:` | 1a density | Markers stop carrying information; the bullets beneath are unconditional and reasoned. | rewrite headers plain, drop line 49 |
| `claude/skills/symptom-check/SKILL.md:105` [S19] | `These have all happened. Don't:` | G2 recency framing | Provenance line only; the bullets stay (each carries its reason). | rewrite to `Do not:` |
| `claude/skills/proofread/SKILL.md:4` [S20] | `model: sonnet` | G2 model pin | No recorded reason; decline if it is a deliberate cost choice. | remove |
| `.claude/skills/gitguardian-triage/SKILL.md:220` [P7] | `The earlier head -1 shortcut misses…` | 1d migration-relative; duplicates line 208 | | remove |
| `.claude/skills/config-audit/SKILL.md:195` [P9] | `release, twitter-digest, etc.` | G2 volatile specific | `twitter-digest` is archived. | rewrite |
| `.claude/skills/config-audit/SKILL.md:83` [P10] | `prefer Opus over Haiku for subagents` | G2 pinned model names | Global rule now says "most capable available model". | rewrite |
| `.claude/skills/optimize-agent-instructions/SKILL.md:119-123` [P11] | `No dense paragraphs of prose — bullets are more reliably followed` | 1c bullet walls prescribed as a rubric | Current guidance: prose for behavior (carrying the reason), structure for reference data. Applied as an audit criterion this pushes every audited file the wrong way. | rewrite |
| `.claude/skills/optimize-agent-instructions/SKILL.md:18,29-30,133-134` [P12] | `reliably follow ~150-200 instructions total … system prompt consumes ~50` / `target: under 200; absolute max: 300` | G2 unverified numeric claim; 1f ceilings | Unsourced counts drive hard targets; the documented harm is specific dated instructions, not length. | rewrite; keep counts as measurements without targets |
| `claude/context/secrets.md:22` [X4] | `(2026-08-01)` in the bullet title; `That gap is what broke on 2026-08-01: ahrefs-api-guard spawned … Fixed via op_reader_for()` | G2 history narrative inside a rule | The mechanism and the test reference are the keepers. | rewrite |
| `emacs/.claude/skills/emacs-freeze/SKILL.md:234` [M1] | `These exist because this skill previously produced a confident, plausible, and wrong diagnosis. Honor them strictly.` | 1a pressure without reason; G2 recency | Each rule below carries its own reason. | rewrite |
| `emacs/.claude/skills/emacs-freeze/SKILL.md:40-43,104-105,156,243` [M2] | `In the session that produced this rule…` (×2), `This ladder exists because skipping it once…`, `Every claim … was wrong at least once before it was tested.` | G2 history narratives | Mechanism already stated; the 1,350-vs-5 example is kept as an illustrative example. | rewrite |

### Low (flag only, not in the patch)

- `claude/CLAUDE.md:32` `No preamble, no restating my question back to me, no summary of what you just said … overrides any urge to show your work.` Reads as an update-suppressor written for chattier models (1d); Fable 5.1 under-narrates and the harness already asks for a one-line opener and a recap. It is also your stated quality bar, so it stays. Re-probe if final messages start missing the recap.
- `claude/skills/document-elisp-extras/SKILL.md:19-21` `@claude/skills/document-elisp-package/SKILL.md` include duplicates five by-name references and pulls 302 lines per trigger; working redundancy (keep-list 8), and the `@` form does not expand under Codex.
- `claude/skills/build/SKILL.md:13,25,59,73` plain-volume "follow the steps instead of describing" fossil; verbatim scripted user-facing lines (single gold outputs).
- `claude/skills/diagnose/SKILL.md:40`, `document-elisp-package/SKILL.md:10,200,209` scattered single caps words, each next to its reason.
- `claude/skills/end-to-end/SKILL.md:58-72,119-122` dense narrow conditionals (1d patch accretion) that each encode authorization policy; keep unless blame ties a line to a retired-model incident.
- `claude/skills/generate-readme/SKILL.md:55,122` `2–4 paragraphs`, `5–10 lines of Elisp` spec the deliverable's shape, not verbosity.
- `claude/skills/add-bib-entry/SKILL.md:44` "historical headless download path": drop "historical".
- `claude/skills/verify/SKILL.md:47` `Length is within N% of target` is an illustrative table cell.
- `claude/skills/overnight-todos/SKILL.md:143-150` three `NEVER` lines each carry a reason and encode policy; style only.
- `claude/hooks/block-walk-list-access.sh:29` `Do NOT attempt to circumvent` in a deny reason for a protected store; fragile-operation text.
- `.claude/skills/gitguardian-triage/SKILL.md:330,453-455` SVG-path DOM selector (verify on next use); ">5 incidents → create plan items" choreography.
- `.claude/skills/optimize-agent-instructions/SKILL.md:82,92` undated claims about harness internals and attention bias.
- `claude/context/secrets.md:18,23` measurement dates (2026-07-28); the numbers are the evidence, the dates add nothing.
- `macos/.claude/skills/review-lulu-alert/SKILL.md:29-47` "Rationalizations that mean stop" table anchors toward the excuses it lists, but guards a firewall-rule write (keep-list 3).
- `macos/.claude/skills/security-audit/SKILL.md:126,173` `(per Kim's Glasswing guidance…)` attribution; "offer to fix" output-shaping sentence.

## Proposed diff

`docs/superpowers/plans/2026-09-02-prompt-audit.patch` (45 files, +122/-218), generated from a scratch copy and verified with `git apply --check` against the current working tree. Not applied. One hunk per finding id above. Codex counterparts are included for every hunk whose text matches (S3, S20, P1, P2 are Claude-only: Codex has no `AskUserQuestion` block and no `model:` frontmatter). Apply all with:

```
git apply docs/superpowers/plans/2026-09-02-prompt-audit.patch
```

or take hunks selectively with `git apply --include=<path>` / `git add -p`. After applying, `bin/ai-config-sync audit` should pass because both sides move together; `claude/README.org` may need a line for the hook wording changes (the `require-readme-update` hook will say so).

## Verification (Step 7)

- Verified directly: gdoc version (0.21.0 vs pinned 0.7.6); `claude/mcp-servers/` absent; `twitter-digest` only under `archive/`; `tests/test_op_routing.py` and `shell/shims/op` exist; the three `model:` pins; every Codex counterpart body byte-identical before edits; patch applies cleanly.
- Not verified: behavioral A/B of any rewrite. No eval suite covers these skills. The highest-stakes rewrites are G5 (scope of the fix-root-cause rule) and S12/S16 (digest and Slack length): if the digest or Slack drafts come back longer than you want after applying, re-add a qualitative bound ("a few bullets", "a short reply"), not the number.
- Re-run this audit at the next model release.
