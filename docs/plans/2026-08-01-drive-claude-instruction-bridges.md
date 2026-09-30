# Drive-Compatible Claude Instruction Bridges Implementation Plan

> **Superseded:** Do not execute this plan. Its Drive baseline and sequencing
> rely on the invalid assumption that directory-symlink failures are not
> user-visible. Rewrite it from the revised workspace design before use.

**Goal:** Replace two Drive-rejected `CLAUDE.md` file symlinks with exact import bridges while keeping `AGENTS.md` canonical and making every local parity workflow bridge-aware.

**Architecture:** Dotfiles recognizes a regular `CLAUDE.md` containing exactly `@AGENTS.md` as a semantic bridge, not a byte mirror. The reminder, commit guard, audit, mirror helper, and `update-log` workflow all preserve `AGENTS.md` as the only writable source before either repository changes.

**Tech Stack:** Python 3 standard library, Bash, `unittest`, Org documentation, Claude Code project-memory imports, Git, Google Drive for desktop.

---

This is plan 2 of 4. Start only after the Epoch document-link plan has reduced the native Drive count to 20. Execute in the real Drive-side working trees, not an isolated worktree, because the acceptance signal is tied to these exact filesystem paths.

## File map

Dotfiles:

- Modify: `bin/ai-config-sync` — semantic bridge detection for audit, reminders, and commit guard.
- Modify: `bin/mirror-claude-agents` — no-op safely for every bridge invocation mode.
- Modify: `tests/test_ai_config_sync_audit.py` — bridge regression coverage.
- Modify: `agents/README.org` — peer-mirror versus import-bridge policy.
- Modify: `claude/skills/update-log/SKILL.md` — select `AGENTS.md` as the session index for a bridge.
- Modify: `codex/skills/update-log/SKILL.md` — paired copy of the same behavior.
- Modify: `claude/README.org` — update-log summary.
- Modify: `codex/README.org` — update-log summary.

Rubric Visualizer:

- Replace: `CLAUDE.md` — mode `120000` symlink becomes a mode `100644` regular file.

Uqbar:

- Replace: `CLAUDE.md` — mode `120000` symlink becomes a mode `100644` regular file after a safe fast-forward.

Do not touch unrelated dotfiles changes or plans already present in the working tree.

### Task 1: Add failing semantic-bridge tests

**Files:**

- Modify: `tests/test_ai_config_sync_audit.py`

- [ ] **Step 1: Add the bridge fixture and regression tests**

Add these methods inside `AiConfigSyncAuditTests`:

~~~python
    def make_instruction_bridge_repo(self) -> Path:
        repo = self.make_repo(["CLAUDE.md", "AGENTS.md"])
        self.write_file(repo, "CLAUDE.md", "@AGENTS.md\n")
        self.write_file(repo, "AGENTS.md", "canonical instructions\n")
        return repo

    def test_import_only_claude_bridge_is_synchronized(self):
        repo = self.make_instruction_bridge_repo()
        self.assertEqual([], self.module.local_instruction_sync_problems(repo))

    def test_import_bridge_requires_agents_target(self):
        repo = self.make_repo(["CLAUDE.md"])
        self.write_file(repo, "CLAUDE.md", "@AGENTS.md\n")
        self.assertEqual(
            ["Missing project-local instruction file: AGENTS.md"],
            self.module.local_instruction_sync_problems(repo),
        )

    def test_non_exact_import_bridge_is_reported_as_drift(self):
        repo = self.make_instruction_bridge_repo()
        self.write_file(repo, "CLAUDE.md", "@AGENTS.md\n\nClaude-only text\n")
        self.assertEqual(
            [
                "Project-local instruction drift after tool-specific "
                "normalization: CLAUDE.md / AGENTS.md"
            ],
            self.module.local_instruction_sync_problems(repo),
        )

    def test_import_bridge_suppresses_reciprocal_reminders(self):
        repo = self.make_instruction_bridge_repo()
        self.assertEqual(
            [],
            self.module.reminder_messages(
                [str(repo / "CLAUDE.md"), str(repo / "AGENTS.md")],
                repo,
            ),
        )

    def test_guard_allows_canonical_agents_only_change_for_bridge(self):
        repo = self.make_instruction_bridge_repo()
        self.write_file(repo, "AGENTS.md", "changed canonical instructions\n")
        self.run_git(repo, "add", "AGENTS.md")
        proc = subprocess.run(
            [
                sys.executable,
                str(DOTFILES / "bin" / "ai-config-sync"),
                "guard-commit",
            ],
            cwd=repo,
            input=json.dumps(
                {
                    "cwd": str(repo),
                    "tool_input": {"command": "git commit -m test"},
                }
            ),
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        self.assertEqual("", proc.stdout)

    def test_mirror_helper_never_copies_over_import_bridge(self):
        repo = self.make_instruction_bridge_repo()
        helper = DOTFILES / "bin" / "mirror-claude-agents"
        for flags in ((), ("--check",), ("--reverse",)):
            with self.subTest(flags=flags):
                proc = subprocess.run(
                    [str(helper), *flags, str(repo)],
                    check=True,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                )
                self.assertIn("AGENTS.md is canonical", proc.stdout)
                self.assertEqual("@AGENTS.md\n", (repo / "CLAUDE.md").read_text())
                self.assertEqual(
                    "canonical instructions\n",
                    (repo / "AGENTS.md").read_text(),
                )

    def test_update_log_skills_define_the_import_bridge_branch(self):
        required = (
            "@AGENTS.md\\n",
            "AGENTS.md is the canonical session-log index",
            "leave CLAUDE.md byte-unchanged",
        )
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                text = (
                    DOTFILES
                    / tool
                    / "skills"
                    / "update-log"
                    / "SKILL.md"
                ).read_text()
                for phrase in required:
                    self.assertIn(phrase, text)
~~~

- [ ] **Step 2: Run the focused tests and verify the new cases fail**

Run:

~~~bash
python3 -m unittest tests/test_ai_config_sync_audit.py -v
~~~

Expected: the existing tests pass, while the bridge-equivalence, reminder, guard, and mirror-helper cases fail because exact bridges are not recognized yet.

### Task 2: Implement semantic bridge recognition

**Files:**

- Modify: `bin/ai-config-sync`
- Modify: `bin/mirror-claude-agents`
- Test: `tests/test_ai_config_sync_audit.py`

- [ ] **Step 1: Add the exact bridge predicate**

Add this beside the project-instruction normalization helpers in `bin/ai-config-sync`:

~~~python
PROJECT_AGENTS_IMPORT_BRIDGE = "@AGENTS.md\n"


def is_project_agents_import_bridge(path: Path) -> bool:
    return (
        path.name == "CLAUDE.md"
        and not path.is_symlink()
        and path.is_file()
        and path.read_text() == PROJECT_AGENTS_IMPORT_BRIDGE
    )
~~~

- [ ] **Step 2: Suppress reminders only for the exact bridge**

Replace the opening instruction-file branches of `local_sync_message_for_path` with:

~~~python
def local_sync_message_for_path(rel: str, repo_root: Path) -> str:
    if not rel:
        return ""

    rel_path = Path(rel)
    if rel_path.name in {"CLAUDE.md", "AGENTS.md"}:
        claude_path = repo_root / rel_path.parent / "CLAUDE.md"
        if is_project_agents_import_bridge(claude_path):
            return ""

    if rel_path.name == "CLAUDE.md":
        counterpart = rel_path.with_name("AGENTS.md").as_posix()
        return f"Project-local {rel} changed; update {counterpart} in the same repo."
    if rel_path.name == "AGENTS.md":
        counterpart = rel_path.with_name("CLAUDE.md").as_posix()
        return f"Project-local {rel} changed; update {counterpart} in the same repo."
~~~

Keep every later hook, skill, settings, and rule branch in the function unchanged.

- [ ] **Step 3: Treat the bridge as synchronized only after both files exist**

In `local_instruction_sync_problems`, add this after the existing missing-file early return and before normalized comparison:

~~~python
    if is_project_agents_import_bridge(claude_path):
        return problems
~~~

Do not change `guard_changed_paths`; it already delegates instruction equivalence to `local_instruction_sync_problems`.

- [ ] **Step 4: Make the shell helper fail safe for all modes**

Add this function after `CLAUDE_FILE` and `AGENTS_FILE` are defined:

~~~bash
is_agents_import_bridge() {
  [ ! -L "$CLAUDE_FILE" ] &&
    [ -f "$CLAUDE_FILE" ] &&
    cmp -s "$CLAUDE_FILE" <(printf '%s\n' '@AGENTS.md')
}
~~~

After the existing check that both files exist, but before the ordinary `cmp` branch, add:

~~~bash
if is_agents_import_bridge; then
  echo "mirror-claude-agents: $PROJECT_DIR uses an @AGENTS.md import bridge; AGENTS.md is canonical; no copy needed"
  exit 0
fi
~~~

Update the script header to state that an exact regular `@AGENTS.md` bridge makes `AGENTS.md` canonical and causes default, `--check`, and `--reverse` modes to no-op.

- [ ] **Step 5: Run focused verification**

Run:

~~~bash
bash -n bin/mirror-claude-agents
python3 -m unittest tests/test_ai_config_sync_audit.py -v
~~~

Expected: all focused tests pass.

### Task 3: Make update-log bridge-aware

**Files:**

- Modify: `claude/skills/update-log/SKILL.md`
- Modify: `codex/skills/update-log/SKILL.md`
- Modify: `claude/README.org`
- Modify: `codex/README.org`

- [ ] **Step 1: Invoke the required skill-writing guidance**

Read and follow `superpowers:writing-skills` before editing the paired skill files. Preserve their existing tool-specific frontmatter and keep their bodies equivalent after frontmatter normalization.

- [ ] **Step 2: Add canonical-session-index selection to Step 0**

Replace the instruction that unconditionally reads `CLAUDE.md` as canonical with this policy in both copies:

~~~markdown
1. **Select the canonical session-index file.** If CLAUDE.md is a regular file
   whose complete byte content is exactly `@AGENTS.md\n`, treat it as an import
   bridge. Require a sibling AGENTS.md; if the target is missing, stop with a
   clear error. In bridge mode, AGENTS.md is the canonical session-log index:
   read and update AGENTS.md, and leave CLAUDE.md byte-unchanged. Otherwise,
   retain the ordinary behavior in this skill: CLAUDE.md is canonical unless
   the project explicitly says otherwise.

2. **Read the selected canonical session-index file.** Look for a reference to
   a session log file, either a path such as `<dir>/YYYY-MM-DD.md` in a
   "Latest session" section or a legacy `@<dir>/YYYY-MM-DD.md` import. Extract
   its directory portion as the log directory.
~~~

Renumber the following detection items without changing their semantics.

- [ ] **Step 3: Replace the sibling mirror procedure**

Replace the `Sibling AGENTS.md` subsection in both copies with:

~~~markdown
### Sibling instruction files

When CLAUDE.md is the exact regular import bridge `@AGENTS.md\n`, update only
the canonical AGENTS.md selected in Step 0. Do not hand-edit or replace the
bridge. Run the first available mirror helper with `--check`; it must report
that AGENTS.md is canonical and no copy is needed.

For every other project that keeps byte-mirrored CLAUDE.md and AGENTS.md:

1. Before editing, run `diff -u CLAUDE.md AGENTS.md`. If they were already in
   sync, treat them as mirrors. If already drifted, preserve the intended
   divergence and edit both deliberately.
2. After updating CLAUDE.md, run the first available
   `mirror-claude-agents <project-dir>` helper, using the project-local helper
   before the dotfiles helper. Run `--check` first, then mirror only if needed.
3. Require a final `diff -u CLAUDE.md AGENTS.md` with no output.
~~~

Update first-run, staging, and reporting language throughout each skill to refer to the selected canonical session-index path. Bridge mode stages `AGENTS.md` when tracked and never stages `CLAUDE.md` unless the bridge itself was intentionally changed.

- [ ] **Step 4: Update the two README summaries**

Add this sentence to each existing update-log summary:

~~~org
An exact regular =CLAUDE.md= import bridge containing =@AGENTS.md= selects
=AGENTS.md= as the canonical session index; =update-log= edits only that target
and preserves the bridge byte-for-byte.
~~~

- [ ] **Step 5: Verify paired skill equivalence**

Run:

~~~bash
bin/ai-config-sync audit
~~~

Expected: the audit passes, including paired `update-log` skill normalization.

### Task 4: Document and commit the dotfiles support

**Files:**

- Modify: `agents/README.org`
- Modify: the eight dotfiles paths listed above

- [ ] **Step 1: Add the bridge policy after project-level instruction pairs**

Insert:

~~~org
An exact regular project =CLAUDE.md= whose complete content is
=@AGENTS.md= is an import bridge rather than an ordinary peer mirror.  In this
layout =AGENTS.md= is the only content source.  The reminder, commit guard,
audit, =mirror-claude-agents= helper, and =update-log= workflow recognize that
exact form and do not copy the bridge over its target.  Any additional content,
a missing target, or a symlink is not accepted as the bridge form and follows
the ordinary drift rules.
~~~

- [ ] **Step 2: Run the full dotfiles verification**

Run:

~~~bash
bash -n bin/mirror-claude-agents
python3 -m unittest tests/test_ai_config_sync_audit.py -v
python3 -m unittest discover -s tests -p 'test_*.py' -v
bin/ai-config-sync audit
git diff --check
~~~

Expected: all tests and the audit pass. Diagnose and report any unrelated pre-existing test failure rather than hiding it.

- [ ] **Step 3: Stage only the bridge-support paths**

Run:

~~~bash
git add agents/README.org
git add bin/ai-config-sync
git add bin/mirror-claude-agents
git add tests/test_ai_config_sync_audit.py
git add claude/skills/update-log/SKILL.md
git add codex/skills/update-log/SKILL.md
git add claude/README.org
git add codex/README.org
git diff --cached --name-only
git diff --cached --check
~~~

Expected staged paths: exactly the eight paths above. Preserve unrelated modifications and untracked files.

- [ ] **Step 4: Commit**

Run:

~~~bash
git commit -m "agents: support AGENTS import bridges"
~~~

Expected: one local dotfiles commit; no push.

### Task 5: Convert and verify the Rubric Visualizer bridge

**Files:**

- Replace: `/Users/pablostafforini/My Drive/repos/rubric-visualizer/CLAUDE.md`

- [ ] **Step 1: Record the clean baseline and canonical hash**

Run in the Rubric Visualizer repository:

~~~bash
git status --porcelain=v2 --branch
git rev-list --left-right --count HEAD...origin/main
git ls-files -s AGENTS.md CLAUDE.md
test -L CLAUDE.md
test "$(readlink CLAUDE.md)" = "AGENTS.md"
shasum -a 256 AGENTS.md
~~~

Expected: no dirty paths, `CLAUDE.md` mode `120000` pointing to `AGENTS.md`, and the existing local-ahead state preserved.

- [ ] **Step 2: Replace only the symlink**

Move `CLAUDE.md` to Trash, then create a regular `CLAUDE.md` through `apply_patch` with exactly:

~~~markdown
@AGENTS.md
~~~

Do not patch through the live symlink; that would edit `AGENTS.md`.

- [ ] **Step 3: Run direct representation and parity checks**

Run:

~~~bash
test ! -L CLAUDE.md
python3 -c 'from pathlib import Path; assert Path("CLAUDE.md").read_bytes() == b"@AGENTS.md\n"'
shasum -a 256 AGENTS.md
/Users/pablostafforini/My\ Drive/dotfiles/bin/mirror-claude-agents --check "$PWD"
git diff --summary -- CLAUDE.md
git diff --check
~~~

Expected: `AGENTS.md` has its baseline hash; the helper reports `AGENTS.md` canonical; the Git diff shows mode `120000` to `100644`.

- [ ] **Step 4: Verify Claude loads the imported instructions**

Run:

~~~bash
claude -p \
  --no-session-persistence \
  --max-budget-usd 0.20 \
  --disallowedTools 'Bash,Read,Grep,Glob,Edit,Write,WebFetch,WebSearch' \
  'From the project instructions already loaded at session start, which two flags print numbered essay and prompt segments? Answer with only the two flags, separated by one space.'
~~~

Expected exact output:

~~~text
--list --list-prompt
~~~

The disabled file tools make this a direct project-memory check rather than a repository search.

- [ ] **Step 5: Commit only the bridge**

Run:

~~~bash
git add CLAUDE.md
git diff --cached --summary
git diff --cached --check
git commit -m "docs: replace Claude symlink with import bridge"
~~~

Expected: one local commit; no push.

- [ ] **Step 6: Restart Drive and require the exact gate**

Record the restart timestamp, gracefully quit and relaunch Google Drive, wait for settling, and inspect the native menu-bar error panel. Require `20 → 19` and no new `UNSUPPORTED` entry for `rubric-visualizer/CLAUDE.md`. Stop the plan if the count or error class differs.

### Task 6: Fast-forward Uqbar while preserving unrelated files

**Files:**

- Inspect only before fast-forward: `/Users/pablostafforini/My Drive/repos/uqbar`

- [ ] **Step 1: Re-measure local and upstream state**

Run in Uqbar:

~~~bash
git status --porcelain=v2 --branch
git fetch origin
git merge-base --is-ancestor HEAD origin/main
git rev-list --left-right --count HEAD...origin/main
git diff --name-only HEAD..origin/main
git ls-files --others --exclude-standard
~~~

Require:

- no tracked or staged dirty paths;
- record unrelated untracked paths privately before changing the checkout;
- `HEAD` is an ancestor of `origin/main`;
- no recorded untracked path appears in the upstream diff.

Stop if any condition changes. Do not stash, reset, rebase, or delete unrelated untracked files.

- [ ] **Step 2: Fast-forward and revalidate semantics**

Run:

~~~bash
git merge --ff-only origin/main
git status --porcelain=v2 --branch
sed -n '1,120p' AGENTS.md
sed -n '1,140p' build
sed -n '1,140p' launch
git ls-files -s AGENTS.md CLAUDE.md build launch build.py launch.py
head -1 build
head -1 launch
readlink CLAUDE.md
readlink build.py
readlink launch.py
rg -n 'build\.py|launch\.py|\./build|\./launch' --glob '!uqbar-api/**' --glob '!uqbar-front/**' .
~~~

Require Bash shebangs in `build` and `launch` and symlink targets `AGENTS.md`, `build`, and `launch`. The known `.vscode/launch.json` debugpy mismatch remains out of scope.

### Task 7: Convert and verify the Uqbar bridge

**Files:**

- Replace: `/Users/pablostafforini/My Drive/repos/uqbar/CLAUDE.md`

- [ ] **Step 1: Record the canonical hash and replace the symlink safely**

Run `shasum -a 256 AGENTS.md`. Move only `CLAUDE.md` to Trash, then create a regular `CLAUDE.md` through `apply_patch` with exactly:

~~~markdown
@AGENTS.md
~~~

- [ ] **Step 2: Verify representation, target integrity, and project memory**

Run:

~~~bash
test ! -L CLAUDE.md
python3 -c 'from pathlib import Path; assert Path("CLAUDE.md").read_bytes() == b"@AGENTS.md\n"'
shasum -a 256 AGENTS.md
/Users/pablostafforini/My\ Drive/dotfiles/bin/mirror-claude-agents --check "$PWD"
git diff --summary -- CLAUDE.md
git diff --check
claude -p \
  --no-session-persistence \
  --max-budget-usd 0.20 \
  --disallowedTools 'Bash,Read,Grep,Glob,Edit,Write,WebFetch,WebSearch' \
  'From the project instructions already loaded at session start, what flag makes the launcher use the local sibling docker-launcher checkout? Answer with only the flag.'
~~~

Expected Claude output:

~~~text
--dev-docker-launcher
~~~

- [ ] **Step 3: Commit only `CLAUDE.md`**

Run:

~~~bash
git add CLAUDE.md
git diff --cached --summary
git diff --cached --check
git commit -m "docs: replace Claude symlink with import bridge"
~~~

Confirm the recorded unrelated untracked files remain present and unstaged. Do not push.

- [ ] **Step 4: Restart Drive and require the exact gate**

After settling, require the native Drive transition `19 → 18` and no new `UNSUPPORTED` entry for `uqbar/CLAUDE.md`. The next plan starts only at 18.
