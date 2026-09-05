# Proofread

Paired Claude Code/Codex proofreading for Markdown prose, with local aspell or
Google Gemini. See [SKILL.md](./SKILL.md) for scope, privacy, review and acceptance.

## Setup

From this skill directory, run `yarn -s setup-runtime` when dependencies are missing.
Setup and execution use PROOFREAD_RUNTIME_DIR, then XDG_DATA_HOME, then ~/.local/share/proofread.
Both the runtime root and node_modules destination are canonicalized and must
remain outside Google Drive. Do not create or symlink local `node_modules`,
builds, caches or test fixtures into the synced tree.
The installer uses Yarn Classic 1.x and `trash`, stages public manifests outside
Drive, and skips implicit Yarn/npm rc files. Required policy or network settings
must be supplied through an approved setup; do not bypass them to install.
These path controls do not sandbox dependency lifecycle scripts.
Once installed, use the shell runtime wrapper shown in the skill; normal
proofreading and suggestion application do not require Yarn on PATH.

LLM mode alone consumes `GOOGLE_AI_API_KEY` injected through the approved secret
workflow. Skill-local `.env` files and the legacy `.env.example` are no longer
runtime configuration; do not copy credentials into either. Existing private
files are not migrated or deleted by setup. `PROOFREAD_MODEL` is an explicit
non-secret override; the current default and its source are in the skill.

## Outputs and verification

Review-only requests stay in-session. The scripts create new `.proofread.md`
and `.final.md` siblings (retaining `.mdx` for MDX) for authorised editing, preserving existing files and
rejecting stale or ambiguous edits. Failures are not empty-success reports.
Spellcheck remains suggestion-only. Compare the written diff with the accepted
changes; excluded Markdown/MDX syntax remains an explicit coverage limit.

The live global skill directories are managed by the dotfiles sync setup.
Do not install this tracked copy with `npx skills add`.

Adapted from [HartreeWorks/skills](https://github.com/HartreeWorks/skills).
