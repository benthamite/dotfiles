---
name: proofread
description: Proofread Markdown for spelling, grammar, typos, punctuation, and light style/clarity issues. Use for proofread, spell check, grammar check, typo review, or proofreading a Markdown file.
model: sonnet
---

# Proofread skill

> Adapted from [Peter Hartree](https://pjh.is/)'s [HartreeWorks/skills](https://github.com/HartreeWorks/skills) repository.

Proofreading for Markdown documents using British English conventions by default. The scripts are for `.md`, `.markdown`, and `.mdx` files only.

Two engines are available:

- **Spellcheck** (fast, deterministic): Uses aspell for spell-checking (~2 seconds)
- **LLM** (thorough, AI-powered): Uses Gemini Flash for spelling, grammar, style, and clarity (~30-60 seconds per 100 lines)

## Workflow

### Step 1: Confirm target and mode

If the user did not provide a file path, ask for one. If the file is not Markdown (`.md`, `.markdown`, or `.mdx`), say this skill is scoped to Markdown files and do not run the bundled scripts.

Infer the engine when the user already gives a clear preference:

- Use **Spellcheck** for "quick spell check", "check spelling", or other spelling-only requests.
- Use **LLM level 2** for generic "proofread", "check grammar", "review for typos", or "light style" requests.
- Use **LLM level 1** for explicitly mechanical-only requests.
- Use **LLM level 3** for comprehensive or thorough editing requests.

If the mode is unclear, ask:

> Which proofreading approach do you want?
>
> **Spellcheck** — Fast deterministic spell-check using aspell (~2 seconds). Good for quick checks. No API key required.
>
> **LLM** — AI-powered proofreading using Gemini Flash (~30-60s per 100 lines). Choose a level:
> - **Level 1 — Mechanical only**: Spelling, punctuation, grammar (fast, minimal output)
> - **Level 2 — Light style pass**: Level 1 + top 5-10 style/clarity suggestions (recommended)
> - **Level 3 — Comprehensive**: All style/clarity suggestions (thorough, more output)

### Step 1a: Check API key (LLM modes only)

**Skip this step if the user chose Spellcheck.**

Resolve the current tool's skill directory before checking configuration:

- Codex: `/Users/pablostafforini/My Drive/dotfiles/bin/agent-skill path proofread --tool codex`
- Claude Code: `/Users/pablostafforini/My Drive/dotfiles/bin/agent-skill path proofread --tool claude`

Set `skill_dir` to the parent directory of the returned `SKILL.md` path. Use that directory for `.env`, scripts, and dependency installs.

If the user chose an LLM level, check if the API key is configured:

1. Read `$skill_dir/.env`
2. Check if it contains `GOOGLE_AI_API_KEY=` with a value

**If the key is missing or the file doesn't exist**, tell the user:

> The LLM proofreading modes require a Google AI API key to use Gemini.
>
> To set it up:
> 1. Go to https://aistudio.google.com/app/apikey
> 2. Create an API key
> 3. Add it to this skill's `.env` file:
>    ```
>    GOOGLE_AI_API_KEY=your_key_here
>    ```
>
> Would you like me to create the .env file for you once you have the key?
>
> Alternatively, you can use the **Spellcheck** mode which doesn't require an API key.

**Important:** Do not search for the API key in the user's shell environment or other locations. Only use the key from the skill's `.env` file.

### Step 2: Run the proofreading script

For spellcheck engine:
```bash
cd "$skill_dir" && yarn -s proofread "<file_path>" --engine spellcheck
```

For LLM engine:
```bash
cd "$skill_dir" && yarn -s proofread "<file_path>" --engine llm --level <1|2|3>
```

Add `--language american` only if the user explicitly requests American English.

The script outputs JSON to stdout. Parse it and present to the user.

### Step 3: Present results

Format the output like this:

```
## Proofreading complete: <filename>

**Auto-applied (<count> corrections):**
- Line <n>: "<from>" → "<to>"
- ...

**Suggestions for review:**
- [S1] Line <n>: <description> (suggested: "<replacement>")
- [S2] Line <n>: <description>
- ...

Corrected file saved to: <filename>.proofread.md

**To accept suggestions**, type their IDs (e.g., "S1 S3") or "all", or "none" to skip.
```

Spellcheck mode does not auto-apply corrections; it reports all possible spelling fixes as suggestions for review.

### Step 4: Apply accepted suggestions

When the user provides IDs:

```bash
cd "$skill_dir" && yarn -s apply "<file>.proofread.md" <S1 S2 ...>
```

Or if they say "all":
```bash
cd "$skill_dir" && yarn -s apply "<file>.proofread.md" all
```

Or if they say "none":
```bash
cd "$skill_dir" && yarn -s apply "<file>.proofread.md" none
```

### Step 5: Confirm completion

Re-read the final file path if needed to confirm it exists and suggestion comments were removed.

```
Final file saved to: <filename>.final.md

Applied: S1, S3
Removed: S2, S4
```

## First-time setup

If the user hasn't installed dependencies yet:

```bash
cd "$skill_dir" && yarn install
```

## Configuration

The skill uses these environment variables from `.env` (LLM modes only):
- `GOOGLE_AI_API_KEY`: Google AI API key for Gemini (required for LLM modes)
- `PROOFREAD_MODEL`: Model ID (default: gemini-2.0-flash)

The spellcheck engine requires `aspell` to be installed:
```bash
brew install aspell
```

## Output files

- `<filename>.proofread.md`: Auto-corrections applied, suggestions as HTML comments
- `<filename>.final.md`: After accepting/rejecting suggestions

## Notes

- Uses British English conventions
- Preserves author's voice and technical terminology
