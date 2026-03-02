---
name: proofread
description: This skill should be used when the user says "proofread", "spell check", "check spelling", "check grammar", "proofread this document", "review for typos", "proofread my article", "check this for errors", or mentions proofreading a markdown file.
---

# Proofread skill

Proofreading for markdown documents using British English conventions. Two engines available:

- **Spellcheck** (fast, deterministic): Uses aspell for spell-checking (~2 seconds)
- **LLM** (thorough, AI-powered): Uses Gemini Flash for spelling, grammar, style, and clarity (~30-60 seconds per 100 lines)

## How it works

1. **Spellcheck engine**: Reports possible misspellings as suggestions for review (no auto-apply, since aspell has limited vocabulary)
2. **LLM engine**: Auto-applies safe corrections (spelling, punctuation, grammar), flags style/clarity as suggestions with IDs (S1, S2, etc.)
3. **Interactive acceptance**: User types suggestion IDs to accept them

## Workflow

### Step 1: Ask which engine and level

When the user wants to proofread a document, ask:

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

If the user chose an LLM level, check if the API key is configured:

1. Read `~/.claude/skills/proofread/.env`
2. Check if it contains `GOOGLE_AI_API_KEY=` with a value

**If the key is missing or the file doesn't exist**, tell the user:

> The LLM proofreading modes require a Google AI API key to use Gemini.
>
> To set it up:
> 1. Go to https://aistudio.google.com/app/apikey
> 2. Create an API key
> 3. Add it to `~/.claude/skills/proofread/.env`:
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
cd ~/.claude/skills/proofread && npx tsx scripts/proofread.ts "<file_path>" --engine spellcheck
```

For LLM engine:
```bash
cd ~/.claude/skills/proofread && npx tsx scripts/proofread.ts "<file_path>" --engine llm --level <1|2|3>
```

The script outputs JSON to stdout. Parse it and present to the user.

### Step 3: Present results

Format the output like this:

```
## Proofreading complete: <filename>

**Auto-applied (<count> corrections):**
- Line <n>: "<from>" → "<to>"
- ...

**Suggestions for review:**
- [S1] Line <n>: <description>
- [S2] Line <n>: <description>
- ...

Corrected file saved to: <filename>.proofread.md

**To accept suggestions**, type their IDs (e.g., "S1 S3") or "all", or "none" to skip.
```

### Step 4: Apply accepted suggestions

When the user provides IDs:

```bash
cd ~/.claude/skills/proofread && npx tsx scripts/apply-suggestions.ts "<file>.proofread.md" <S1 S2 ...>
```

Or if they say "all":
```bash
cd ~/.claude/skills/proofread && npx tsx scripts/apply-suggestions.ts "<file>.proofread.md" all
```

### Step 5: Confirm completion

```
Final file saved to: <filename>.final.md

Applied: S1, S3
Removed: S2, S4
```

## First-time setup

If the user hasn't installed dependencies yet:

```bash
cd ~/.claude/skills/proofread && yarn install
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
- Processes long documents in chunks automatically
- Progress shown via stderr, results via stdout (JSON)
