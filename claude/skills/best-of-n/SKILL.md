---
name: best-of-n
description: Run a Best-of-N multi-model query. Use when the user asks to best of n, /best-of-n, bon query, sample models repeatedly, compare multiple model responses, or synthesize the best answer across models.
argument-hint: <prompt>
---

# Best-of-N query

> Adapted from [Peter Hartree](https://pjh.is/)'s [HartreeWorks/skills](https://github.com/HartreeWorks/skills) repository.

Query each AI model N times with temperature variation, pick the best response per model, then synthesise across models. This produces higher-quality input for the final synthesis at the cost of N× more API calls.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

Use the live skill directory for the current tool:

- Claude Code: `${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills/best-of-n`
- Codex: `${CODEX_HOME:-$HOME/.codex}/skills/best-of-n`

In command examples below, set `BON_DIR` to the appropriate directory first:

```bash
# Claude Code
BON_DIR="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills/best-of-n"

# Codex
BON_DIR="${CODEX_HOME:-$HOME/.codex}/skills/best-of-n"

cd "$BON_DIR"
```

If dependencies are missing, run `yarn install` in `BON_DIR` before using the query commands.

## When not to use

- Do not use for ordinary questions where the user wants a direct answer from the current agent.
- Do not use just because the phrase "best of N" appears in unrelated prose.
- Do not use for deep research or browser-only model workflows unless the user explicitly asks to adapt this script; those models are excluded from normal Best-of-N sampling.

### Execution steps

#### Step 1: Get or draft the prompt

**A) Cold start** (no prior discussion): Use the provided prompt, or ask "What question would you like to send to multiple models?"

**B) Mid-conversation** (substantive prior discussion): Draft a comprehensive prompt that captures full context (2-4 paragraphs minimum), includes relevant file contents or code, states the core question clearly, and specifies what format/depth of response is useful. Save to `/tmp/bon-prompt-draft.md`, open it, and show the user for approval.

#### Step 2: Brainstorm detection

Check whether the prompt suggests brainstorming—look for keywords like "brainstorm", "ideas", "come up with", "creative", "possibilities", or any open-ended ideation task.

**If brainstorming detected**, say "Brainstorm mode: merges all unique ideas across samples instead of picking one best." Then use the host's structured question tool (`AskUserQuestion` in Claude Code, `request_user_input` in Codex when available; otherwise ask directly in chat) to present the presets.

Before presenting the options, run `yarn query presets` and treat `models.json`/the command output as the source of truth for model names, sample counts, temperatures, and call counts. The menu should have this shape:

- **Question:** "Which brainstorm preset?"
- **Header:** "Preset"
- **Options:**
  1. Label: `Quick (Recommended)` — Description: `4 models × 3 samples = 12 calls (~3 min). GPT-5.2, Opus 4.6, Gemini 3.1 Pro, Grok 4.1`
  2. Label: `Intense` — Description: `7 models × 5 samples = 35 calls (~8 min). GPT-5.2, Opus 4.6, Gemini 3.1 Pro, Grok 4.1, GPT-5.2 Pro, Sonnet 4.5, Gemini 3 Flash. Temp 1.1`
  3. Label: `Ultra` — Description: `7 models × 6 samples = 42 calls (~10 min). Same 7 models as Intense. Temperature sweep 0.5→1.5`
  4. Label: `Custom` — Description: `Pick your own models and settings`

Mapping user selection to CLI:
- Quick → `--preset brainstorm`. Skip Steps 2b and 3, go straight to Step 4.
- Intense → `--preset brainstorm-intense`. Skip Steps 2b and 3, go straight to Step 4.
- Ultra → `--preset brainstorm-ultra`. Skip Steps 2b and 3, go straight to Step 4.
- Custom → Fall through to Step 2b to pick models, then Step 3 for N/temperature. Add `--brainstorm` to the final command.

**If not brainstorming**, proceed to Step 2b.

#### Step 2b: Model selection

First, fetch available presets and models (defined in `models.json`):

```bash
cd "$BON_DIR" && yarn query presets
```

Present `quick`, `comprehensive`, and a "Pick models" option as a numbered menu. Add "(Recommended)" to `quick`. Do not present brainstorm, deep-research, or browser-oriented presets in the normal menu unless the user explicitly asked for those modes. Wait for user input.

Deep research and browser models are automatically excluded—they don't benefit from temperature sampling.

If the user selects **Pick models**, fetch the eligible model list:

```bash
cd "$BON_DIR" && yarn query models
```

Show the output and ask the user to enter model IDs (comma-separated).

#### Step 3: Ask N and temperature

Ask the user (or use defaults):
- **N** (samples per model): default 4
- **Temperature**: default 0.8

Show a cost warning: "This will make {models × N} API calls."

#### Step 4: Run the query

If the user chose a preset, use `--preset <name>`. If they picked individual models, use `--models <comma-separated-ids>`.

Generate a slug from the prompt (lowercase, non-alphanumeric to hyphens, max 50 chars).

For brainstorm presets (`brainstorm`, `brainstorm-intense`, `brainstorm-ultra`), do not pass `--num-samples` or `--temperature` unless the user explicitly overrides the preset; the preset already defines those values. For custom brainstorming, add `--brainstorm` to merge all unique ideas across samples instead of picking one best response per model.

With a brainstorm preset:

```bash
cd "$BON_DIR" && yarn query \
  --preset <brainstorm-preset-name> \
  --synthesise \
  --output-format both \
  "<prompt>"
```

With a normal preset:

```bash
cd "$BON_DIR" && yarn query \
  --preset <preset-name> \
  --num-samples <n> \
  --temperature <temp> \
  --synthesise \
  --output-format both \
  "<prompt>"
```

Or with explicit models:

```bash
cd "$BON_DIR" && yarn query \
  --models "<model-id-1>,<model-id-2>,..." \
  --num-samples <n> \
  --temperature <temp> \
  --synthesise \
  --output-format both \
  "<prompt>"
```

The script auto-generates an output directory at `data/model-outputs/<timestamp>-bon-<slug>/` containing `results.md`, `results.html`, and per-model responses.

#### Step 5: Open results

Read the "Results saved to:" line from the script output, then verify the output directory contains `results.md`; if `--output-format both` was used, verify `results.html` as well. Open the HTML version if available, otherwise markdown.

If `$INSIDE_EMACS` is set, use EWW:
```bash
emacsclient -e '(eww-open-file "<output-dir>/results.html")'
```

Otherwise:
```bash
open "<output-dir>/results.html"
```

In the final response, report the output directory, models that completed, failed models if any, and whether synthesis was produced.

---

## How it works

```
Prompt → [Model₁ × N] → Per-model comparison → Best₁ ─┐
         [Model₂ × N] → Per-model comparison → Best₂ ─┤→ Cross-model synthesis → Final
         [Model₃ × N] → Per-model comparison → Best₃ ─┘
```

1. All model × N queries run in parallel with 100ms stagger per same-provider call
2. Per-model comparison uses Gemini 3 Flash (fallback: Claude 4.5 Sonnet) to pick the best sample
3. Cross-model synthesis uses Claude Opus 4.6 with extended thinking

## Direct script invocation

```bash
cd "$BON_DIR"

# Basic usage
yarn query "What are the pros and cons of TypeScript?"

# With options
yarn query -n 4 -T 0.8 -p quick "Your question"
yarn query -n 2 -m gpt-5.2,gemini-3-flash "Your question"

# List eligible models and presets
yarn query models
yarn query presets
```

### CLI options

| Option | Default | Description |
|--------|---------|-------------|
| `-n, --num-samples <n>` | 4 | Samples per model |
| `-T, --temperature <temp>` | 0.8 | Temperature for all runs |
| `-m, --models <list>` | — | Comma-separated model IDs |
| `-p, --preset <name>` | quick | Preset: quick, comprehensive |
| `-t, --timeout <seconds>` | 180 | Timeout per call |
| `--output-format <format>` | markdown | markdown, html, both |
| `-s, --synthesise` | true | Run cross-model synthesis |
| `-B, --brainstorm` | false | Merge all unique ideas instead of picking one best |
| `-o, --output <dir>` | auto | Output directory |

## Temperature guidance

- **0.8** (default): Meaningful variation while staying coherent
- **0.3–0.5**: Less variation, useful for factual queries
- **1.0+**: More variation, can become erratic on some models
- Reasoning models (Anthropic thinking, OpenAI reasoning) have inherent stochasticity; temperature may be silently ignored by the SDK

## Output structure

```
data/model-outputs/2026-02-08-1430-bon-slug/
├── results.md          # Live results + synthesis (markdown)
├── results.html        # Live results + synthesis (HTML)
├── responses.json
└── per-model/
    ├── gpt-5.2-thinking/
    │   ├── sample-0.md … sample-3.md
    │   ├── comparison.md
    │   └── best-response.md
    └── claude-opus-4-6-thinking/
        └── …
```

In brainstorm mode, per-model directories contain `merged-ideas.md` instead of `best-response.md`.

## Configuration

API keys are stored in `.env` (gitignored). Model definitions and presets are in `models.json` (shipped). To customise, create a `config.json` with just the keys you want to override—it merges on top of `models.json`.
