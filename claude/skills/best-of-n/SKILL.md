---
name: best-of-n
description: This skill should be used when the user asks to "best of n", "best-of-n query", "run best of n", "bon query", "bon", "/best-of-n", "sample multiple responses", "sample each model", or mentions wanting to query each AI model multiple times and pick the best response before synthesising across models.
argument-hint: <prompt>
---

# Best-of-N query

Query each AI model N times with temperature variation, pick the best response per model, then synthesise across models. This produces higher-quality input for the final synthesis at the cost of N× more API calls.

## When this skill is invoked

**IMPORTANT**: When triggered, follow the execution steps below. Do NOT just describe what the skill does.

### Execution steps

#### Step 1: Get or draft the prompt

**A) Cold start** (no prior discussion): Use the provided prompt, or ask "What question would you like to send to multiple models?"

**B) Mid-conversation** (substantive prior discussion): Draft a comprehensive prompt that captures full context (2-4 paragraphs minimum), includes relevant file contents or code, states the core question clearly, and specifies what format/depth of response is useful. Save to `/tmp/bon-prompt-draft.md`, open it, and show the user for approval.

#### Step 2: Brainstorm detection

Check whether the prompt suggests brainstorming—look for keywords like "brainstorm", "ideas", "come up with", "creative", "possibilities", or any open-ended ideation task.

**If brainstorming detected**, say "Brainstorm mode: merges all unique ideas across samples instead of picking one best." Then use `AskUserQuestion` to present the presets:

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
cd ~/.claude/skills/best-of-n && yarn query presets 2>/dev/null
```

Present the non-brainstorm presets as a numbered menu, plus a "Pick models" option. Add "(Recommended)" to the quick preset. Wait for user input.

Deep research and browser models are automatically excluded—they don't benefit from temperature sampling.

If the user selects **Pick models**, fetch the eligible model list:

```bash
cd ~/.claude/skills/best-of-n && yarn query models 2>/dev/null
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

For brainstorming prompts, add `--brainstorm` to merge all unique ideas across samples instead of picking one best response per model.

```bash
cd ~/.claude/skills/best-of-n && yarn query \
  --preset <preset-name> \
  --num-samples <n> \
  --temperature <temp> \
  --synthesise \
  --output-format both \
  "<prompt>"
```

Or with explicit models:

```bash
cd ~/.claude/skills/best-of-n && yarn query \
  --models "<model-id-1>,<model-id-2>,..." \
  --num-samples <n> \
  --temperature <temp> \
  --synthesise \
  --output-format both \
  "<prompt>"
```

The script auto-generates an output directory at `data/model-outputs/<timestamp>-bon-<slug>/` containing `results.md`, `results.html`, and per-model responses.

#### Step 5: Open results

Open the results file (HTML version if available, otherwise markdown).

If `$INSIDE_EMACS` is set, use EWW:
```bash
emacsclient -e '(eww-open-file "<output-dir>/results.html")'
```

Otherwise:
```bash
open "<output-dir>/results.html"
```

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
cd ~/.claude/skills/best-of-n

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

## Configuration

API keys are stored in `.env` (gitignored). Model definitions and presets are in `models.json` (shipped). To customise, create a `config.json` with just the keys you want to override—it merges on top of `models.json`.

