/**
 * Cross-model synthesis: combine best-of-n responses from multiple models.
 * Uses Claude Opus 4.6 with extended thinking (same as AMM).
 */

import { generateText } from 'ai';
import { createAnthropic } from '@ai-sdk/anthropic';

interface BestResponse {
  modelName: string;
  displayName: string;
  response: string;
  sampleCount: number;
  bestIndex: number;
}

/**
 * Generate the synthesis prompt, adapted from AMM's generateSynthesisPrompt
 * with added best-of-n context.
 */
function buildCrossModelPrompt(
  originalPrompt: string,
  bestResponses: BestResponse[],
): string {
  const responsesSection = bestResponses
    .map(
      (r) => `## ${r.displayName} (best of ${r.sampleCount} samples)

${r.response}
`,
    )
    .join('\n---\n\n');

  return `# Cross-model synthesis (best-of-N)

You are synthesising the best responses from ${bestResponses.length} AI models. Each response was selected as the best out of N samples (generated with temperature variation), so these represent each model's strongest output.

## Original prompt

${originalPrompt}

## Best responses

${responsesSection}

---

## Synthesis task

Create an **executive synthesis** that:
- Summarises the key consensus in 2-3 sentences
- Lists 4-6 key findings as bullet points
- Notes any disagreements with brief analysis
- Highlights unique insights worth preserving

### Key principles

1. **Identify consensus**: What do multiple models agree on? This is likely reliable.

2. **Highlight unique insights**: What did only one model mention that's valuable?
   - Tag the source: "[From GPT-5.2]" or similar
   - Don't discard these just because others didn't mention them

3. **Flag disagreements**: Where do models contradict?
   - Present both positions fairly
   - Analyse which seems more credible and why

4. **Remove duplication**: Don't repeat the same point multiple times

5. **Preserve nuance**: Keep qualifications and uncertainty expressed by models

### Output format

### Executive summary
[2-3 sentences capturing the core answer]

### Key findings
- [Bullet points of main findings]

### Points of disagreement
- [Any contradictions, or "None significant" if models agreed]

### Unique insights
- **[Model name]**: [Notable insight only this model provided]

### Confidence level
[One sentence on how confident we should be based on model agreement]

---

Please generate the synthesis now.`;
}

/**
 * Build prompt for brainstorm cross-model synthesis.
 * Focuses on collecting ALL unique ideas rather than consensus analysis.
 */
function buildBrainstormCrossModelPrompt(
  originalPrompt: string,
  bestResponses: BestResponse[],
): string {
  const responsesSection = bestResponses
    .map(
      (r) => `## ${r.displayName} (merged from ${r.sampleCount} samples)

${r.response}
`,
    )
    .join('\n---\n\n');

  return `# Cross-model brainstorm synthesis

You are synthesising brainstorming results from ${bestResponses.length} AI models. Each model was queried multiple times with temperature variation, and their responses were merged into comprehensive idea lists. Your job is to create the ultimate combined list.

## Original prompt

${originalPrompt}

## Merged ideas per model

${responsesSection}

---

## Synthesis task

Create a **comprehensive master list** of all ideas, organised into thematic clusters.

### Goals
1. **Include every unique idea** — if any model proposed it, include it
2. **Merge duplicates across models** — note when multiple models independently proposed the same idea (strong signal)
3. **Preserve the best framing** — for each idea, use whichever model's name and description was most compelling
4. **Tag sources** — after each idea, note which models proposed it in brackets
5. **Cluster by theme** — group related ideas under thematic headings

### Output format

**Start with the top picks**, then give the full breakdown:

### Top 5 most promising ideas
[Ranked list with 1-sentence justification each — this is the most important section]

---

Then for each theme:

### [Theme name]

1. **[Idea name]** — [Best 2-sentence description]. [Source models]
2. ...

Then at the end:

---

### Summary statistics
- Total unique ideas across all models: [count]
- Ideas proposed by multiple models independently: [count] (highest confidence)
- Ideas unique to one model: [count] (most novel)

---

Please generate the synthesis now.`;
}

/**
 * Run cross-model synthesis on the best responses from each model.
 * In brainstorm mode, uses a prompt focused on collecting all unique ideas.
 */
export async function synthesiseBestResponses(
  originalPrompt: string,
  bestResponses: BestResponse[],
  brainstorm: boolean = false,
): Promise<string> {
  const successfulResponses = bestResponses.filter((r) => r.response);

  if (successfulResponses.length === 0) {
    return 'No successful responses to synthesise.';
  }

  const prompt = brainstorm
    ? buildBrainstormCrossModelPrompt(originalPrompt, successfulResponses)
    : buildCrossModelPrompt(originalPrompt, successfulResponses);

  console.log(
    `\n\x1b[35m✨ Running cross-model synthesis with Claude Opus 4.6 (${successfulResponses.length} best responses)\x1b[0m\n`,
  );

  const anthropicProvider = createAnthropic({
    baseURL: 'https://api.anthropic.com/v1',
  });

  const result = await generateText({
    model: anthropicProvider('claude-opus-4-6'),
    prompt,
    maxOutputTokens: 16000,
    providerOptions: {
      anthropic: {
        thinking: {
          type: 'enabled',
          budgetTokens: 10000,
        },
      },
    },
  });

  return result.text;
}
