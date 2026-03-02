/**
 * Per-model synthesis: compare N responses from a single model and pick the best.
 * Uses Gemini 3 Flash (fast, cheap) with Claude 4.5 Sonnet as fallback.
 */

import { generateText } from 'ai';
import { google } from '@ai-sdk/google';
import { createAnthropic } from '@ai-sdk/anthropic';

export interface SampleResult {
  index: number;
  response: string;
  latencyMs: number;
  tokensUsed?: number;
}

export interface PerModelSynthesisResult {
  bestIndex: number;
  comparisonMarkdown: string;
  consistentPoints: string[];
  uniquePoints: string[];
  contradictions: string[];
}

export interface BrainstormResult {
  mergedIdeas: string;
  comparisonMarkdown: string;
}

function buildComparisonPrompt(
  modelName: string,
  originalPrompt: string,
  samples: SampleResult[],
): string {
  const samplesSection = samples
    .map((s, i) => `### Sample ${i + 1}\n\n${s.response}\n`)
    .join('\n---\n\n');

  return `# Per-model comparison

You are comparing ${samples.length} responses from the same model (**${modelName}**) to the same prompt, generated with temperature variation. Your job is to pick the best one and analyse the variation.

## Original prompt

${originalPrompt}

## Responses

${samplesSection}

---

## Instructions

Respond with EXACTLY this JSON structure (no markdown fencing, no extra text):

{
  "best_index": <1-based index of best response>,
  "reasoning": "<1-2 sentences explaining why this response is best>",
  "consistent_points": ["<point that appears in most/all samples — high confidence>"],
  "unique_points": ["<point unique to one sample — creative or potentially hallucinated>"],
  "contradictions": ["<point where samples disagree with each other>"]
}

### Selection criteria (in order of importance)

1. **Accuracy** — fewest factual errors or unsupported claims
2. **Completeness** — covers the question thoroughly
3. **Clarity** — well-organised, easy to follow
4. **Specificity** — concrete details over vague generalities

Pick the response that best combines these qualities. If responses are very similar, prefer the one with the clearest structure.`;
}

function buildBrainstormPrompt(
  modelName: string,
  originalPrompt: string,
  samples: SampleResult[],
): string {
  const samplesSection = samples
    .map((s, i) => `### Sample ${i + 1}\n\n${s.response}\n`)
    .join('\n---\n\n');

  return `# Brainstorm idea extraction

You are reviewing ${samples.length} responses from the same model (**${modelName}**) to the same brainstorming prompt, generated with temperature variation. Your job is to extract and merge ALL unique ideas into one comprehensive list.

## Original prompt

${originalPrompt}

## Responses

${samplesSection}

---

## Instructions

Create a **single merged list** of every distinct idea across all ${samples.length} samples.

For each idea:
- Use the best name from any sample (or improve it slightly)
- Write the best 2-sentence description, drawing from whichever sample described it most compellingly
- Note in parentheses how many of the ${samples.length} samples included this or a very similar idea, e.g. "(3/${samples.length} samples)" or "(1/${samples.length} — unique)"

### Rules
1. **Include everything** — if an idea appeared in even one sample, include it
2. **Merge duplicates** — combine ideas that are essentially the same concept into one entry, keeping the best phrasing
3. **Preserve novelty** — ideas unique to one sample are often the most creative; never drop them
4. **Mirror the format** — use the same structural format as the original responses

After the list, add a brief "---" separator and then 2-3 sentences noting: how many total unique ideas were found, how many recurred across most samples, and how many were unique to a single sample.

Output ONLY the merged list and the brief note. No other preamble or meta-commentary.`;
}

function extractJson(raw: string): Record<string, unknown> | null {
  // Strategy 1: Direct parse
  try {
    return JSON.parse(raw.trim());
  } catch {}

  // Strategy 2: Strip markdown code fences
  let cleaned = raw.trim();
  if (cleaned.startsWith('```')) {
    cleaned = cleaned.replace(/^```(?:json)?\s*\n?/, '').replace(/\n?```\s*$/, '');
    try {
      return JSON.parse(cleaned);
    } catch {}
  }

  // Strategy 3: Find JSON object anywhere in the text
  const firstBrace = raw.indexOf('{');
  const lastBrace = raw.lastIndexOf('}');
  if (firstBrace !== -1 && lastBrace > firstBrace) {
    try {
      return JSON.parse(raw.slice(firstBrace, lastBrace + 1));
    } catch {}
  }

  return null;
}

function parseComparisonResponse(raw: string, sampleCount: number): {
  bestIndex: number;
  reasoning: string;
  consistentPoints: string[];
  uniquePoints: string[];
  contradictions: string[];
} | null {
  const parsed = extractJson(raw);
  if (!parsed) return null;

  const bestIndex = parsed.best_index;
  if (typeof bestIndex !== 'number' || bestIndex < 1 || bestIndex > sampleCount) {
    return null;
  }

  return {
    bestIndex: (bestIndex as number) - 1, // convert to 0-based
    reasoning: (parsed.reasoning as string) || '',
    consistentPoints: Array.isArray(parsed.consistent_points) ? parsed.consistent_points : [],
    uniquePoints: Array.isArray(parsed.unique_points) ? parsed.unique_points : [],
    contradictions: Array.isArray(parsed.contradictions) ? parsed.contradictions : [],
  };
}

function formatComparisonMarkdown(
  modelName: string,
  bestIndex: number,
  sampleCount: number,
  reasoning: string,
  consistentPoints: string[],
  uniquePoints: string[],
  contradictions: string[],
): string {
  let md = `## Comparison notes\n\n`;
  md += `**Best response:** Sample ${bestIndex + 1} of ${sampleCount}\n\n`;
  md += `**Why:** ${reasoning}\n\n`;

  if (consistentPoints.length > 0) {
    md += `### Consistent across samples (high confidence)\n\n`;
    for (const p of consistentPoints) md += `- ${p}\n`;
    md += '\n';
  }

  if (uniquePoints.length > 0) {
    md += `### Unique to one sample (verify independently)\n\n`;
    for (const p of uniquePoints) md += `- ${p}\n`;
    md += '\n';
  }

  if (contradictions.length > 0) {
    md += `### Contradictions between samples\n\n`;
    for (const c of contradictions) md += `- ${c}\n`;
    md += '\n';
  }

  return md;
}

/**
 * Compare N samples from a single model and pick the best.
 */
export async function compareModelSamples(
  modelName: string,
  originalPrompt: string,
  samples: SampleResult[],
): Promise<PerModelSynthesisResult> {
  if (samples.length === 0) {
    throw new Error(`No samples to compare for ${modelName}`);
  }

  if (samples.length === 1) {
    return {
      bestIndex: 0,
      comparisonMarkdown: '## Comparison notes\n\nOnly one sample — no comparison needed.\n',
      consistentPoints: [],
      uniquePoints: [],
      contradictions: [],
    };
  }

  const prompt = buildComparisonPrompt(modelName, originalPrompt, samples);

  // Try Gemini 3 Flash first (fast, cheap)
  let raw: string | null = null;

  try {
    console.log(`  Comparing ${samples.length} samples for ${modelName} (Gemini 3 Flash)...`);
    const result = await generateText({
      model: google('gemini-3-flash-preview'),
      prompt,
      maxOutputTokens: 2000,
    });
    raw = result.text;
  } catch (err) {
    console.warn(`  Gemini 3 Flash failed for ${modelName}, trying Claude 4.5 Sonnet...`);
  }

  // Fallback to Claude 4.5 Sonnet
  if (!raw) {
    try {
      const anthropicProvider = createAnthropic({ baseURL: 'https://api.anthropic.com/v1' });
      const result = await generateText({
        model: anthropicProvider('claude-sonnet-4-5-20250929'),
        prompt,
        maxOutputTokens: 2000,
      });
      raw = result.text;
    } catch (err) {
      console.warn(`  Claude 4.5 Sonnet also failed for ${modelName}. Falling back to longest response.`);
    }
  }

  // Parse response
  if (raw) {
    const parsed = parseComparisonResponse(raw, samples.length);
    if (parsed) {
      return {
        bestIndex: parsed.bestIndex,
        comparisonMarkdown: formatComparisonMarkdown(
          modelName,
          parsed.bestIndex,
          samples.length,
          parsed.reasoning,
          parsed.consistentPoints,
          parsed.uniquePoints,
          parsed.contradictions,
        ),
        consistentPoints: parsed.consistentPoints,
        uniquePoints: parsed.uniquePoints,
        contradictions: parsed.contradictions,
      };
    }
    console.warn(`  Could not parse comparison for ${modelName}. Falling back to longest response.`);
  }

  // Final fallback: pick longest successful response
  let longestIdx = 0;
  let longestLen = 0;
  for (let i = 0; i < samples.length; i++) {
    if (samples[i].response.length > longestLen) {
      longestLen = samples[i].response.length;
      longestIdx = i;
    }
  }

  return {
    bestIndex: longestIdx,
    comparisonMarkdown: formatComparisonMarkdown(
      modelName,
      longestIdx,
      samples.length,
      'Fallback: selected longest response (comparison model unavailable)',
      [],
      [],
      [],
    ),
    consistentPoints: [],
    uniquePoints: [],
    contradictions: [],
  };
}

/**
 * Brainstorm mode: extract and merge ALL unique ideas from N samples.
 * Instead of picking one best response, produces a deduplicated merged list.
 */
export async function brainstormModelSamples(
  modelName: string,
  originalPrompt: string,
  samples: SampleResult[],
): Promise<BrainstormResult> {
  if (samples.length === 0) {
    throw new Error(`No samples for brainstorm extraction from ${modelName}`);
  }

  if (samples.length === 1) {
    return {
      mergedIdeas: samples[0].response,
      comparisonMarkdown: '## Extraction notes\n\nOnly one sample — no merging needed.\n',
    };
  }

  const prompt = buildBrainstormPrompt(modelName, originalPrompt, samples);
  let raw: string | null = null;

  try {
    console.log(
      `  Extracting ideas from ${samples.length} samples for ${modelName} (Gemini 3 Flash)...`,
    );
    const result = await generateText({
      model: google('gemini-3-flash-preview'),
      prompt,
      maxOutputTokens: 4000,
    });
    raw = result.text;
  } catch {
    console.warn(`  Gemini 3 Flash failed for ${modelName}, trying Claude 4.5 Sonnet...`);
  }

  if (!raw) {
    try {
      const anthropicProvider = createAnthropic({ baseURL: 'https://api.anthropic.com/v1' });
      const result = await generateText({
        model: anthropicProvider('claude-sonnet-4-5-20250929'),
        prompt,
        maxOutputTokens: 4000,
      });
      raw = result.text;
    } catch {
      console.warn(
        `  Claude 4.5 Sonnet also failed for ${modelName}. Using concatenation fallback.`,
      );
    }
  }

  if (raw) {
    // Split into merged ideas and notes if separator present
    const separatorIdx = raw.lastIndexOf('\n---\n');
    if (separatorIdx !== -1) {
      const mergedIdeas = raw.slice(0, separatorIdx).trim();
      const notes = raw.slice(separatorIdx + 5).trim();
      return {
        mergedIdeas,
        comparisonMarkdown: `## Extraction notes\n\n${notes}\n`,
      };
    }
    return {
      mergedIdeas: raw.trim(),
      comparisonMarkdown:
        '## Extraction notes\n\nIdeas extracted and merged across all samples.\n',
    };
  }

  // Fallback: concatenate all samples
  const allResponses = samples
    .map((s, i) => `**From sample ${i + 1}:**\n\n${s.response}`)
    .join('\n\n---\n\n');
  return {
    mergedIdeas: allResponses,
    comparisonMarkdown:
      '## Extraction notes\n\nFallback: concatenated all samples (extraction model unavailable).\n',
  };
}
