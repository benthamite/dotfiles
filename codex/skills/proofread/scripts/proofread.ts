#!/usr/bin/env npx tsx

import { createGoogleGenerativeAI } from "@ai-sdk/google";
import { generateText } from "ai";
import { parse } from "dotenv";
import { existsSync, readFileSync, writeFileSync } from "fs";
import { basename, dirname, join } from "path";
import { execSync } from "child_process";

// Load environment variables ONLY from the skill's .env file (not shell environment)
const envPath = join(dirname(decodeURIComponent(new URL(import.meta.url).pathname)), "..", ".env");
let GOOGLE_AI_API_KEY: string | undefined;
let MODEL = "gemini-2.0-flash";

if (existsSync(envPath)) {
  const envConfig = parse(readFileSync(envPath));
  GOOGLE_AI_API_KEY = envConfig.GOOGLE_AI_API_KEY;
  MODEL = envConfig.PROOFREAD_MODEL || MODEL;
}

// Only create the Google client if we have an API key (spellcheck mode doesn't need it)
let google: ReturnType<typeof createGoogleGenerativeAI> | undefined;
if (GOOGLE_AI_API_KEY) {
  google = createGoogleGenerativeAI({
    apiKey: GOOGLE_AI_API_KEY,
  });
}

interface Change {
  line: number;
  type: "spelling" | "grammar" | "punctuation";
  from: string;
  to: string;
  context?: string;
}

interface Suggestion {
  id: string;
  line: number;
  type: "style" | "clarity" | "spelling";
  text: string;
  suggested: string | null;
  context?: string;
}

interface ProofreadResult {
  file: string;
  correctedFile: string;
  level: number;
  autoApplied: {
    count: number;
    changes: Change[];
  };
  suggestions: Suggestion[];
}

// Estimate tokens (rough: 1 token ≈ 4 chars for English)
function estimateTokens(text: string): number {
  return Math.ceil(text.length / 4);
}

// Split text into chunks if needed
function chunkText(text: string, maxTokens: number = 6000): string[] {
  const tokens = estimateTokens(text);
  if (tokens <= maxTokens) {
    return [text];
  }

  const lines = text.split("\n");
  const chunks: string[] = [];
  let currentChunk: string[] = [];
  let currentTokens = 0;

  for (const line of lines) {
    const lineTokens = estimateTokens(line);
    if (currentTokens + lineTokens > maxTokens && currentChunk.length > 0) {
      chunks.push(currentChunk.join("\n"));
      currentChunk = [];
      currentTokens = 0;
    }
    currentChunk.push(line);
    currentTokens += lineTokens;
  }

  if (currentChunk.length > 0) {
    chunks.push(currentChunk.join("\n"));
  }

  return chunks;
}

function buildPrompt(text: string, level: number, startLine: number, language: string = "british"): string {
  const langLabel = language === "american" ? "American English" : "British English";
  const levelInstructions = {
    1: `Focus ONLY on:
- Spelling errors
- Punctuation errors
- Clear grammar mistakes

Do NOT suggest style or clarity improvements.`,
    2: `Focus on:
- Spelling errors (auto-correct)
- Punctuation errors (auto-correct)
- Clear grammar mistakes (auto-correct)
- Top 5-10 most impactful style/clarity issues (as suggestions)

For style/clarity, only flag the most important issues like:
- Very long sentences (>40 words)
- Confusing pronoun references
- Passive voice where active would be much clearer`,
    3: `Provide comprehensive proofreading:
- Spelling errors (auto-correct)
- Punctuation errors (auto-correct)
- Clear grammar mistakes (auto-correct)
- All style suggestions
- All clarity suggestions

Be thorough but preserve the author's voice.`,
  };

  return `You are a professional proofreader. Review the following text using ${langLabel} conventions.

${levelInstructions[level as 1 | 2 | 3]}

IMPORTANT RULES:
1. Line numbers start at ${startLine} for this chunk
2. The "from" field must be the EXACT text to replace (case-sensitive)
3. Preserve the author's voice and technical terminology
4. Don't over-edit - only flag genuine issues
5. SKIP ANY TEXT containing: backslashes, curly braces, superscripts/subscripts, or anything that looks like LaTeX/math notation
6. DO NOT try to "fix" formatting of numbers, units, or mathematical expressions
7. DO NOT change British to American English or vice versa
8. Focus on ACTUAL spelling/grammar errors in plain prose only

Return a JSON array with this structure (no markdown, just raw JSON):
[
  {"line": <number>, "type": "auto-correction", "from": "<exact text>", "to": "<corrected>", "reason": "<brief reason>"},
  {"line": <number>, "type": "suggestion", "from": "<original text>", "to": "<suggested replacement>", "reason": "<brief reason>"}
]

If text has no issues, return: []

TEXT TO PROOFREAD:
\`\`\`
${text}
\`\`\``;
}

// Extract and sanitize JSON from response text
function extractJson(response: string): string {
  let jsonStr: string;

  // Pattern 1: ```json ... ```
  let match = response.match(/```json\s*([\s\S]*?)```/);
  if (match) {
    jsonStr = match[1].trim();
  } else {
    // Pattern 2: ``` ... ```
    match = response.match(/```\s*([\s\S]*?)```/);
    if (match) {
      jsonStr = match[1].trim();
    } else {
      // Pattern 3: Find JSON array directly
      match = response.match(/\[[\s\S]*\]/);
      jsonStr = match ? match[0].trim() : response.trim();
    }
  }

  // Sanitize common JSON issues from LLMs:
  // - Fix unescaped newlines in strings
  // - Fix unescaped backslashes
  jsonStr = jsonStr
    .replace(/\n\s*(?=[^"]*"[^"]*$)/gm, ' ') // Replace newlines inside strings
    .replace(/(?<!\\)\\(?!["\\/bfnrt]|u[0-9a-fA-F]{4})/g, '\\\\'); // Escape invalid backslashes

  return jsonStr;
}

async function proofreadChunk(
  text: string,
  level: number,
  startLine: number,
  language: string = "british"
): Promise<{ autoCorrections: Change[]; suggestions: Suggestion[] }> {
  if (!google) {
    console.error("Error: GOOGLE_AI_API_KEY not configured in the skill's .env file");
    console.error("Please add your Google AI API key to: " + envPath);
    process.exit(1);
  }

  const prompt = buildPrompt(text, level, startLine, language);

  try {
    const { text: response } = await generateText({
      model: google(MODEL),
      prompt,
    });

    const jsonStr = extractJson(response);
    let items;
    try {
      items = JSON.parse(jsonStr);
    } catch (parseError) {
      console.error("JSON parse error:", parseError);
      console.error("First 500 chars of response:", response.slice(0, 500));
      return { autoCorrections: [], suggestions: [] };
    }

    if (!Array.isArray(items)) {
      console.error("Response is not an array:", typeof items);
      return { autoCorrections: [], suggestions: [] };
    }

    // Split flat array into auto-corrections and suggestions
    const autoCorrections: Change[] = [];
    const suggestions: Suggestion[] = [];

    for (const item of items) {
      if (item.type === "auto-correction") {
        autoCorrections.push({
          line: item.line,
          type: "grammar", // Generic type for auto-corrections
          from: item.from,
          to: item.to,
          context: item.reason,
        });
      } else if (item.type === "suggestion") {
        suggestions.push({
          id: "", // Will be assigned later
          line: item.line,
          type: "style", // Generic type for suggestions
          text: item.reason || "Style/clarity suggestion",
          suggested: item.to,
          context: item.from,
        });
      }
    }

    return { autoCorrections, suggestions };
  } catch (error) {
    console.error("Error calling Gemini API:", error);
    return { autoCorrections: [], suggestions: [] };
  }
}

function applyAutoCorrections(text: string, corrections: Change[]): string {
  const lines = text.split("\n");

  // Sort corrections by line number descending to avoid index shifts
  const sorted = [...corrections].sort((a, b) => b.line - a.line);

  for (const correction of sorted) {
    const lineIndex = correction.line - 1;
    if (lineIndex >= 0 && lineIndex < lines.length) {
      lines[lineIndex] = lines[lineIndex].replace(correction.from, correction.to);
    }
  }

  return lines.join("\n");
}

function insertSuggestionComments(text: string, suggestions: Suggestion[]): string {
  const lines = text.split("\n");

  // Sort by line number descending to avoid index shifts
  const sorted = [...suggestions].sort((a, b) => b.line - a.line);

  for (const suggestion of sorted) {
    const lineIndex = suggestion.line - 1;
    if (lineIndex >= 0 && lineIndex < lines.length) {
      const comment = `<!-- [${suggestion.id}] REVIEW: ${suggestion.text}${
        suggestion.suggested ? ` Suggested: "${suggestion.suggested}"` : ""
      } -->`;
      // Insert comment at end of the line
      lines[lineIndex] = lines[lineIndex] + " " + comment;
    }
  }

  return lines.join("\n");
}

async function proofread(
  filePath: string,
  level: number,
  language: string = "british"
): Promise<ProofreadResult> {
  const text = readFileSync(filePath, "utf-8");
  const fileName = basename(filePath);
  const fileDir = dirname(filePath);
  const correctedFileName = fileName.replace(/\.md$/, ".proofread.md");
  const correctedFilePath = join(fileDir, correctedFileName);

  const chunks = chunkText(text);
  const allAutoCorrections: Change[] = [];
  const allSuggestions: Suggestion[] = [];

  let currentLine = 1;
  let suggestionCounter = 1;

  for (let i = 0; i < chunks.length; i++) {
    console.error(`Processing chunk ${i + 1}/${chunks.length}...`);

    const { autoCorrections, suggestions } = await proofreadChunk(
      chunks[i],
      level,
      currentLine,
      language
    );

    allAutoCorrections.push(...autoCorrections);

    // Assign IDs to suggestions
    for (const suggestion of suggestions) {
      allSuggestions.push({
        ...suggestion,
        id: `S${suggestionCounter++}`,
      });
    }

    currentLine += chunks[i].split("\n").length;
  }

  // Apply auto-corrections
  let correctedText = applyAutoCorrections(text, allAutoCorrections);

  // Insert suggestion comments
  correctedText = insertSuggestionComments(correctedText, allSuggestions);

  // Write corrected file
  writeFileSync(correctedFilePath, correctedText, "utf-8");

  return {
    file: fileName,
    correctedFile: correctedFileName,
    level,
    autoApplied: {
      count: allAutoCorrections.length,
      changes: allAutoCorrections,
    },
    suggestions: allSuggestions,
  };
}

// Deterministic spell-check using aspell
async function spellcheck(filePath: string): Promise<ProofreadResult> {
  const text = readFileSync(filePath, "utf-8");
  const fileName = basename(filePath);
  const fileDir = dirname(filePath);
  const correctedFileName = fileName.replace(/\.md$/, ".proofread.md");
  const correctedFilePath = join(fileDir, correctedFileName);

  console.error("Running aspell spell-check (British English)...");

  const lines = text.split("\n");
  const allAutoCorrections: Change[] = [];

  // Check if aspell is available
  try {
    execSync("which aspell", { stdio: "ignore" });
  } catch {
    console.error("Error: aspell not found. Install with: brew install aspell");
    process.exit(1);
  }

  // Process each line with aspell
  for (let lineNum = 0; lineNum < lines.length; lineNum++) {
    const line = lines[lineNum];

    // Skip lines that look like they contain code/LaTeX
    if (line.match(/[\\{}$`]/) || line.match(/^```/) || line.match(/^\s*[-*]\s*\[/)) {
      continue;
    }

    // Extract plain words (skip URLs, markdown links, etc.)
    const cleanLine = line
      .replace(/\[([^\]]+)\]\([^)]+\)/g, "$1") // Extract link text
      .replace(/https?:\/\/[^\s]+/g, "") // Remove URLs
      .replace(/`[^`]+`/g, "") // Remove inline code
      .replace(/[_*]+/g, " "); // Remove emphasis markers

    if (!cleanLine.trim()) continue;

    try {
      // Run aspell on the line
      const result = execSync(
        `aspell -a --lang=en_GB 2>/dev/null`,
        { input: cleanLine + '\n', encoding: "utf-8", maxBuffer: 1024 * 1024 }
      );

      // Parse aspell output
      // Format: & word count offset: suggestion1, suggestion2, ...
      // Or: # word offset (no suggestions)
      for (const aspellLine of result.split("\n")) {
        const match = aspellLine.match(/^& (\S+) \d+ (\d+): (.+)$/);
        if (match) {
          const [, misspelled, , suggestionsStr] = match;
          const suggestions = suggestionsStr.split(", ");

          // Skip acronyms (all uppercase)
          if (misspelled === misspelled.toUpperCase() && misspelled.length > 1) continue;

          // Skip proper nouns (starts with capital, rest lowercase)
          if (/^[A-Z][a-z]+$/.test(misspelled)) continue;

          // Skip very short words (often abbreviations)
          if (misspelled.length <= 2) continue;

          // Skip words with numbers
          if (/\d/.test(misspelled)) continue;

          // Only auto-correct if suggestion is very similar (likely a typo)
          if (suggestions.length > 0 && suggestions[0]) {
            const suggestion = suggestions[0];

            // Calculate similarity - only correct if difference is small
            const lenDiff = Math.abs(misspelled.length - suggestion.length);
            const isSimilar = lenDiff <= 2 &&
              (misspelled.toLowerCase().includes(suggestion.toLowerCase().slice(0, 3)) ||
               suggestion.toLowerCase().includes(misspelled.toLowerCase().slice(0, 3)));

            // Only include if it looks like a genuine typo
            if (isSimilar && line.includes(misspelled)) {
              allAutoCorrections.push({
                line: lineNum + 1,
                type: "spelling",
                from: misspelled,
                to: suggestion,
                context: `Suggestion: ${suggestions.slice(0, 3).join(", ")}`,
              });
            }
          }
        }
      }
    } catch {
      // Ignore aspell errors for individual lines
    }
  }

  // Deduplicate corrections (same word on same line)
  const seen = new Set<string>();
  const uniqueCorrections = allAutoCorrections.filter((c) => {
    const key = `${c.line}:${c.from}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });

  // In spellcheck mode, don't auto-apply - just report as suggestions
  // This is safer since aspell has limited vocabulary
  const suggestions: Suggestion[] = uniqueCorrections.map((c, i) => ({
    id: `S${i + 1}`,
    line: c.line,
    type: "spelling" as const,
    text: `Possible misspelling: "${c.from}"`,
    suggested: c.to,
    context: c.context,
  }));

  // Write original file with suggestion comments (if any)
  const correctedText = insertSuggestionComments(text, suggestions);
  writeFileSync(correctedFilePath, correctedText, "utf-8");

  return {
    file: fileName,
    correctedFile: correctedFileName,
    level: 1, // Spellcheck is always level 1 (mechanical only)
    autoApplied: {
      count: 0, // No auto-corrections in spellcheck mode
      changes: [],
    },
    suggestions, // All findings are suggestions for review
  };
}

// CLI entry point
async function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error("Usage: npx tsx proofread.ts <file.md> [--engine llm|spellcheck] [--level 1|2|3] [--language british|american]");
    console.error("");
    console.error("Engines:");
    console.error("  llm        - Gemini-based proofreading (default)");
    console.error("  spellcheck - Fast deterministic spell-check using aspell");
    console.error("");
    console.error("Levels (llm engine only):");
    console.error("  1 - Mechanical only (spelling, punctuation, grammar)");
    console.error("  2 - Light style pass (+ top 5-10 style suggestions)");
    console.error("  3 - Comprehensive review (all suggestions)");
    process.exit(1);
  }

  const filePath = args[0];
  let level = 2; // Default to level 2
  let engine = "llm"; // Default to LLM
  let language = "british"; // Default to British English

  const engineIndex = args.indexOf("--engine");
  if (engineIndex !== -1 && args[engineIndex + 1]) {
    engine = args[engineIndex + 1];
    if (!["llm", "spellcheck"].includes(engine)) {
      console.error("Error: Engine must be 'llm' or 'spellcheck'");
      process.exit(1);
    }
  }

  const levelIndex = args.indexOf("--level");
  if (levelIndex !== -1 && args[levelIndex + 1]) {
    level = parseInt(args[levelIndex + 1], 10);
    if (level < 1 || level > 3) {
      console.error("Error: Level must be 1, 2, or 3");
      process.exit(1);
    }
  }

  const langIndex = args.indexOf("--language");
  if (langIndex !== -1 && args[langIndex + 1]) {
    language = args[langIndex + 1].toLowerCase();
    if (language !== "british" && language !== "american") {
      console.error("Error: Language must be 'british' or 'american'");
      process.exit(1);
    }
  }

  try {
    let result: ProofreadResult;

    if (engine === "spellcheck") {
      result = await spellcheck(filePath);
    } else {
      result = await proofread(filePath, level, language);
    }

    // Output JSON to stdout for Claude to parse
    console.log(JSON.stringify(result, null, 2));
  } catch (error) {
    console.error("Error:", error);
    process.exit(1);
  }
}

main();
