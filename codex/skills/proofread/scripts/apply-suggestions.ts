#!/usr/bin/env npx tsx

import { readFileSync, writeFileSync } from "fs";
import { basename, dirname, join } from "path";

interface ApplyResult {
  file: string;
  finalFile: string;
  applied: string[];
  removed: string[];
}

interface EncodedSuggestion {
  id: string;
  text?: string;
  from?: string;
  to?: string | null;
}

interface SuggestionComment {
  fullMatch: string;
  id: string;
  from?: string;
  to?: string | null;
}

function parseEncodedSuggestion(fullMatch: string, encoded: string): SuggestionComment | null {
  try {
    const payload = JSON.parse(decodeURIComponent(encoded.trim())) as EncodedSuggestion;
    if (!payload.id) {
      return null;
    }
    return {
      fullMatch,
      id: payload.id,
      from: payload.from,
      to: payload.to,
    };
  } catch {
    return null;
  }
}

function collectSuggestionComments(line: string): SuggestionComment[] {
  const comments: SuggestionComment[] = [];

  const encodedRegex = /\s*<!--\s*proofread:([\s\S]*?)\s*-->/g;
  let match;
  while ((match = encodedRegex.exec(line)) !== null) {
    const parsed = parseEncodedSuggestion(match[0], match[1]);
    if (parsed) {
      comments.push(parsed);
    }
  }

  if (comments.length > 0) {
    return comments;
  }

  // Backward compatibility for files generated before encoded comments existed.
  const legacyRegex = /\s*<!--\s*\[(S\d+)\]\s*REVIEW:\s*(.*?)(?:\s*Suggested:\s*"([^"]*)")?\s*-->/g;
  while ((match = legacyRegex.exec(line)) !== null) {
    comments.push({
      fullMatch: match[0],
      id: match[1],
      to: match[3],
    });
  }

  return comments;
}

function applySuggestions(
  filePath: string,
  acceptedIds: string[]
): ApplyResult {
  const text = readFileSync(filePath, "utf-8");
  const fileName = basename(filePath);
  const fileDir = dirname(filePath);

  // Final file removes .proofread from name
  const finalFileName = fileName.replace(".proofread.md", ".final.md");
  const finalFilePath = join(fileDir, finalFileName);

  const applied: string[] = [];
  const removed: string[] = [];
  const accepted = new Set(acceptedIds.map((id) => id.toUpperCase()));
  const acceptAll = accepted.has("ALL");

  // Process each line - handle multiple comments per line
  const lines = text.split("\n");
  const processedLines: string[] = [];

  for (const line of lines) {
    let processedLine = line;
    const commentsOnLine = collectSuggestionComments(line);

    if (commentsOnLine.length > 0) {
      for (const comment of commentsOnLine) {
        const shouldApply = acceptAll || accepted.has(comment.id.toUpperCase());
        // Remove the review comment before applying replacements so matches in
        // the comment payload itself cannot be altered.
        processedLine = processedLine.replace(comment.fullMatch, "");

        if (shouldApply && comment.from && comment.to && processedLine.includes(comment.from)) {
          processedLine = processedLine.replace(comment.from, comment.to);
          applied.push(comment.id);
        } else {
          removed.push(comment.id);
        }
      }
      processedLines.push(processedLine.trimEnd());
    } else {
      processedLines.push(line);
    }
  }

  const finalText = processedLines.join("\n");
  writeFileSync(finalFilePath, finalText, "utf-8");

  return {
    file: fileName,
    finalFile: finalFileName,
    applied,
    removed,
  };
}

// CLI entry point
function main() {
  const args = process.argv.slice(2);

  if (args.length < 2) {
    console.error(
      "Usage: yarn -s apply <file.proofread.md> <S1 S2 ...> | all | none"
    );
    console.error("");
    console.error("Examples:");
    console.error("  yarn -s apply doc.proofread.md S1 S3 S5");
    console.error("  yarn -s apply doc.proofread.md all");
    console.error("  yarn -s apply doc.proofread.md none");
    process.exit(1);
  }

  const filePath = args[0];
  const acceptedIds = args.slice(1).map((id) => id.toUpperCase());

  try {
    const result = applySuggestions(filePath, acceptedIds);
    console.log(JSON.stringify(result, null, 2));
  } catch (error) {
    console.error("Error:", error);
    process.exit(1);
  }
}

main();
