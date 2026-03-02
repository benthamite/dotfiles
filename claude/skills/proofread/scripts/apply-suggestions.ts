#!/usr/bin/env npx tsx

import { readFileSync, writeFileSync } from "fs";
import { basename, dirname, join } from "path";

interface ApplyResult {
  file: string;
  finalFile: string;
  applied: string[];
  removed: string[];
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

  // Process each line - handle multiple comments per line
  const lines = text.split("\n");
  const processedLines: string[] = [];

  // Regex to match suggestion comments globally
  const commentRegex = /\s*<!--\s*\[(S\d+)\]\s*REVIEW:\s*(.*?)(?:\s*Suggested:\s*"([^"]*)")?\s*-->/g;

  for (const line of lines) {
    let processedLine = line;
    let match;

    // Find all comments on this line
    const commentsOnLine: Array<{fullMatch: string; id: string; suggested: string | undefined}> = [];
    const tempLine = line;

    while ((match = commentRegex.exec(tempLine)) !== null) {
      commentsOnLine.push({
        fullMatch: match[0],
        id: match[1],
        suggested: match[3]
      });
    }
    // Reset regex lastIndex for next line
    commentRegex.lastIndex = 0;

    if (commentsOnLine.length > 0) {
      for (const comment of commentsOnLine) {
        if (acceptedIds.includes(comment.id) || acceptedIds.includes("ALL")) {
          applied.push(comment.id);
        } else {
          removed.push(comment.id);
        }
        // Remove the comment from the line
        processedLine = processedLine.replace(comment.fullMatch, "");
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
      "Usage: npx tsx apply-suggestions.ts <file.proofread.md> <S1 S2 ...> | all"
    );
    console.error("");
    console.error("Examples:");
    console.error("  npx tsx apply-suggestions.ts doc.proofread.md S1 S3 S5");
    console.error("  npx tsx apply-suggestions.ts doc.proofread.md all");
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
