#!/usr/bin/env node

import { basename, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { applyEdits, fail, joinLines, outputPath, protectedLines, publish, readSnapshot, reportError, splitLines } from "./proofread.ts";
import type { Edit, Marker } from "./proofread.ts";

interface Parsed { id: string; payload: Marker | null }
export function applySuggestions(file: string, selection: string[]): object {
  const output = outputPath(file, true);
  const snapshot = readSnapshot(file);
  const accepted = selection.map(id => id.toUpperCase());
  if (!accepted.length || new Set(accepted).size !== accepted.length || accepted.some(id => !/^(?:ALL|NONE|S[1-9]\d*)$/.test(id)) || (accepted.length > 1 && accepted.some(id => ["ALL", "NONE"].includes(id)))) fail("invalid_selection");
  const lines = splitLines(snapshot.text);
  const mdx = extname(output) === ".mdx";
  const entries = new Map<number, Parsed[]>();
  const ids = new Set<string>();
  const clean = lines.map((line, index) => {
    const comments: Parsed[] = [];
    const pattern = mdx ? / \{\/\* proofread:([^\r\n]*?) \*\/\}/g : / <!-- proofread:([^\r\n]*?) -->/g;
    let body = line.body.replace(pattern, (_full, encoded: string) => {
      let value: unknown;
      try { value = JSON.parse(decodeURIComponent(encoded)); } catch { fail("malformed_marker"); }
      if (!value || typeof value !== "object" || !("id" in value) || typeof value.id !== "string" || !/^S[1-9]\d*$/.test(value.id) || ids.has(value.id)) fail("malformed_marker");
      ids.add(value.id);
      const payload = value as Marker;
      if (payload.version !== undefined && payload.version !== 1) fail("malformed_marker");
      if (payload.version === 1 && (typeof payload.line !== "string" || /[\r\n]/.test(payload.line) || typeof payload.text !== "string" || !payload.text.trim() || typeof payload.from !== "string" || !payload.from || /[\r\n]/.test(payload.from) || !(payload.to === null || typeof payload.to === "string" && !/[\r\n]/.test(payload.to)))) fail("malformed_marker");
      comments.push({ id: payload.id, payload: payload.version === 1 ? payload : null });
      return "";
    });
    // Legacy comments can be explicitly discarded, never accepted without an anchor.
    if (!mdx) body = body.replace(/ <!-- \[(S[1-9]\d*)\] REVIEW: [^\r\n]*? -->/g, (_full, id: string) => {
      if (ids.has(id)) fail("malformed_marker");
      ids.add(id); comments.push({ id, payload: null }); return "";
    });
    if (/proofread:|<!--\s*\[S\d+\]\s*REVIEW:/.test(body)) fail("malformed_marker");
    if (comments.length) entries.set(index + 1, comments);
    return { body, eol: line.eol };
  });
  if (accepted.some(id => !["ALL", "NONE"].includes(id) && !ids.has(id))) fail("unknown_selection");
  const blocked = protectedLines(clean);
  const edits: Edit[] = [];
  const applied: string[] = [];
  const removed: string[] = [];
  for (const [line, comments] of entries) {
    if (blocked.has(line)) fail("protected_marker");
    for (const { id, payload } of comments) {
      if (payload && clean[line - 1].body !== payload.line) fail("stale_marker");
      if (accepted.includes("ALL") || accepted.includes(id)) {
        if (!payload) fail("legacy_marker_not_applicable");
        if (payload.to === null) fail("suggestion_not_applicable");
        edits.push({ line, from: payload.from, to: payload.to }); applied.push(id);
      } else removed.push(id);
    }
  }
  publish(snapshot, output, joinLines(applyEdits(clean, edits)));
  return { status: "success", file: basename(snapshot.path), finalFile: basename(output), sourceSha256: snapshot.hash, applied, removed };
}
function main(): void {
  const args = process.argv.slice(2);
  if (args.length === 1 && ["--help", "-h"].includes(args[0])) {
    console.log("Usage: apply-suggestions <file.proofread.md|file.proofread.mdx> <S1 S2 ... | all | none>"); return;
  }
  try {
    if (args.length < 2 || args[0].startsWith("-")) fail("invalid_arguments");
    console.log(JSON.stringify(applySuggestions(args[0], args.slice(1)), null, 2));
  } catch (error) { reportError(error); }
}
if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) main();
