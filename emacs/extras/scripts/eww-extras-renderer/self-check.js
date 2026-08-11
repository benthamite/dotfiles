#!/usr/bin/env node

"use strict";

const fs = require("node:fs");
const path = require("node:path");

const moduleRoot = process.argv[2];
if (!moduleRoot) {
  throw new Error("Missing module root");
}

const expected = new Map([
  ["@duckduckgo/autoconsent", "16.20.1"],
  ["playwright-core", "1.62.1"],
]);
for (const [name, version] of expected) {
  const metadataPath = path.join(
    moduleRoot,
    "node_modules",
    ...name.split("/"),
    "package.json",
  );
  const metadata = JSON.parse(fs.readFileSync(metadataPath, "utf8"));
  if (metadata.version !== version) {
    throw new Error(`${name}: expected ${version}, got ${metadata.version}`);
  }
}
