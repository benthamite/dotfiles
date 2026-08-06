#!/usr/bin/env node

import { existsSync, realpathSync } from "fs";
import { basename, dirname, isAbsolute, join, relative, resolve, sep } from "path";

function canonicalizeAllowMissing(input) {
  let current = resolve(input);
  const missing = [];

  while (!existsSync(current)) {
    const parent = dirname(current);
    if (parent === current) {
      throw new Error(`Cannot resolve proofread runtime path: ${input}`);
    }
    missing.unshift(basename(current));
    current = parent;
  }

  return join(realpathSync(current), ...missing);
}

const home = process.env.HOME;
if (!home) {
  throw new Error("HOME must be set to resolve the proofread runtime");
}

const configured = process.env.PROOFREAD_RUNTIME_DIR
  || (process.env.XDG_DATA_HOME
    ? join(process.env.XDG_DATA_HOME, "proofread")
    : join(home, ".local", "share", "proofread"));
const runtimeDir = canonicalizeAllowMissing(configured);
const driveRoot = canonicalizeAllowMissing(join(home, "My Drive"));
const fromDrive = relative(driveRoot, runtimeDir);
const insideDrive = fromDrive === ""
  || (!fromDrive.startsWith(`..${sep}`) && fromDrive !== ".." && !isAbsolute(fromDrive));

if (insideDrive) {
  throw new Error(
    `Refusing proofread runtime inside Google Drive: ${runtimeDir}`,
  );
}

process.stdout.write(`${runtimeDir}\n`);
