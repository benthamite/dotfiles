#!/usr/bin/env node

import { lstatSync, readlinkSync, realpathSync } from "fs";
import { dirname, isAbsolute, join, parse, relative, resolve, sep } from "path";

function canonicalizeAllowMissing(input, symlinkDepth = 0) {
  const absolute = resolve(input);
  const { root } = parse(absolute);
  const components = absolute.slice(root.length).split(sep).filter(Boolean);
  let current = realpathSync(root);

  for (let index = 0; index < components.length; index += 1) {
    const candidate = join(current, components[index]);
    let status;
    try {
      status = lstatSync(candidate);
    } catch (error) {
      if (error?.code === "ENOENT") {
        return join(current, ...components.slice(index));
      }
      throw error;
    }

    if (status.isSymbolicLink()) {
      if (symlinkDepth >= 40) {
        throw new Error(`Symlink cycle in proofread runtime path: ${candidate}`);
      }
      const target = resolve(dirname(candidate), readlinkSync(candidate));
      return canonicalizeAllowMissing(
        join(target, ...components.slice(index + 1)),
        symlinkDepth + 1,
      );
    }

    current = realpathSync(candidate);
  }

  return current;
}

function assertOutsideDrive(candidate, driveRoot) {
  const fromDrive = relative(driveRoot, candidate);
  const insideDrive = fromDrive === ""
    || (!fromDrive.startsWith(`..${sep}`) && fromDrive !== ".." && !isAbsolute(fromDrive));

  if (insideDrive) {
    throw new Error(
      `Refusing proofread runtime inside Google Drive: ${candidate}`,
    );
  }
}

function main() {
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

  assertOutsideDrive(runtimeDir, driveRoot);
  const nodeModulesDir = canonicalizeAllowMissing(join(runtimeDir, "node_modules"));
  assertOutsideDrive(nodeModulesDir, driveRoot);

  process.stdout.write(`${nodeModulesDir}\n`);
}

try {
  main();
} catch (error) {
  const message = error instanceof Error ? error.message : String(error);
  console.error(`Error: ${message}`);
  process.exitCode = 1;
}
