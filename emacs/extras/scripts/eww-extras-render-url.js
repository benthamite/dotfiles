#!/usr/bin/env node

"use strict";

const fs = require("node:fs/promises");
const os = require("node:os");
const path = require("node:path");
const { createRequire } = require("node:module");

function parseArgs(argv) {
  const args = {};
  for (let index = 0; index < argv.length; index += 2) {
    const key = argv[index];
    const value = argv[index + 1];
    if (!key?.startsWith("--") || value === undefined) {
      throw new Error(`Invalid argument sequence near ${key || "<end>"}`);
    }
    args[key.slice(2)] = value;
  }
  return args;
}

function validateArgs(args) {
  const allowed = new Set([
    "url",
    "output",
    "type",
    "chrome-program",
    "module-root",
  ]);
  for (const name of Object.keys(args)) {
    if (!allowed.has(name)) throw new Error(`Unknown option --${name}`);
  }
  for (const name of allowed) {
    if (!args[name]) throw new Error(`Missing --${name}`);
  }
  if (!new Set(["pdf", "html"]).has(args.type)) {
    throw new Error(`Invalid --type ${args.type}`);
  }
  return args;
}

function loadPlaywright(moduleRoot) {
  const cacheRequire = createRequire(path.join(moduleRoot, "package.json"));
  return cacheRequire("playwright-core");
}

async function loadAutoconsent(moduleRoot) {
  const cacheRequire = createRequire(path.join(moduleRoot, "package.json"));
  const entry = cacheRequire.resolve("@duckduckgo/autoconsent");
  const content = await fs.readFile(
    path.join(path.dirname(entry), "autoconsent.playwright.js"),
    "utf8",
  );
  const rules = cacheRequire("@duckduckgo/autoconsent/rules/rules.json");
  return { content, rules };
}

const consentConfig = {
  enabled: true,
  autoAction: "optOut",
  disabledCmps: [],
  enablePrehide: true,
  enableCosmeticRules: true,
  enableGeneratedRules: true,
  enableHeuristicDetection: true,
  enablePopupMutationObserver: true,
  detectRetries: 20,
  isMainWorld: false,
  prehideTimeout: 2000,
  visualTest: false,
  logs: {
    lifecycle: false,
    rulesteps: false,
    detectionsteps: false,
    evals: false,
    errors: true,
    messages: false,
    waits: false,
  },
  performanceLoggingEnabled: false,
  heuristicPopupSearchTimeout: 100,
  heuristicMode: "tier2",
};

function wait(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

async function installAutoconsent(context, moduleRoot) {
  const { content, rules } = await loadAutoconsent(moduleRoot);
  const state = { detected: false, done: false, cmpNames: new Set() };
  await context.exposeBinding(
    "autoconsentSendMessage",
    async ({ frame }, message) => {
      if (!message || frame.isDetached()) return;
      if (message.type === "init") {
        await frame.evaluate(
          ({ config, ruleBundle }) => globalThis.autoconsentReceiveMessage?.({
            type: "initResp",
            config,
            rules: ruleBundle,
          }),
          { config: consentConfig, ruleBundle: rules },
        );
      } else if (message.type === "eval") {
        const result = await frame.evaluate(message.code);
        await frame.evaluate(
          ({ id, value }) => globalThis.autoconsentReceiveMessage?.({
            type: "evalResp",
            id,
            result: value,
          }),
          { id: message.id, value: result },
        );
      }
      if (new Set(["cmpDetected", "popupFound"]).has(message.type)) {
        state.detected = true;
        if (message.cmp) state.cmpNames.add(message.cmp);
      }
      if (message.type === "autoconsentDone") {
        state.done = true;
        if (message.cmp) state.cmpNames.add(message.cmp);
      }
    },
  );
  await context.addInitScript({ content });
  return state;
}

function blockingConsentExpression() {
  const consentWords = /\b(cookie|consent|privacy|tracking|preferences)\b/i;
  const viewportArea = Math.max(1, innerWidth * innerHeight);
  for (const node of document.querySelectorAll("body *")) {
    const style = getComputedStyle(node);
    if (style.display === "none" || style.visibility === "hidden" || Number(style.opacity) === 0) continue;
    const rect = node.getBoundingClientRect();
    if (rect.width <= 0 || rect.height <= 0) continue;
    const text = `${node.id} ${node.className} ${node.getAttribute("aria-label") || ""} ${node.textContent || ""}`;
    if (!consentWords.test(text)) continue;
    const modal = node.getAttribute("role") === "dialog" || node.getAttribute("aria-modal") === "true";
    const positioned = new Set(["fixed", "sticky"]).has(style.position);
    const large = rect.width * rect.height >= viewportArea * 0.15;
    if (modal || (positioned && large)) return true;
  }
  return false;
}

async function findBlockingConsentUi(page) {
  for (const frame of page.frames()) {
    try {
      if (await frame.evaluate(blockingConsentExpression)) return true;
    } catch (_) {
      // A frame can detach while the page settles.
    }
  }
  return false;
}

function cleanupResidualUiExpression() {
  document.querySelector("style#autoconsent-prehide")?.remove();
  const consentWords = /\b(cookie|consent|privacy|tracking|preferences)\b/i;
  const viewportArea = Math.max(1, innerWidth * innerHeight);
  for (const node of document.querySelectorAll("body *")) {
    const text = `${node.id} ${node.className} ${node.getAttribute("aria-label") || ""} ${node.textContent || ""}`;
    if (consentWords.test(text)) continue;
    const style = getComputedStyle(node);
    const rect = node.getBoundingClientRect();
    const modal = node.getAttribute("role") === "dialog" || node.getAttribute("aria-modal") === "true";
    const genericOverlay = /\b(modal|overlay|backdrop|subscribe|newsletter)\b/i.test(text);
    const largePositioned = new Set(["fixed", "sticky"]).has(style.position) &&
      rect.width * rect.height >= viewportArea * 0.15;
    if (modal || genericOverlay || largePositioned) node.remove();
  }
  document.documentElement.style.overflow = "auto";
  if (document.body) document.body.style.overflow = "auto";
}

async function cleanupResidualUi(page) {
  for (const frame of page.frames()) {
    try {
      await frame.evaluate(cleanupResidualUiExpression);
    } catch (_) {
      // A frame can detach while the page settles.
    }
  }
}

async function waitForConsent(page, state) {
  const observationDeadline = Date.now() + 500;
  while (Date.now() < observationDeadline && !state.detected && !state.done) {
    await wait(25);
  }
  if (state.detected && !state.done) {
    const consentDeadline = Date.now() + 5000;
    while (Date.now() < consentDeadline && !state.done) await wait(25);
  }
  await wait(250);
  if (await findBlockingConsentUi(page)) {
    throw new Error("verification: unresolved consent blocker");
  }
  await cleanupResidualUi(page);
  if (await findBlockingConsentUi(page)) {
    throw new Error("verification: unresolved consent blocker after cleanup");
  }
}

async function serialize(page, type, output) {
  if (type === "pdf") {
    await page.pdf({ path: output, printBackground: true });
  } else {
    await fs.writeFile(output, `<!doctype html>\n${await page.content()}`, {
      mode: 0o600,
    });
  }
}

async function render(rawArgs, options = {}) {
  const args = validateArgs(rawArgs);
  const deadlineMs = options.deadlineMs || 27000;
  const deadlineAt = Date.now() + deadlineMs;
  const temporaryRoot = options.temporaryRoot || os.tmpdir();
  const { chromium } = loadPlaywright(args["module-root"]);
  const profile = await fs.mkdtemp(path.join(temporaryRoot, "eww-extras-renderer-"));
  await fs.chmod(profile, 0o700);
  const outputDirectory = path.dirname(args.output);
  const temporaryOutput = path.join(
    outputDirectory,
    `.${path.basename(args.output)}.${process.pid}.${Date.now()}.tmp`,
  );
  let context;
  try {
    context = await chromium.launchPersistentContext(profile, {
      executablePath: args["chrome-program"],
      headless: true,
      timeout: Math.min(8000, deadlineMs),
      args: ["--disable-extensions", "--no-first-run", "--no-default-browser-check"],
    });
    const remaining = deadlineAt - Date.now();
    if (remaining <= 0) throw new Error("timeout: internal render deadline");
    let deadlineTimer;
    const deadline = new Promise((_, reject) => {
      deadlineTimer = setTimeout(
        () => reject(new Error("timeout: internal render deadline")),
        remaining,
      );
    });
    try {
      await Promise.race([(async () => {
        const consentState = await installAutoconsent(context, args["module-root"]);
        const pages = context.pages();
        const page = pages[0] || await context.newPage();
        await page.goto(args.url, { waitUntil: "commit", timeout: 15000 });
        await page.waitForLoadState("domcontentloaded", { timeout: 15000 });
        await page.waitForFunction(() => document.body?.textContent?.trim(), null, {
          timeout: 5000,
        });
        await waitForConsent(page, consentState);
        await serialize(page, args.type, temporaryOutput);
        const metadata = await fs.stat(temporaryOutput);
        if (metadata.size === 0) throw new Error("serialization: empty output");
        await fs.rename(temporaryOutput, args.output);
      })(), deadline]);
    } finally {
      clearTimeout(deadlineTimer);
    }
  } finally {
    if (context) await context.close().catch(() => {});
    await fs.rm(temporaryOutput, { force: true });
    await fs.rm(profile, { recursive: true, force: true });
  }
}

async function main() {
  await render(parseArgs(process.argv.slice(2)));
}

if (require.main === module) {
  main().catch((error) => {
    console.error(error.message);
    process.exitCode = 1;
  });
}

module.exports = {
  blockingConsentExpression,
  cleanupResidualUiExpression,
  findBlockingConsentUi,
  parseArgs,
  render,
  validateArgs,
};
