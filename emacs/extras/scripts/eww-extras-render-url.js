#!/usr/bin/env node

const fs = require("fs");
const { spawn } = require("child_process");

function parseArgs(argv) {
  const args = {};
  for (let i = 0; i < argv.length; i += 2) {
    const key = argv[i];
    const value = argv[i + 1];
    if (!key || !key.startsWith("--") || value === undefined) {
      throw new Error(`Invalid argument sequence near ${key || "<end>"}`);
    }
    args[key.slice(2)] = value;
  }
  return args;
}

function requireArg(args, name) {
  if (!args[name]) {
    throw new Error(`Missing --${name}`);
  }
  return args[name];
}

function wait(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

class CdpClient {
  constructor(socket) {
    this.nextId = 1;
    this.pending = new Map();
    this.waiters = new Map();
    socket.addEventListener("message", (event) => this.handleMessage(event));
    this.socket = socket;
  }

  static connect(url) {
    return new Promise((resolve, reject) => {
      const socket = new WebSocket(url);
      socket.addEventListener("open", () => resolve(new CdpClient(socket)));
      socket.addEventListener("error", reject);
    });
  }

  handleMessage(event) {
    const message = JSON.parse(event.data);
    if (message.id && this.pending.has(message.id)) {
      const { resolve, reject } = this.pending.get(message.id);
      this.pending.delete(message.id);
      if (message.error) {
        reject(new Error(message.error.message));
      } else {
        resolve(message.result || {});
      }
      return;
    }
    if (message.method) {
      const key = this.eventKey(message.method, message.sessionId);
      const waiters = this.waiters.get(key) || [];
      waiters.splice(0).forEach((resolve) => resolve(message.params || {}));
      this.waiters.delete(key);
    }
  }

  send(method, params = {}, sessionId = null) {
    const id = this.nextId++;
    const message = { id, method, params };
    if (sessionId) {
      message.sessionId = sessionId;
    }
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.socket.send(JSON.stringify(message));
    });
  }

  waitFor(method, sessionId, timeoutMs) {
    const key = this.eventKey(method, sessionId);
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error(`Timed out waiting for ${method}`));
      }, timeoutMs);
      const wrappedResolve = (params) => {
        clearTimeout(timeout);
        resolve(params);
      };
      const waiters = this.waiters.get(key) || [];
      waiters.push(wrappedResolve);
      this.waiters.set(key, waiters);
    });
  }

  eventKey(method, sessionId) {
    return `${sessionId || ""}:${method}`;
  }
}

function launchChrome(args) {
  const chromeArgs = [
    "--headless=new",
    `--user-data-dir=${args["user-data-dir"]}`,
    `--profile-directory=${args["profile-directory"]}`,
    "--disable-gpu",
    "--disable-extensions",
    "--disable-software-rasterizer",
    "--no-first-run",
    "--no-default-browser-check",
    "--hide-scrollbars",
    "--remote-debugging-port=0",
    "about:blank",
  ];
  const chrome = spawn(args["chrome-program"], chromeArgs, {
    stdio: ["ignore", "ignore", "pipe"],
  });
  const endpoint = new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error("Timed out waiting for Chrome DevTools endpoint"));
    }, 10000);
    chrome.stderr.on("data", (chunk) => {
      const match = chunk.toString().match(/DevTools listening on (ws:\/\/\S+)/);
      if (match) {
        clearTimeout(timeout);
        resolve(match[1]);
      }
    });
  });
  return { chrome, endpoint };
}

async function openPage(client, url) {
  const { targetId } = await client.send("Target.createTarget", { url: "about:blank" });
  const { sessionId } = await client.send("Target.attachToTarget", {
    targetId,
    flatten: true,
  });
  await client.send("Page.enable", {}, sessionId);
  await client.send("Runtime.enable", {}, sessionId);
  const loaded = client.waitFor("Page.loadEventFired", sessionId, 15000).catch(() => null);
  await client.send("Page.navigate", { url }, sessionId);
  await loaded;
  await wait(2500);
  return sessionId;
}

async function cleanupPage(client, sessionId) {
  await client.send("Runtime.evaluate", {
    expression: `(${cleanupExpression.toString()})()`,
    awaitPromise: true,
    returnByValue: true,
  }, sessionId);
}

async function cleanupExpression() {
  const pause = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
  document.querySelectorAll([
    '[aria-label="close"]',
    '[aria-label="Close"]',
    'button[title="Close"]',
    'button[title="close"]',
  ].join(",")).forEach((button) => {
    try {
      button.click();
    } catch (_) {}
  });
  await pause(500);
  const selectors = [
    '[role="dialog"]',
    '[aria-modal="true"]',
    '[class*="subscribeDialog"]',
    '[class*="SubscribeDialog"]',
    '[class*="modal"]',
    '[class*="Modal"]',
    '[class*="overlay"]',
    '[class*="Overlay"]',
    '[class*="backdrop"]',
    '[class*="Backdrop"]',
    '[id*="cookie"]',
    '[class*="cookie"]',
  ];
  document.querySelectorAll(selectors.join(",")).forEach((node) => node.remove());
  const viewportArea = window.innerWidth * window.innerHeight;
  document.querySelectorAll("body *").forEach((node) => {
    const style = window.getComputedStyle(node);
    if (!["fixed", "sticky"].includes(style.position)) {
      return;
    }
    const rect = node.getBoundingClientRect();
    const area = Math.max(0, rect.width) * Math.max(0, rect.height);
    const zIndex = Number.parseInt(style.zIndex, 10);
    if (area > viewportArea * 0.15 && (Number.isNaN(zIndex) || zIndex >= 10)) {
      node.remove();
    }
  });
  document.documentElement.style.overflow = "auto";
  document.body.style.overflow = "auto";
}

async function writePdf(client, sessionId, output) {
  const result = await client.send("Page.printToPDF", {
    printBackground: true,
    displayHeaderFooter: false,
    preferCSSPageSize: false,
  }, sessionId);
  fs.writeFileSync(output, Buffer.from(result.data, "base64"));
}

async function writeHtml(client, sessionId, output) {
  const result = await client.send("Runtime.evaluate", {
    expression: "'<!doctype html>\\n' + document.documentElement.outerHTML",
    returnByValue: true,
  }, sessionId);
  fs.writeFileSync(output, result.result.value);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const type = requireArg(args, "type");
  const output = requireArg(args, "output");
  requireArg(args, "url");
  requireArg(args, "chrome-program");
  requireArg(args, "user-data-dir");
  requireArg(args, "profile-directory");
  if (!["pdf", "html"].includes(type)) {
    throw new Error(`Invalid --type ${type}`);
  }
  if (typeof WebSocket === "undefined") {
    throw new Error("This script requires a Node.js runtime with WebSocket support");
  }
  const { chrome, endpoint } = launchChrome(args);
  try {
    const client = await CdpClient.connect(await endpoint);
    const sessionId = await openPage(client, args.url);
    await cleanupPage(client, sessionId);
    if (type === "pdf") {
      await writePdf(client, sessionId, output);
    } else {
      await writeHtml(client, sessionId, output);
    }
    await client.send("Browser.close").catch(() => null);
  } finally {
    setTimeout(() => chrome.kill(), 1000);
  }
}

main().catch((error) => {
  console.error(error.stack || error.message);
  process.exit(1);
});
