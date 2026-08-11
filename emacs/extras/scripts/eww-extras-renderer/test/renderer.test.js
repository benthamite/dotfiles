"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const os = require("node:os");
const path = require("node:path");
const test = require("node:test");

const { startFixtureServer } = require("./fixture-server.js");

const rendererPath = path.resolve(__dirname, "../../eww-extras-render-url.js");
const renderer = require(rendererPath);

test("parseArgs accepts only paired production options", () => {
  assert.deepEqual(
    renderer.parseArgs([
      "--url", "https://example.com/",
      "--output", "/tmp/output.pdf",
      "--type", "pdf",
      "--chrome-program", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "--module-root", "/tmp/modules",
    ]),
    {
      url: "https://example.com/",
      output: "/tmp/output.pdf",
      type: "pdf",
      "chrome-program": "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "module-root": "/tmp/modules",
    },
  );
});

test("validateArgs rejects copied-profile inputs", () => {
  assert.throws(
    () => renderer.validateArgs({
      url: "https://example.com/",
      output: "/tmp/output.pdf",
      type: "pdf",
      "chrome-program": "/usr/bin/chrome",
      "module-root": "/tmp/modules",
      "user-data-dir": "/tmp/copied-profile",
    }),
    /Unknown option --user-data-dir/,
  );
});

test("validateArgs rejects unsupported output types", () => {
  assert.throws(
    () => renderer.validateArgs({
      url: "https://example.com/",
      output: "/tmp/output.txt",
      type: "text",
      "chrome-program": "/usr/bin/chrome",
      "module-root": "/tmp/modules",
    }),
    /Invalid --type text/,
  );
});

test("real headless Chrome renders a plain page in a fresh context", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "plain.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await renderer.render({
    url: `${server.origin}/plain`,
    output,
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  });

  const html = await fs.readFile(output, "utf8");
  assert.match(html, /Plain fixture content/);
});

test("Autoconsent opts out before HTML serialization", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "consent.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await renderer.render({
    url: `${server.origin}/cookieconsent2`,
    output,
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  });

  const html = await fs.readFile(output, "utf8");
  assert.match(html, /data-consent="opted-out"/);
  assert.doesNotMatch(html, /id="cc--main"/);
});

test("unknown consent blocker fails without creating output", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "unknown.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await assert.rejects(
    renderer.render({
      url: `${server.origin}/unknown`,
      output,
      type: "html",
      "chrome-program": chromeProgram,
      "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
    }),
    /verification: unresolved consent blocker/,
  );
  await assert.rejects(fs.access(output));
});

test("a failed render preserves an existing destination", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "existing.html");
  await fs.writeFile(output, "existing content");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await assert.rejects(renderer.render({
    url: `${server.origin}/unknown`,
    output,
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  }));
  assert.equal(await fs.readFile(output, "utf8"), "existing content");
});

test("a later render cannot see a prior run's consent cookie", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });
  const common = {
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  };

  await renderer.render({
    ...common,
    url: `${server.origin}/cookieconsent2`,
    output: path.join(temporary, "first.html"),
  });
  const second = path.join(temporary, "second.html");
  await renderer.render({
    ...common,
    url: `${server.origin}/cookie-state`,
    output: second,
  });

  assert.match(await fs.readFile(second, "utf8"), />no-cookies</);
});

test("PDF output is nonempty and has the PDF signature", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "plain.pdf");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await renderer.render({
    url: `${server.origin}/plain`,
    output,
    type: "pdf",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  });
  assert.equal((await fs.readFile(output)).subarray(0, 4).toString(), "%PDF");
});

test("generic residual overlays are removed after consent verification", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "residual.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await renderer.render({
    url: `${server.origin}/residual`,
    output,
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  });
  const html = await fs.readFile(output, "utf8");
  assert.match(html, /Residual fixture content/);
  assert.doesNotMatch(html, /newsletter-overlay/);
});

test("internal deadline cleans output and browser profile", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const profiles = path.join(temporary, "profiles");
  await fs.mkdir(profiles);
  const output = path.join(temporary, "timeout.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await assert.rejects(
    renderer.render({
      url: `${server.origin}/never-ready`,
      output,
      type: "html",
      "chrome-program": chromeProgram,
      "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
    }, { deadlineMs: 2000, temporaryRoot: profiles }),
    /timeout: internal render deadline/,
  );
  assert.deepEqual(await fs.readdir(profiles), []);
  await assert.rejects(fs.access(output));
});

test("bot-verification pages fail instead of becoming PDFs", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "challenge.pdf");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await assert.rejects(renderer.render({
    url: `${server.origin}/challenge`,
    output,
    type: "pdf",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  }), /verification: browser challenge page/);
  await assert.rejects(fs.access(output));
});

test("rendering waits for dynamically populated content to settle", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "dynamic.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await renderer.render({
    url: `${server.origin}/dynamic`,
    output,
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  });
  assert.match(await fs.readFile(output, "utf8"), /Dynamic article content arrived/);
});

test("newsletter text does not remove an ordinary content ancestor", async (t) => {
  const chromeProgram = process.env.EWW_EXTRAS_RENDERER_CHROME_PROGRAM;
  if (!chromeProgram) {
    t.skip("test-browser supplies system Chrome");
    return;
  }
  const server = await startFixtureServer();
  const temporary = await fs.mkdtemp(path.join(os.tmpdir(), "eww-render-test-"));
  const output = path.join(temporary, "newsletter.html");
  t.after(async () => {
    await server.close();
    await fs.rm(temporary, { recursive: true, force: true });
  });

  await renderer.render({
    url: `${server.origin}/newsletter-content`,
    output,
    type: "html",
    "chrome-program": chromeProgram,
    "module-root": process.env.EWW_EXTRAS_RENDERER_MODULE_ROOT,
  });
  assert.match(await fs.readFile(output, "utf8"), /Important article body/);
});
