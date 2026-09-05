'use strict';

// Pure rule fixtures: no helper imports, browser databases, credentials or setup.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');

const root = path.resolve(__dirname, '..');
const rulesFile = path.join(root, 'codex/skills/chrome-permission-audit/rules.json');
const source = fs.readFileSync(rulesFile, 'utf8');
const rules = JSON.parse(source).rules;
const fixtures = [
  ['docs.bank.example', 0],
  ['support.payroll.example', 0],
  ['not-annas-archive.evil', 0],
  ['google.com.evil', 0],
  ['bing.gov.evil', 0],
  ['docs.google.com', 2],
  ['aistudio.google.com', 1],
  ['chatgpt.com', 2],
  ['claude.ai', 2],
  ['gemini.google.com', 2],
  ['x.com', 2],
  ['writer.substack.com', 2],
  ['forms.gle', 2],
  ['google.com.ar', 3],
  ['google.com', 3],
  ['bing.com', 3],
  ['duckduckgo.com', 3],
  ['web.archive.org', 3],
  ['admin.example.com', 1],
  ['platform.openai.com', 1],
  ['openai.com', 3],
];

for (const [host, expected] of fixtures) {
  test('classifies ' + host + ' without permissive suffix/prefix guesses', () => {
    const match = rules.find(rule => new RegExp(rule.pattern).test(host));
    assert.equal(match ? match.tier : 0, expected);
  });
}

test('paired rules are identical', () => {
  assert.equal(fs.readFileSync(path.join(root,
    'claude/skills/chrome-permission-audit/rules.json'), 'utf8'), source);
});
