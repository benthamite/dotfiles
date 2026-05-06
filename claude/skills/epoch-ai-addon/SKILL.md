---
name: epoch-ai-addon
description: Work on the Epoch AI Google Docs add-on/editorial checker, including Apps Script code changes, metadata generation, link/style checking, Google Docs runtime fixtures, OAuth scope issues, Marketplace SDK configuration, publishing, and regression testing. Use when the user mentions the Epoch AI add-on, editorial checker, Elliot's Docs add-on feedback, Apps Script deployments, Google Workspace Marketplace publishing, add-on metadata tests, or Google Docs add-on authorization problems.
---

# Epoch AI Add-on

Use this skill to avoid rediscovering the fragile Apps Script, Google Docs, and Marketplace workflow.

## Repos

- Project notes: `/Users/pablostafforini/My Drive/Epoch/projects/editorial-checker`
- Add-on repo: `/Users/pablostafforini/My Drive/Epoch/projects/epoch-ai-addon/repo`
- Testing reference: `/Users/pablostafforini/My Drive/Epoch/projects/epoch-ai-addon/repo/docs/testing.md`

Start by reading `CLAUDE.md` in the project notes and `docs/testing.md` in the add-on repo.

## Local Gate

Run these before treating a code change as ready:

```sh
cd "/Users/pablostafforini/My Drive/Epoch/projects/epoch-ai-addon/repo"
npm test
npx @google/clasp status
```

`clasp status` should track only Apps Script source: `appsscript.json`, `.gs`, and `.html`. `.claspignore` intentionally excludes local tests, docs, package files, and fixture sources.

For metadata changes, expect local tests to cover:

- metadata parsing and field writes
- source-tab selection, including nested/versioned tabs
- title normalization and LLM post-processing
- manifest scopes and advanced services

## Runtime Fixtures

Use the saved Google Editor add-on test deployments documented in `docs/testing.md`. They cover:

- no Metadata tab
- empty Metadata tab
- unrelated table on Metadata tab
- filled fields with overwrite off
- Elliot-style nested/versioned tabs

The fixture source lives under `test-fixtures/google-docs/`. To create fresh fixture documents:

```sh
cd "/Users/pablostafforini/My Drive/Epoch/projects/epoch-ai-addon/repo"
python3 scripts/create-runtime-test-docs.py
```

The script creates new Google Docs under `pablo@epoch.ai`. If a bad fixture is created, move it to Drive trash; do not hard-delete it.

## Apps Script Push

Normal development pushes should target the container-bound Docs script, then restore the standalone `.clasp.json`:

```sh
cd "/Users/pablostafforini/My Drive/Epoch/projects/epoch-ai-addon/repo"
cp .clasp.json .clasp.json.standalone
cp .clasp.json.container-bound .clasp.json
npx @google/clasp push
cp .clasp.json.standalone .clasp.json
```

If a command fails between the second and fourth lines, restore `.clasp.json` before continuing.

## Browser Workflow

For browser-heavy work, use Codex Desktop:

```sh
cd "/Users/pablostafforini/My Drive/Epoch/projects/editorial-checker"
codex app .
```

Use the in-app browser for Apps Script, Google Docs, and Marketplace pages when available. For manual Chrome fallback, use Chrome profile `Profile 2` for Epoch/work pages.

Known in-app browser limitation: Google Drive pickers and Google Docs add-on sidebars are cross-origin/inaccessible iframes. Codex can often navigate to the right page and inspect surrounding UI, but may need the user to paste/select fixture docs in the picker or click sidebar buttons.

## Authorization Canary

Pablo's account is not an adequate canary for user-specific OAuth bugs, because it may already have granted scopes. For scope or authorization changes, prefer a clean or stale Google Workspace test user. The repo currently has no confirmed dedicated account such as `addon-test@epoch.ai`; creating one requires Workspace admin/user-management access.

If no test user exists, `ScriptApp.invalidateAuth` in the dev script is a weaker fallback. Label it as weaker because it still runs under Pablo's browser/account history.

## Release Gate

Before publishing:

1. Run `npm test`.
2. Confirm `npx @google/clasp status` excludes local test infra.
3. Push the container-bound development script.
4. Run the saved Editor add-on test deployments relevant to the changed feature.
5. For scope/auth changes, run a clean/stale-user authorization canary.
6. Create an Apps Script version and update the Marketplace deployment.
7. Update the Marketplace SDK App Configuration script version.
8. Publish from the Marketplace SDK Store Listing tab.
9. Run one Marketplace-installed production canary.

Consult current Google documentation rather than guessing if Apps Script, OAuth, or Marketplace behavior appears to have changed.
