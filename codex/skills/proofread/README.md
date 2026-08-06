# Proofread

A paired Claude Code/Codex skill for proofreading Markdown files with aspell or Gemini Flash.

## Documentation

See [SKILL.md](./SKILL.md) for complete documentation and usage instructions.

## Local setup

```bash
yarn -s setup-runtime
```

Setup and execution use the same precedence: PROOFREAD_RUNTIME_DIR, then XDG_DATA_HOME, then ~/.local/share/proofread. They reject any resolved path inside the Google Drive sync root, including paths that enter it through a symlink. Do not create or symlink a local `node_modules` directory.

The live global skill directories are managed by the dotfiles Claude/Codex sync setup; do not install this tracked copy with `npx skills add`.

## About

Adapted from [Peter Hartree](https://x.com/peterhartree)'s [HartreeWorks/skills](https://github.com/HartreeWorks/skills) repository.
