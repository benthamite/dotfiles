# Audit Mac App

A local agent skill for auditing macOS applications before running them, with focus on apps requesting sensitive permissions.

## Documentation

See [SKILL.md](./SKILL.md) for complete documentation and usage instructions.

## Local Usage

```bash
# Set this to the directory containing this README.md.
SKILL_DIR="/path/to/audit-mac-app"
"$SKILL_DIR/scripts/audit-mac-app.sh" /Applications/AppName.app
```

Electron source extraction requires Node/npm because the scanner uses `npx --yes asar`.

## About

Adapted from [Peter Hartree](https://x.com/peterhartree)'s [HartreeWorks/skills](https://github.com/HartreeWorks/skills) repository.
