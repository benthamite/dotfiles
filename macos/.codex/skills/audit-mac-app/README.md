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

Electron source extraction uses the exact `@electron/asar` version and integrity
hashes committed in `package-lock.json`. On first use, the helper selects an
installed Node 22.12 or newer and runs `npm ci --ignore-scripts`; later audits
invoke the locked local extractor without resolving a package through `npx`.

## About

Adapted from [Peter Hartree](https://x.com/peterhartree)'s [HartreeWorks/skills](https://github.com/HartreeWorks/skills) repository.
