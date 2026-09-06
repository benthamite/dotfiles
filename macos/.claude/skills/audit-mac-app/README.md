# Audit Mac App

See [SKILL.md](SKILL.md) for the workflow and interpretation limits. The paired
scanner collects bounded static evidence; it does not launch the app, grant
permissions or return a safety verdict.

```bash
# Set this to the directory containing this README, independent of the shell cwd.
SKILL_DIR="/absolute/path/to/audit-mac-app"
"$SKILL_DIR/scripts/audit-mac-app.sh" "/Applications/Selected.app"
```

The JSON report includes check statuses, contextual signals and coverage gaps.
Exit 0 means a report was emitted, not that the app or every check passed.
Invalid/unsupported input or invocation returns 2.

Select additional work explicitly:

- `--assess-gatekeeper`: assess current system policy; this can consult Apple
  services. Acceptance is not automatically a notarization or safety verdict.
- `--extract-asar`: request the isolated Electron extraction pass.
- `--extracted-root PATH`: inspect existing extraction output instead. Verify
  its archive/version provenance separately; it is not automatically bound to
  the selected app.

The extraction helper retains exact `@electron/asar` dependencies and integrity
values in `package-lock.json`. Dependency bootstrap and archive parsing are
separate operations: bootstrap may use the network; parsing runs only through
the pinned, networkless Docker Desktop VM boundary documented in canonical
`bin/README.org`. The reviewed engine/image must already be available. Do not
substitute a mutable image or host parser if it is unavailable.

The scanner uses macOS Python 3; the extractor wrapper uses Homebrew Python 3.
A cold extractor cache additionally needs
a trusted Node 22.12+ installation with sibling npm, plus the trash utility for
owned staging cleanup. `AUDIT_MAC_APP_NODE` selects a trusted Node executable;
`AUDIT_MAC_APP_CACHE_DIR` selects an owned private (0700) off-Drive cache.
Warm-cache reuse checks its manifest/version/layout, not authentication of all
installed module bytes. Treat that cache as trusted local state.

The default supply-chain test suite uses synthetic inputs. Set
`AUDIT_MAC_APP_LIVE_TESTS=1` only for the selected live VM acceptance test with
the reviewed engine/image and matching cache already provisioned; it does not
start Docker, pull images or install dependencies.

Use fresh output and private caches outside Google Drive. Never execute
extracted files or follow their symlinks. Preserve requested evidence before
cleaning only owned temporary artifacts. Consult the helper's documented
errors for unsupported inputs or cache/runtime state instead of bypassing them.
Failed or interrupted extraction retains its directory because an ordinary
failure code cannot establish that container cleanup completed. Resolve that
cleanup state before removing the reported directory or retrying.

Adapted from [HartreeWorks/skills](https://github.com/HartreeWorks/skills).
