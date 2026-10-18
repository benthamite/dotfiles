# LuLu outbound firewall and Homebrew upgrades

Pablo's Mac runs LuLu (Objective-See), an outbound firewall. Homebrew binaries are only ad-hoc signed, so LuLu's rules for them are tied to their exact file path. When `brew upgrade` installs a new version, the path changes (`/opt/homebrew/Cellar/<formula>/<new-version>/…`) and the new binary has no rule: LuLu blocks it or holds it on an unanswered alert.

**Symptom.** A Homebrew-installed tool's outbound connections hang, then fail with a timeout or "Couldn't connect to server", while other programs reach the same host. First seen 18 Oct 2026: after the personal-updates job upgraded git 2.56.0 to 2.56.0_1, `/opt/homebrew/bin/git` could not reach github.com (`Failed to connect to github.com port 443`), while `curl` and `/usr/bin/git` could.

**Check.** Compare the Homebrew binary with a system one against the same host, e.g. `timeout 40 /opt/homebrew/bin/git ls-remote <url>` versus `timeout 40 /usr/bin/git ls-remote <url>`. Only the Homebrew one failing points to LuLu; confirm the formula's version changed in `/opt/homebrew/Cellar/<formula>/`.

**Fix (needs Pablo; LuLu's rules are root-protected).** In the LuLu menu-bar icon, answer the pending alert with Allow, or add an Allow rule for the new path (for git: `…/libexec/git-core/git-remote-https`). Meanwhile `/usr/bin/git` works for pushes.

**Open (TODO).** Nothing detects this automatically. A post-upgrade connectivity check in `bin/personal-updates` that alerts through `launchd-heartbeat` (see `alerting.md`) would.
