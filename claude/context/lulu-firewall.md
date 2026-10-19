# LuLu outbound firewall and versioned installs

Pablo's Mac runs LuLu (Objective-See), an outbound firewall. Homebrew binaries are only ad-hoc signed, so LuLu's rules for them are tied to their exact file path. When `brew upgrade` installs a new version, the path changes (`/opt/homebrew/Cellar/<formula>/<new-version>/…`) and the new binary has no rule: LuLu blocks it or holds it on an unanswered alert.

**Symptom.** A Homebrew-installed tool's outbound connections hang, then fail with a timeout or "Couldn't connect to server", while other programs reach the same host. First seen 18 Oct 2026: after the personal-updates job upgraded git 2.56.0 to 2.56.0_1, `/opt/homebrew/bin/git` could not reach github.com (`Failed to connect to github.com port 443`), while `curl` and `/usr/bin/git` could.

**Check.** Compare the Homebrew binary with a system one against the same host, e.g. `timeout 40 /opt/homebrew/bin/git ls-remote <url>` versus `timeout 40 /usr/bin/git ls-remote <url>`. Only the Homebrew one failing points to LuLu; confirm the formula's version changed in `/opt/homebrew/Cellar/<formula>/`.

**Second symptom.** A periodic job using the tool raises one LuLu alert per run while the first goes unanswered. On 18 Oct 2026 the 10-minute `org.ph-ps.local-agent-sessions` job left 18 git alerts open overnight.

**Prevention (since 18 Oct 2026).** LuLu supports directory rules: a rule path ending in `*` matches every process whose path starts with the rest, so `/opt/homebrew/Cellar/git/*` covers every git version. `bin/lulu-directory-rules check` lists installed package directories that have a version-specific allow rule but no directory rule; `bin/lulu-directory-rules add` creates the missing ones through LuLu's Add Rule window. Adding allow rules is blocked for agents by the auto-mode classifier, so Pablo runs `add` himself. `add` opens LuLu's Settings window itself, but LuLu shows it only when no alert window is visible, so `add` refuses while any alert is open. `bin/personal-updates` runs the check before every upgrade lane, holds back upgrades of uncovered packages, and the daily delayed lane alerts through `launchd-heartbeat` (monitor `personal-updates`, see `alerting.md`) until `add` is run.

**Claude Code (19 Oct 2026).** The same happens to Claude Code's native installer, which auto-updates several times a day into `~/.local/share/claude/versions/<version>`. The binary is Developer ID signed, yet LuLu 4.5.1 stores its rules with no signing info, keyed on the versioned path (why its signature check fails is unknown; LuLu's logs were not readable). Overnight sessions then left one `api.anthropic.com` alert per process (eight on 19 Oct, for 2.1.303). `lulu-directory-rules` covers that directory too; `personal-updates` ignores it, since Homebrew cannot strand it.

**Other versioned layouts.** On 19 Oct the rules also held versioned paths under `~/.pyenv/versions`, `~/.nvm/versions/node` and the Playwright cache, each with one or a few rules. They change rarely and are not covered; add a pattern to `VERSIONED_PATHS` if their alerts recur.

**Remaining gap.** A package upgraded only as another package's dependency, or upgraded by hand with `brew upgrade`, is not held. If its alerts appear, answer one with Allow, then run `lulu-directory-rules add`.
