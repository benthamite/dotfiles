# Secrets setup

- When reading secrets from `pass` or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- When using `pass`, always use full paths (e.g. `env/home-assistant-token`, not `home-assistant-token`). Never grep `pass ls` output — it strips directory context. Use `pass find <name>` to search.
- **Tlon organization secrets**: if a Tlon-related secret already has a `pass` entry under `tlon/`, keep that entry as the single source of truth. Do not duplicate it under `env/`.
- **Personal secrets** (non-Epoch): stored in `pass` (GPG-encrypted), normally under `env/` when the consumer conceptually needs an environment variable. **Epoch-related secrets live in 1Password**, usually in the `Automations` vault behind `op://` references; do not create duplicate Epoch entries in `pass`.
- **Classifying duplicates**: inspect the relevant `pass` entry structure directly. Entries may store the real secret in named fields such as `key`, `gptel`, or service-specific labels rather than on the first line. Do not infer identity from the path or by comparing an environment variable to the whole `pass show` output.
- **Account-specific MCP secrets**: placement rules live in `context/mcp-servers.md`; treat resolved values as secrets and never print them.
- **Private GitHub file fetch**: `gh api contents` returns `download_url` with an inline `?token=...`, which the Bash secret hook rightly blocks for `curl`. Use the base64 content path instead: `gh api repos/OWNER/REPO/contents/PATH --jq .content | base64 -D > out`. For files >1 MB, use `gh api repos/OWNER/REPO/git/blobs/SHA --jq .content | base64 -D` or `gh api --paginate`.
