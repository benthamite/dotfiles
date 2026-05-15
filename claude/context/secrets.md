# Secrets setup

- When reading secrets from `pass` or `.zshenv-secrets`, never echo or print them in the terminal or logs.
- When using `pass`, always use full paths (e.g. `env/home-assistant-token`, not `home-assistant-token`). Never grep `pass ls` output — it strips directory context. Use `pass find <name>` to search.
- **Personal secrets** (non-Epoch): stored in `pass` (GPG-encrypted). The **Epoch** secrets setup is described in `../../Epoch/CLAUDE.md`.
- **Account-specific MCP secrets**: placement rules live in `context/mcp-servers.md`; treat resolved values as secrets and never print them.
- **Private GitHub file fetch**: `gh api contents` returns `download_url` with an inline `?token=...`, which the Bash secret hook rightly blocks for `curl`. Use the base64 content path instead: `gh api repos/OWNER/REPO/contents/PATH --jq .content | base64 -D > out`. For files >1 MB, use `gh api repos/OWNER/REPO/git/blobs/SHA --jq .content | base64 -D` or `gh api --paginate`.
