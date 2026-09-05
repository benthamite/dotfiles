# Secrets and incident evidence

Apply the main skill's scope and read policy throughout. Inspect references and
metadata; never print source lines or resolve credentials merely to classify
their storage. Use the canonical secrets context for vault choice and broker
commands, including per-process injection. An encrypted store is not "safe by
definition": its permissions, authorization scope, and credential consumers
still matter.

## Checks

- Inventory in-scope shell/config paths, ignored and tracked environment files,
  launchd configuration, and agent memory/transcript locations. Include
  account-specific agent paths discovered by the agents domain. Do not open
  dedicated credential files or session stores; record metadata-only coverage.
- Use an available, trusted scanner with verified redaction for approved files.
  Provider patterns are candidates, not proof of live secrets. Consider current
  AWS temporary and long-term credentials, GitHub token families, Slack tokens,
  private-key headers in ordinary source, and generic credential assignments.
  Recognize placeholders, secret-store references, public keys and public client
  IDs. Never scan the private-key files themselves.
- Check the worktree, index, and relevant Git history as separate surfaces with
  explicit coverage. Scope historical scanning to authorized refs; record
  shallow/history/tool limits. For public dotfiles history, use the
  `publish-dotfiles` skill's scanner and incident semantics, without publishing.
- Inspect `.gitattributes`, encryption-filter status, and committed-blob
  encryption through safe metadata/redacting tools. Git-crypt protects selected
  Git blobs; an unlocked working tree is plaintext. Current encryption does not
  establish that earlier commits were encrypted. Do not flag the existence of
  `.zshenv-secrets` alone, and do not exempt its runtime export behavior.
- Check whether secret-bearing `.env` variants and resolved `.env.op` outputs
  are tracked or can be accidentally added. An ignored path can still be
  tracked. Tracked templates containing placeholders are not leaked secrets.
- Inspect `.envrc` as executable configuration within the read policy; do not
  activate it. Fetching from a store still exports secrets to commands in that
  directory. Establish recipients and inheritance, not just storage format.
- Check shell-history exposure only through a safe redacting scanner. Include
  command arguments and credential-bearing URLs; never print raw history.
- Check file ownership, access modes, accidental sync/publication paths, and
  secret references. Avoid dumping process environments or full process args.
  Do not recommend globally exported credentials as an MCP/config remedy.

## Shell export classifier

Resolve `scripts/classify-shell-exports.py` relative to the directory containing
`SKILL.md`, then run `python3 ABSOLUTE_HELPER ABSOLUTE_TARGET`. Never execute
the target. The helper emits JSON metadata and explicit coverage diagnostics.
Use it only for targets the existing guard permits. A denied `.envrc` or other
sensitive path remains not checked; do not substitute a raw content reader.

Do not infer ambient secrets from export counts, comments, or variable names
alone. Identity exports such as email addresses and usernames are not credential
findings. A `credential-*` classification identifies credential-like wiring;
establish whether it is exercised and which processes inherit it.
`credential-empty` contains no credential value. Export rows carry scope and
certainty; `not-checked` rows carry a fixed reason and no variable name.

Ordinary exports inside a function can affect the caller after the function
runs. Function/conditional results indicate potential exposure until invocation
is established. Explicitly local variables, subshells, and command-prefixed
assignments have narrower scope; distinguish these from global exports.
Unsupported syntax is not checked, never a clean result. Follow only safe,
in-scope source references; do not execute them to settle uncertainty.

Literal and store-backed ambient credentials both increase exposure. Calibrate
severity to reachability, access, and credential scope. Recommend the canonical
broker's per-process injection and minimum required credentials, rather than
substituting a different vault or globally loading secrets from an encrypted
store. Per-process injection does not isolate the consumer from its own secrets.

## Credential incident memory

Before investigating a detected credential, run the bundled
`scripts/credential-incident-registry.py` with `--start TARGET list`, resolving
both helper and directory target to absolute paths. In Git it reads
`GIT_DIR/dotfiles-publish/credential-incidents.json`; otherwise it uses
`$XDG_STATE_HOME/security-audit/credential-incidents.json`, or
`~/.local/state/security-audit/credential-incidents.json`.

Resolve the registry location with the helper's `--start TARGET path` first.
Check it against the authorized scope before `list` or `lookup`; a directory
audit does not override a restriction excluding account/global state. If that
state is out of scope, report incident-memory lookup not checked.

The registry directory is private (0700), with a private file (0600). It records
keyed fingerprints, provider status, verification method/time, redacted
references, locations, and next actions. It must not contain credentials.
Metadata validation rejects recognizable secret material; it cannot prove
arbitrary text is secret-free. Keep summaries factual and avoid copying provider
responses.

Use `lookup --fingerprint FINGERPRINT` only with a compatible registry
fingerprint. Do not truncate or reinterpret another scanner's identifier, and
do not generate a new fingerprint by reading a credential during an audit.
Reuse relevant research and completed verification, keeping its date and
limitations visible. A registry record is evidence,
not an allowlist: it never suppresses a scanner finding. Reassess changed
exposure and scope; a recorded next action is not authorization to perform it.

Audit mode does not create/update registry state or refresh live credential
validity. During authorized remediation, perform the applicable recorded next
action and update immediately after a provider action or live verification.
Use `record --input FILE` with a mode-0600 temporary JSON input, then `validate`;
trash the temporary input after recording. For public Git history,
`dotfiles-publish incident-record` remains the only command that resolves an
exact finding after rejection of the old credential has been verified.

## Reference maintenance

Checked 2026-09-04. Consult these primary sources when scanner behavior or
encryption semantics affect a finding; use installed-tool help for its version.

- [Git-crypt scope and limitations](https://github.com/AGWA/git-crypt#limitations)
- [GitHub secret-scanning pattern coverage](https://docs.github.com/en/code-security/secret-scanning/introduction/supported-secret-scanning-patterns)
- [Direnv execution model](https://direnv.net/man/direnv.1.html)
