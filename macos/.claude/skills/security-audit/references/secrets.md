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
  dedicated credential files or authentication/session-cookie stores; record
  metadata-only coverage. Agent conversation transcripts are distinct: scan
  only explicitly permitted inputs through trusted local redaction, never raw
  transcript dumps or an exception to an existing input denial.
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
`SKILL.md`, then use the permitted form
`/usr/bin/python3 ABSOLUTE_HELPER ABSOLUTE_TARGET` after verifying that interpreter
and its startup environment. This explicit Python invocation is not isolated
mode: it does not itself disable startup customizations. If their trust cannot
be established, mark the check unavailable; do not widen the guard's allowance
or switch to a raw reader. Never execute the target.
The helper emits a JSON list with metadata and explicit coverage
diagnostics. Exit 0 means the supported subset was inspected; exit 2 means
coverage is incomplete, including unreadable input. Preserve partial rows and
`not-checked` diagnostics rather than treating nonzero exit as an empty scan.
Use it only for targets the existing guard permits. A denied `.envrc` or other
sensitive path remains not checked; do not substitute a raw content reader.

Do not infer ambient secrets from export counts, comments, or variable names
alone. Identity exports such as email addresses and usernames are not credential
findings. A `credential-*` classification identifies credential-like wiring;
establish whether it is exercised and which processes inherit it.
Rows marked `classification_basis: name-heuristic` use the variable's name,
not credential validation. In particular, `non-secret` means an unrecognized
credential name, not proof of harmless contents. `credential-empty` describes
only a supported assignment's known empty value, not every runtime invocation.
Export rows carry scope and certainty; `not-checked` rows carry a fixed reason
and no variable name. Input reads are bounded to regular files; special files,
oversized input and observed changes remain explicit coverage gaps. The helper
does not follow a final symlink. None of this expands the guard's read policy.
Recognizable credential-shaped names and names over 128 characters are omitted
with `sensitive-or-oversized-identifier`. This limited filter cannot recognize
every secret encoded in a name; keep metadata scoped and review it before sharing.

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

Before investigating a detected credential, resolve the bundled
`scripts/credential-incident-registry.py` and directory target to absolute paths.
Invoke the helper's executable entry point directly; it uses isolated system
Python and trusted system Git for discovery, not project-selected executables.
An unavailable runtime is a coverage gap, not permission to install or substitute
an untrusted interpreter. In Git its default registry is
`GIT_DIR/dotfiles-publish/credential-incidents.json`; otherwise it uses
`$XDG_STATE_HOME/security-audit/credential-incidents.json`, or
`~/.local/state/security-audit/credential-incidents.json`.

Resolve that location with `--start TARGET path` first, then use
`--start TARGET list` only after checking the resolved scope. Check scope again
before `lookup`; a directory audit does not override a restriction excluding
account/global state. If that state is out of scope, report incident-memory
lookup not checked.

Git targets retain per-worktree `GIT_DIR` placement; do not assume a registry is
shared with the main worktree. A failed or invalid target discovery is an error,
not permission to consult global state. Non-Git discovery must be established
before using the documented global location, and `XDG_STATE_HOME` must be
absolute. An explicit `--registry` selects a store but does not widen authority.
Registry and input paths reject artifact symlinks and traversal; documented
macOS `/tmp` and `/var` aliases are normalized. Read limits are 256 KiB for a
record input and 8 MiB for registry state. Existing unsafe permissions are
reported, not silently changed. Missing state differs from failed validation.

The registry directory is private (0700), with a private file (0600). It records
keyed fingerprints, provider status, verification method/time, redacted
references, locations, and next actions. It must not contain credentials.
Metadata validation rejects recognizable secret material; it cannot prove
arbitrary text is secret-free. Keep summaries factual and avoid copying provider
responses.

Use `lookup --fingerprint FINGERPRINT` only with a compatible registry
fingerprint. Do not truncate or reinterpret another scanner's identifier, and
do not generate a new fingerprint by reading a credential during an audit.
Lookup includes finding fingerprints recorded under locations. It emits every
matching record as consecutive JSON objects, not one JSON array; consume all
matches rather than selecting the first or parsing the whole stream as one
document. A miss has exit 1 and empty output; invalid state is an error, not a miss.
Reuse relevant research and completed verification, keeping its date and
limitations visible. A registry record is evidence, not an allowlist: it never
suppresses a scanner finding. Reassess changed exposure and scope; a recorded
next action is not authorization to perform it.

Audit mode does not create/update registry state or refresh live credential
validity. During authorized remediation, perform the applicable recorded next
action and update immediately after a provider action or live verification.
Use `record --input FILE` with a mode-0600 temporary JSON input, then `validate`;
trash the temporary input after recording. For public Git history,
`dotfiles-publish incident-record` remains the only command that resolves an
exact finding after rejection of the old credential has been verified.

`record` replaces a complete entry, not a partial field patch. Reuse an incident
ID only for the same credential fingerprint; a different credential needs a
distinct ID. Updates serialize cooperating helper writers using a private
sidecar lock and atomically replace the validated state. This does not protect
against arbitrary external writers or malicious same-user code. Read-only
commands must not create that lock, directories or records. Do not delete the
lock file to force an update; report a timeout or unsafe state instead.

## Reference maintenance

Checked 2026-09-04. Consult these primary sources when scanner behavior or
encryption semantics affect a finding; use installed-tool help for its version.

- [Git-crypt scope and limitations](https://github.com/AGWA/git-crypt#limitations)
- [GitHub secret-scanning pattern coverage](https://docs.github.com/en/code-security/secret-scanning/introduction/supported-secret-scanning-patterns)
- [Direnv execution model](https://direnv.net/man/direnv.1.html)
