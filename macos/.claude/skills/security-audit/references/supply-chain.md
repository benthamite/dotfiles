# Supply chain

Inventory the package manager and version, manifests/locks, actual install
commands, registry origins, and executable entry points before judging policy.
Inspect effective settings locally without dumping credentials from npm/pip
configuration. Do not install tools or resolve/build project dependencies merely
to audit them. A missing scanner is a coverage gap.

## Dependencies and install behavior

- Assess resolved versions and lockfile integrity, including committed state,
  hashes, registry/source URLs, and frozen-install use. Manifest ranges can
  coexist with exact locked versions. Libraries and applications have different
  locking needs. Unsupported/EOL versions and applicable advisories are evidence;
  major-version distance or popularity alone is not.
- For npm, check the effective `ignore-scripts` policy, precedence, and actual
  workflow overrides. An allowlisted build can be deliberate. Script suppression
  does not protect against malicious runtime code.
- For pnpm, check installed-version documentation and `pnpm-workspace.yaml`.
  Versions 11 and later use `allowBuilds`; older versions used
  `onlyBuiltDependencies`/`ignoredBuiltDependencies`. Inspect broad bypasses
  such as `dangerouslyAllowAllBuilds` and executable `.pnpmfile` hooks.
  Do not infer pnpm policy from an npm-only key.
- For Bun, inspect `trustedDependencies`, its version-specific default trusted
  list, and overrides. Missing npm settings do not establish missing controls.
- Python source distributions/build backends may execute code while preparing
  metadata or building. A virtualenv is not a security sandbox. Wheel-only
  installation (`--only-binary :all:`) and pinned artifact hashes reduce
  install-time risk; wheels still execute code when used. Assess build origin,
  index configuration/dependency confusion, and the real isolation boundary.
- Preserve Pablo's npm release-age policy: `min-release-age=3` (days), requiring
  npm 11.10.0 or newer. Verify the actual binary version and recognized effective
  configuration; capture `npm config ls -l` and emit only the checked key.
  `npm config get` alone can echo an unknown key. For pnpm,
  `minimumReleaseAge` uses minutes; for Bun it uses seconds. Check current
  syntax/config location and exemptions. For uv, inspect `--exclude-newer`
  policy and installed-version semantics. Release age reduces some exposure;
  it is not vulnerability detection or a reason to defer an urgent security fix.

## Advisory collection

Verify the scanner's provenance, supported inputs, output redaction, and network
destination before use. A trusted scanner can still run a resolver or send
private package names/source URLs to a remote service.

- `npm audit --json`: use an existing lockfile and trusted registry, without
  `audit fix` or an install. The advisory request sends dependency metadata;
  fallback requests can include the lockfile tree. If its private metadata
  would leave the authorized data boundary, use an approved source or mark the
  remote check not checked. Filter output locally before reporting.
- `pip-audit` (not `pip audit`): prefer its documented `--path` mode against
  existing installed metadata with a trusted interpreter, or complete pinned
  requirements with `--no-deps --disable-pip` where supported. Establish input
  completeness before claiming dependency coverage. Do not run project or
  requirements resolution modes that invoke pip/build backends on untrusted
  inputs. Do not activate the target virtualenv or execute its Python to read
  metadata; startup hooks can execute code.
- `cargo audit`: use an existing Cargo.lock and a trusted advisory database.
  Account for network/cache effects and stale database data.
- Dependabot: use the configured GitHub access route, enumerate **all pages**,
  and explicitly handle open/dismissed/fixed state. For the approved gh route,
  the alerts endpoint needs `--paginate`; a first page is not a full scan.

Report package/version, advisory ID, affected workflow, advisory severity,
local exposure, and fix/mitigation availability separately. Keep relevant
transitive findings visible even without an upgrade path. Do not automatically
downgrade dev-only dependencies: build tools run on the developer's machine.

## Other executable supply chains

Include the following when present in the authorized scope:

- Emacs/Elpaca packages and recipes: origins, pinned revisions/update workflow,
  build forms, and executable local/project configuration. Inspect files; do
  not start Emacs, load packages, tangle, or run builds during an audit.
- Homebrew taps/casks, downloaded binaries, shell/bootstrap installers, and
  project Git hooks: source, integrity/signature evidence where available,
  update support, and execution privileges.
- Transient launchers such as `npx`/`uvx`, MCP/plugin installers, container
  image tags, and remote Git dependencies: selected artifact/revision, update
  policy, and access at execution. A package lock may not cover these paths.
- GitHub Actions: third-party actions pinned to immutable commits, token
  permissions, secrets exposure to untrusted PR code, privileged triggers such
  as `pull_request_target`, artifact/cache trust, and self-hosted runner access.
- Dependency review services such as Socket: report available protection and
  limitations. Lack of a particular vendor is not itself a vulnerability.

## Reference maintenance

Checked 2026-09-04. Recheck manager/version-sensitive behavior before prescribing
a key; do not copy a policy across managers on the strength of its name.

- [npm audit effects](https://docs.npmjs.com/cli/v11/commands/npm-audit/)
- [npm ci and lock behavior](https://docs.npmjs.com/cli/v11/commands/npm-ci/)
- [npm 11.10.0 release-age support](https://github.com/npm/cli/releases/tag/v11.10.0)
- [pnpm build controls](https://pnpm.io/settings/build)
- [Bun lifecycle policy](https://bun.com/docs/pm/lifecycle)
- [pip secure installs](https://pip.pypa.io/en/stable/topics/secure-installs/)
- [pip-audit inputs and security model](https://github.com/pypa/pip-audit#security-model)
- [uv resolution policy](https://docs.astral.sh/uv/concepts/resolution/)
- [Cargo audit](https://rustsec.org/docs/cargo-audit/)
- [Dependabot pagination and state](https://docs.github.com/en/rest/dependabot/alerts)
- [GitHub Actions security](https://docs.github.com/en/actions/security-for-github-actions/security-guides/security-hardening-for-github-actions)
