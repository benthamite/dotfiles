# Diagnostic runner contract

Use the helper from this skill's resolved directory, not a guessed installation
path. It is a batch command, not a library to load into a live Emacs session.

For an already identified self-contained target:

```bash
LINT_SKILL_DIR="/absolute/path/to/this/skill"
LINT_TARGET="/absolute/path/to/target.el"
emacs -Q --batch -l "$LINT_SKILL_DIR/scripts/lint-file.el" -- "$LINT_TARGET"
```

When dependencies are required, add verified `-L ABSOLUTE_DIRECTORY` arguments
before `-l`. Keep canonical source directories ahead of dependencies and exclude
the package's own build. Inspect the actual load-path order; do not assume the
order of repeated `-L` arguments is the final search order. The helper prefers
source over stale bytecode. Missing dependencies are failed checks, not a reason
to install packages or load a different profile.

## Result and interpretation

The final JSON record uses schema version 1 and identifies the requested and
resolved target, Emacs version, requested compiler warning policy, pre/post
SHA256 and whether its content stayed unchanged. The policy is `all`; this does
not prove that source-level suppression forms or every dependency were audited.
The `compiler` and `checkdoc` objects each report `completed`, `status`,
`diagnostics` and `errors`. Retain stderr too: a crash or other premature exit
may prevent a complete record.

- Exit 0: both checks completed with no diagnostics or errors, unchanged source
  and successful cleanup.
- Exit 1: both checks completed without errors, source stayed unchanged and
  cleanup succeeded, but diagnostic findings remain.
- Exit 2: invalid input, skipped/incomplete/error checks, changed source, or
  incomplete temporary-output cleanup. This takes precedence over findings in
  completed stages; do not interpret it as a clean run.

A `no-byte-compile` file is explicitly skipped by the compiler. Checkdoc
exceptions are errors, not an empty successful callback. Compiler errors count
even when `byte-compile-file` returns nil without throwing an outer exception.
Positions are native Emacs buffer positions, not invented line numbers.

## Execution and evidence limits

The helper writes compilation output only in an owned private directory outside
Drive and never replaces adjacent `.elc` files. Cleanup reports a retained path
if unexpected artifacts prevent safe completion; inspect and preserve them rather
than recursively deleting an unknown tree.

The checkdoc pass reads source without visiting it through user hooks or executing
file-local eval forms, disables autofixing and does not use the Flymake wrapper
that suppresses checkdoc exceptions. Byte compilation still evaluates compile-time
code and dependencies; this process is not a security sandbox.

Pre/post target hashes detect observed drift, not an atomic snapshot or proof that
all dependencies remained unchanged. Results apply to the observed source and
provided batch context, not a live package or commit receipt. Keep required
package/project verification separate.

## Relevant semantics

`defvar` is not an inert warning annotation: with an initializer it marks the
variable special permanently; without one the declaration affects the current
lexical scope/file. See [GNU variable definitions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html).
The installed Emacs 30.2 compiler documents nil for compilation errors and
`no-byte-compile` for a deliberate skip; its checkdoc Flymake wrapper catches
errors. The helper checks the underlying operations directly instead.
