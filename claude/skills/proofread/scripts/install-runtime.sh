#!/bin/sh

set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
runtime_dir=$(node "$script_dir/resolve-runtime-dir.mjs")

exec yarn install --frozen-lockfile --modules-folder "$runtime_dir/node_modules"
