#!/bin/sh

set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
node_modules_dir=$(node "$script_dir/resolve-runtime-dir.mjs")

exec yarn install --frozen-lockfile --modules-folder "$node_modules_dir"
