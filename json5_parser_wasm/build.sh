#!/usr/bin/env sh

set -e

cd "$(dirname "$(readlink -f -- "$0")")"

set -x

cargo build
wasm-pack build --target nodejs --out-dir pkg/nodejs
wasm-pack build --target web --out-dir pkg/web
