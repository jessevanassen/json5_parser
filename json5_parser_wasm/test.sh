#!/usr/bin/env sh

set -e

cd "$(dirname "$(readlink -f -- "$0")")"

set -x

./build.sh

git submodule update --init

node ./test_runner.mjs
