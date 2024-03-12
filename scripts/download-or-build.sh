#!/usr/bin/env bash

set -ue

if [ -f "./target/release/libextism_hs.a" ]; then
  echo "Extism library already installed, remove ./target/release/libextism_hs.a to rebuild it"
  exit 0
fi

if ! [ -x "$(command -v extism)" ]; then
  echo 'Extism CLI is not installed. Building Extism using cargo' >&2
  cargo build --release
else
  VERSION=$(grep 'extism = ".*"' Cargo.toml | awk '{ print $3 }' | sed 's/"//g')
  extism lib install --prefix ./target/extism --version v$VERSION
  mkdir -p ./target/release
  cp ./target/extism/lib/libextism.a ./target/release/libextism_hs.a
fi

