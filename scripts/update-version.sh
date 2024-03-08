#!/usr/bin/env bash

sed -i s/"version v.\.."/"version v$1"/g scripts/download-or-build.sh
sed -i s/"extism = \".*\""/"extism = \"$1\""/g Cargo.toml
