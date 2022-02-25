#!/usr/bin/env bash

set -e
set -o pipefail

# Activate debug
# set -x

sed '11,20d' | sed 's/execute(/execute2(/g'  > $1
