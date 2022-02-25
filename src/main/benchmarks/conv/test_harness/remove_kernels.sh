#!/usr/bin/env bash

set -e
set -o pipefail

# Activate debug
# set -x


rm $1/kernel_*.cl 2> /dev/null
