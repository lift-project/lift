#!/usr/bin/env bash

set -e
set -o pipefail

# Activate debug
# set -x


< $1/libconv.cpp sed 's/kernel//' | sed 's/KERNEL/execute/' | sed 's/global//g' | sed 's/restrict//g' > $1/libconv_cpu.cpp
