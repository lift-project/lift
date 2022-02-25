#!/usr/bin/env bash

set -e
set -o pipefail

# Activate debug
# set -x

g++ ./main.cpp -DFULL_THREADS -lOpenCL && ./a.out && rm a.out
