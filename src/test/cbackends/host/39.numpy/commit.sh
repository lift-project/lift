#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

git add * && git commit -a -m  "one more numpy op done"  && git push
