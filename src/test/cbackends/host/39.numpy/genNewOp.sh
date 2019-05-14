#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

echo sed 's/sinh/'"$1"'/g' sinh.cpp > $1.cpp
# nvim $1.cpp

# sed 's/sinh/'"$1"'/g' ./numpy/sinh.py > ./numpy/$1.py
# nvim ./numpy/$1.py
