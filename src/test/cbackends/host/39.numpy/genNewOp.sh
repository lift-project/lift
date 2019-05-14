#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

sed 's/sinh/'"$1"'/g' sinh.cpp > $1.cpp
echo "change data please ..."
sleep(1)
nvim $1.cpp

sed 's/sinh/'"$1"'/g' ./numpy/sinh.py > ./numpy/$1.py
chomod +x ./numpy/$1.py
echo "change to the same data please ..."
sleep(1)
nvim ./numpy/$1.py

echo "add compilation commands please ..."
sleep(1)
nvim genMain.sh
