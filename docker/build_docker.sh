#!/bin/bash

set -xe

# Completely wipe:
rm -rf ./lift_clean
mkdir ./lift_clean

git clone ../ lift_clean

cd lift_clean
git submodule update --init --recursive

docker build -t dubache/lift .
