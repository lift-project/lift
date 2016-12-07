#!/bin/bash

set -e

function check_command() {
  echo -ne "Checking for ${1}... "
  if ! type "$1" > /dev/null; then
    echo "[failed]"
    echo "Missing command $1" >&2
    exit -1
  fi
  echo "[ok]"
}

check_command "git"
echo "Update git submodules"
git submodule init
git submodule update
git submodule status 

