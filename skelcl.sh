#!/bin/bash

set -e

CONFIG_FILE=skelcl.conf
SKELCL_CMAKE_COMMON_FLAGS="-DBUILD_EXECUTOR=ON -DTHROW_ON_FAILURE=ON"

function check_command(){
  echo -ne "Checking for ${1}... "
  if ! type "$1" > /dev/null; then
    echo "[failed]"
    echo "Missing command $1" >&2
    exit -1
  fi
  echo "[ok]"
}

function init(){
  if [ ! -f ${CONFIG_FILE} ]; then 
    echo "Creating config..."
    cat << EOF > ${CONFIG_FILE}
#SkelCL config flags
SKELCL_CMAKE_FLAGS=
SKELCL_LLVM_PATH=
EOF
  fi

  echo "Reading config..."
  source `pwd`/${CONFIG_FILE}
}

function update(){
  check_command "git"
  git submodule init
  git submodule update
}

function configure(){
  check_command "cmake"
  check_command "g++"

  # SkelCL
  mkdir -p lib/SkelCL/build
  pushd lib/SkelCL
  if [ ! -d libraries/gtest ]; then
    wget http://googletest.googlecode.com/files/gtest-1.7.0.zip
    unzip -q gtest-1.7.0.zip
    mv gtest-1.7.0 libraries/gtest
    rm gtest-1.7.0.zip
  fi

  if [ ! -e libraries/stooling/libraries/llvm ]; then
    llvm_found=false
    set +e

    if [ -z "$SKELCL_LLVM_PATH" ]; then
      if type "llvm-config" > /dev/null; then
        llvm-config --bindir
        if [ "$?" = 0 ]; then
          llvm_path=$(llvm-config --bindir)
          echo "LLVM install path found in ${llvm_path}/.., so you want to use it?"
          echo "(y)es or (n)o "
          read ANSWER
          if [[ "$ANSWER" == 'y' ]]; then
            ln -s "${llvm_path}/.." libraries/stooling/libraries/llvm
            llvm_found=true
          fi
        fi
      fi
      set -e
      if [ "${llvm_found}" = false ]; then
        echo "[error] Cannot find LLVM. You need to configure LLVM for SkelCL manually" >&2 
        exit -1
      fi
    else
      echo "Using LLVM path ${SKELCL_LLVM_PATH} set in ${CONFIG_FILE}"
      ln -s "${SKELCL_LLVM_PATH}" libraries/stooling/libraries/llvm
      set -e
      fi
  fi


  (cd build && cmake ${SKELCL_CMAKE_COMMON_FLAGS} ${SKELCL_CMAKE_FLAGS} ..)
  popd
}

function build(){
  check_command "g++"
  check_command "cmake"

  # SkelCL
  pushd lib/SkelCL/build
  make
  popd
}

init
update
configure
build
