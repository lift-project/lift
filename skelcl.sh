#!/bin/bash

set -e

CONFIG_FILE=skelcl.conf
SKELCL_CMAKE_COMMON_FLAGS="-DBUILD_EXECUTOR=ON -DTHROW_ON_FAILURE=ON -DCMAKE_BUILD_TYPE=RELEASE -DBUILD_STOOLING=OFF"
INTERACTIVE=false

while getopts ":i" opt; do 
  case $opt in
    i)
      echo "Running in interactive mode"
      INTERACTIVE=true
      ;;
    \?)
      INTERACTIVE=false
      ;;
  esac
done

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

  if $INTERACTIVE ; then 
    echo "Script running in interactive mode."
    echo "Please select platform: "
    echo "(a)rch linux, (u)buntu linux, (c)entOS, (o)sx or (s)use"
    read PLATFORM_ANSWER
    case "$PLATFORM_ANSWER" in
      "A" | "a" ) 
        echo "Using arch linux installation script"
        ./installDependenciesArchLinux.sh
        ;;
      "U" | "u" ) 
        echo "Using ubuntu linux installation script"
        ./installDependenciesUbuntu.sh
        ;;
      "C" | "c" ) 
        echo "Using CentOS installation script"
        ./installDependenciesCentOS.sh
        ;;
      "O" | "o" ) 
        echo "Using OSX installation script"
        ./installDependenciesOSX.command
        ;;
      "S" | "s" ) 
        echo "Using SUSE installation script"
        ./installDependenciesSUSE.sh
        ;;
      *)
        echo "Didn't recognise option, failing"
        exit
        ;;
    esac
  else
    if [ ! -d libraries/gtest ]; then
      wget http://googletest.googlecode.com/files/gtest-1.7.0.zip
      unzip -q gtest-1.7.0.zip
      mv gtest-1.7.0 libraries/gtest
      rm gtest-1.7.0.zip
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
  make -j 4 executor
  popd
}

init
update
configure
build
