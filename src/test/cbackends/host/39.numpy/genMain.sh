#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

\find lift_numpy/ -name '*.cpp' | parallel echo "\#include \<{}\>" > lift_numpy.hpp

cat ./lift_numpy.hpp

g++ ./main.cpp -I . && ./a.out && rm a.out




 
