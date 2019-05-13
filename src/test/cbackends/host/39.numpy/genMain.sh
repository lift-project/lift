#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

\find lift_numpy/ -name '*.cpp' | parallel echo "\#include \<{}\>" > lift_numpy.hpp

cat ./lift_numpy.hpp

g++ ./sin.cpp -I . && ./a.out && rm a.out
g++ ./cos.cpp -I . && ./a.out && rm a.out
g++ ./tan.cpp -I . && ./a.out && rm a.out
g++ ./arcsin.cpp -I . && ./a.out && rm a.out




 
