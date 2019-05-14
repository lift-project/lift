#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

\find lift_numpy/ -name '*.cpp' | parallel echo "\#include \<{}\>" > lift_numpy.hpp

cat ./lift_numpy.hpp

# g++ ./sin.cpp -I . && ./a.out && rm a.out
# g++ ./cos.cpp -I . && ./a.out && rm a.out
# g++ ./tan.cpp -I . && ./a.out && rm a.out
# g++ ./arcsin.cpp -I . && ./a.out && rm a.out
# g++ ./arccos.cpp -I . && ./a.out && rm a.out
# g++ ./arctan.cpp -I . && ./a.out && rm a.out
# g++ ./hypot.cpp -I . && ./a.out && rm a.out
# g++ ./arctan2.cpp -I . && ./a.out && rm a.out
# g++ ./degrees.cpp -I . && ./a.out && rm a.out
# g++ ./radians.cpp -I . && ./a.out && rm a.out

# g++ ./deg2rad.cpp -I . && ./a.out && rm a.out
# g++ ./rad2deg.cpp -I . && ./a.out && rm a.out

g++ ./sinh.cpp -I . && ./a.out && rm a.out
g++ ./cosh.cpp -I . && ./a.out && rm a.out


if [ -z a.out ]; then rm a.out; fi



 
