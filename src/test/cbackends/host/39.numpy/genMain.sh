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

# g++ ./sinh.cpp -I . && ./a.out && rm a.out
# g++ ./cosh.cpp -I . && ./a.out && rm a.out
# g++ ./tanh.cpp -I . && ./a.out && rm a.out
# g++ ./arcsinh.cpp -I . && ./a.out && rm a.out
# g++ ./arccosh.cpp -I . && ./a.out && rm a.out
# g++ ./arctanh.cpp -I . && ./a.out && rm a.out

# g++ ./around.cpp -I . && ./a.out && rm a.out
# g++ ./round_.cpp -I . && ./a.out && rm a.out
# g++ ./rint.cpp -I . && ./a.out && rm a.out
# g++ ./fix.cpp -I . && ./a.out && rm a.out
# g++ ./floor.cpp -I . && ./a.out && rm a.out
# g++ ./ceil.cpp -I . && ./a.out && rm a.out
# g++ ./trunc.cpp -I . && ./a.out && rm a.out


# g++ ./prod.cpp -I . && ./a.out && rm a.out
# g++ ./sum.cpp -I . && ./a.out && rm a.out
# g++ ./nanprod.cpp -I . && ./a.out && rm a.out
# g++ ./nansum.cpp -I . && ./a.out && rm a.out
# g++ ./cumprod.cpp -I . && ./a.out && rm a.out
# g++ ./cumsum.cpp -I . && ./a.out && rm a.out
# g++ ./nancumprod.cpp -I . && ./a.out && rm a.out
# g++ ./nancumsum.cpp -I . && ./a.out && rm a.out
# g++ ./diff.cpp -I . && ./a.out && rm a.out
# g++ ./ediff1d.cpp -I . && ./a.out && rm a.out
# g++ ./gradient.cpp -I . && ./a.out && rm a.out
# g++ ./cross.cpp -I . && ./a.out && rm a.out
# g++ ./trapz.cpp -I . && ./a.out && rm a.out

# g++ ./exp.cpp -I . && ./a.out && rm a.out
# g++ ./expm1.cpp -I . && ./a.out && rm a.out
# g++ ./exp2.cpp -I . && ./a.out && rm a.out
# g++ ./log.cpp -I . && ./a.out && rm a.out
g++ ./log10.cpp -I . && ./a.out && rm a.out


if [ -f a.out ]; then rm a.out; fi



 
