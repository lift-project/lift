#!/usr/bin/env bash 

set -e
set -o pipefail


# Activate debug
# set -x

my_path="/home/lu/Documents/Research/lift/src/test/backends.c/sdh/04.matrix_mul_abitrary_size_for_A_B"

gedit $my_path/test_lift_matrixmul_sched_lib.hpp &

gedit --new-window $my_path/test_lift_matrixmul_kernel.cpp
