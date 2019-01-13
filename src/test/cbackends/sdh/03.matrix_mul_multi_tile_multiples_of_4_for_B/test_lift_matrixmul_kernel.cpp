
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <string.h>
#include <bits/stdc++.h>

using namespace std;

#include "util.hpp"

    ; 
float multAndSumUp(float acc, float l, float r){
    { return acc + (l * r); }; 
}
int main(){
    // Pop input, output pointers and sizes
    float * v_initial_param_1_16 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_2_17 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_58_20 = reinterpret_cast<float *>(GPEQ_POP());
    int v_K_3 = GPEQ_POP();
    int v_M_2 = GPEQ_POP();
    int v_N_1 = GPEQ_POP();
    // For each transmuter
    for (int v_i_11 = 0;(v_i_11 <= (-1 + ((v_M_2)/(2)))); (++v_i_11)){
        // For each tile
        int v_i_12 = GPE_TILE_ID();
        // For each element processed sequentially
        for (int v_i_13 = 0;(v_i_13 < ((v_N_1)/(4))); v_i_13 = (v_i_13 + 1)){
            // For each GPE. TODO: check if you can get this by API call instead of push and pop
            int v_i_14 = GPEQ_POP();
            // For each element reduced sequentially
            v_user_func_58_20[(v_i_14 + (2 * v_N_1 * v_i_11) + (v_N_1 * v_i_12) + (4 * v_i_13))] = multAndSumUp(0.0f, v_initial_param_1_16[0], v_initial_param_2_17[0]); 
            for (int v_i_15 = 1;(v_i_15 <= (-1 + v_K_3)); (++v_i_15)){
                v_user_func_58_20[(v_i_14 + (2 * v_N_1 * v_i_11) + (v_N_1 * v_i_12) + (4 * v_i_13))] = multAndSumUp(v_user_func_58_20[(v_i_14 + (2 * v_N_1 * v_i_11) + (v_N_1 * v_i_12) + (4 * v_i_13))], v_initial_param_1_16[(v_i_15 + (2 * v_K_3 * v_i_11) + (v_K_3 * v_i_12))], v_initial_param_2_17[(v_i_15 + (4 * v_K_3 * v_i_13) + (v_K_3 * v_i_14))]); 
            }
        }
        // Sync to LCP
        LCPQ_PUSH(1); 
    }
}