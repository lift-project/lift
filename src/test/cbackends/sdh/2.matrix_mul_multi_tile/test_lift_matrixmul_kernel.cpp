
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
    float * v_initial_param_1_18 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_2_19 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_59_22 = reinterpret_cast<float *>(GPEQ_POP());
    int v_K_3 = GPEQ_POP();
    int v_M_2 = GPEQ_POP();
    int v_N_1 = GPEQ_POP();
    // For each transmuter
    for (int v_i_12 = 0;(v_i_12 <= (-1 + ((v_M_2)/(16)))); (++v_i_12)){
        // For each tile
        int v_i_13 = GPE_TILE_ID();
        // For each GPE. TODO: check if you can get this by API call instead of push and pop
        int v_i_14 = GPEQ_POP();
        // For each element processed sequentially
        for (int v_i_15 = 0;(v_i_15 < 2); v_i_15 = (v_i_15 + 1)){
            // For each element processed sequentially
            for (int v_i_16 = 0;(v_i_16 < v_N_1); v_i_16 = (v_i_16 + 1)){
                // For each element reduced sequentially
                v_user_func_59_22[(v_i_16 + (16 * v_N_1 * v_i_12) + (8 * v_N_1 * v_i_13) + (2 * v_N_1 * v_i_14) + (v_N_1 * v_i_15))] = multAndSumUp(0.0f, v_initial_param_1_18[0], v_initial_param_2_19[0]); 
                for (int v_i_17 = 1;(v_i_17 <= (-1 + v_K_3)); (++v_i_17)){
                    v_user_func_59_22[(v_i_16 + (16 * v_N_1 * v_i_12) + (8 * v_N_1 * v_i_13) + (2 * v_N_1 * v_i_14) + (v_N_1 * v_i_15))] = multAndSumUp(v_user_func_59_22[(v_i_16 + (16 * v_N_1 * v_i_12) + (8 * v_N_1 * v_i_13) + (2 * v_N_1 * v_i_14) + (v_N_1 * v_i_15))], v_initial_param_1_18[(v_i_17 + (16 * v_K_3 * v_i_12) + (8 * v_K_3 * v_i_13) + (2 * v_K_3 * v_i_14) + (v_K_3 * v_i_15))], v_initial_param_2_19[(v_i_17 + (v_K_3 * v_i_16))]); 
                }
            }
        }
        // Sync to LCP
        LCPQ_PUSH(1); 
    }
}