
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
    float * v_initial_param_1_14 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_2_15 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_53_18 = reinterpret_cast<float *>(GPEQ_POP());
    int v_K_3 = GPEQ_POP();
    int v_M_2 = GPEQ_POP();
    int v_N_1 = GPEQ_POP();
    // For each transmuter
    for (int v_i_10 = 0;(v_i_10 <= (-1 + ((v_M_2)/(2)))); (++v_i_10)){
        // For each tile
        int v_i_11 = GPE_TILE_ID();
        // For each GPE. TODO: check if you can get this by API call instead of push and pop
        int v_i_12 = GPEQ_POP();
        // For each element reduced sequentially
        v_user_func_53_18[(v_i_12 + (2 * v_N_1 * v_i_10) + (v_N_1 * v_i_11))] = multAndSumUp(0.0f, v_initial_param_1_14[0], v_initial_param_2_15[0]); 
        for (int v_i_13 = 1;(v_i_13 <= (-1 + v_K_3)); (++v_i_13)){
            v_user_func_53_18[(v_i_12 + (2 * v_N_1 * v_i_10) + (v_N_1 * v_i_11))] = multAndSumUp(v_user_func_53_18[(v_i_12 + (2 * v_N_1 * v_i_10) + (v_N_1 * v_i_11))], v_initial_param_1_14[(v_i_13 + (2 * v_K_3 * v_i_10) + (v_K_3 * v_i_11))], v_initial_param_2_15[(v_i_13 + (v_K_3 * v_i_12))]); 
        }
        // Sync to LCP
        LCPQ_PUSH(1); 
    }
}