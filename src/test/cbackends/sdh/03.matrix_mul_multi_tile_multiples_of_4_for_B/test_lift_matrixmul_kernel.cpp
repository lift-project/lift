
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
    float * v_initial_param_1_12 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_2_13 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_46_16 = reinterpret_cast<float *>(GPEQ_POP());
    int v_K_3 = GPEQ_POP();
    int v_M_2 = GPEQ_POP();
    int v_N_1 = GPEQ_POP();
    // For each tile
    int v_i_9 = GPE_TILE_ID();
    {
        for (int v_gpe_batch_88 = 0;(v_gpe_batch_88 <= (v_N_1 / 4)); (++v_gpe_batch_88)){
            int v_i_10 = GPEQ_POP();
            if ((v_i_10 < v_N_1)){
                {
                    // For each element reduced sequentially
                    v_user_func_46_16[(v_i_10 + (v_N_1 * v_i_9))] = multAndSumUp(0.0f, v_initial_param_1_12[0], v_initial_param_2_13[0]); 
                    for (int v_i_11 = 1;(v_i_11 <= (-1 + v_K_3)); (++v_i_11)){
                        v_user_func_46_16[(v_i_10 + (v_N_1 * v_i_9))] = multAndSumUp(v_user_func_46_16[(v_i_10 + (v_N_1 * v_i_9))], v_initial_param_1_12[(v_i_11 + (v_K_3 * v_i_9))], v_initial_param_2_13[(v_i_11 + (v_K_3 * v_i_10))]); 
                    }
                }
            }
            LCPQ_PUSH(1); 
        }
    }
}