
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
    float * v_user_func_46_78 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_2 = GPEQ_POP();
    int v_K_3 = GPEQ_POP();
    int v_N_1 = GPEQ_POP();
    for (int v_tile_batch_87 = 0;(v_tile_batch_87 <= (v_M_2 / 2)); (++v_tile_batch_87)){
        int v_virtual_tile_id_88 = (GPE_TILE_ID() + (v_tile_batch_87 * 2));
        int v_i_9 = v_virtual_tile_id_88;
        if ((v_virtual_tile_id_88 < v_M_2)){
            {
                for (int v_gpe_batch_89 = 0;(v_gpe_batch_89 <= (v_N_1 / 4)); (++v_gpe_batch_89)){
                    
                    __asm__ __volatile__ (
                    "dmb\n\t"
                    ); 
                    int v_i_10 = GPEQ_POP();
                    if ((v_i_10 < v_N_1)){
                        // For each element reduced sequentially
                        v_user_func_46_78[(v_i_10 + (v_N_1 * v_i_9))] = 0.0f; 
                        for (int v_i_11 = 0;(v_i_11 <= (-1 + v_K_3)); (++v_i_11)){
                            v_user_func_46_78[(v_i_10 + (v_N_1 * v_i_9))] = multAndSumUp(v_user_func_46_78[(v_i_10 + (v_N_1 * v_i_9))], v_initial_param_1_12[(v_i_11 + (v_K_3 * v_i_9))], v_initial_param_2_13[(v_i_11 + (v_K_3 * v_i_10))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}