
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
    float * v_user_func_52_528 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_2 = GPEQ_POP();
    int v_K_3 = GPEQ_POP();
    int v_N_1 = GPEQ_POP();
    for (int v_tile_batch_110 = 0;(v_tile_batch_110 <= 1); (++v_tile_batch_110)){
        int v_virtual_tile_id_111 = (GPE_TILE_ID() + (v_tile_batch_110 * 2));
        int v_i_11 = v_virtual_tile_id_111;
        if ((v_virtual_tile_id_111 < 2)){
            {
                for (int v_gpe_batch_112 = 0;(v_gpe_batch_112 <= (v_N_1 / 4)); (++v_gpe_batch_112)){
                    
                    __asm__ __volatile__ (
                    "dmb\n\t"
                    ); 
                    int v_i_12 = GPEQ_POP();
                    if ((v_i_12 < v_N_1)){
                        // For each element reduced sequentially
                        v_user_func_52_528[(v_i_12 + (2 * v_N_1 * v_i_10) + (v_N_1 * v_i_11))] = 0.0f; 
                        for (int v_i_13 = 0;(v_i_13 <= (-1 + v_K_3)); (++v_i_13)){
                            v_user_func_52_528[(v_i_12 + (2 * v_N_1 * v_i_10) + (v_N_1 * v_i_11))] = multAndSumUp(v_user_func_52_528[(v_i_12 + (2 * v_N_1 * v_i_10) + (v_N_1 * v_i_11))], v_initial_param_1_14[(v_i_13 + (2 * v_K_3 * v_i_10) + (v_K_3 * v_i_11))], v_initial_param_2_15[(v_i_13 + (v_K_3 * v_i_12))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}