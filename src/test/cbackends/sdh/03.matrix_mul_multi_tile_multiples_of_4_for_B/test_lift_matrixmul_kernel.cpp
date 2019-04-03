
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
    float * v_initial_param_627106_215190 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_627107_215191 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_627123_215703 = reinterpret_cast<float *>(GPEQ_POP());
    int v_K_215180 = GPEQ_POP();
    int v_M_215179 = GPEQ_POP();
    int v_N_215178 = GPEQ_POP();
    for (int v_tile_batch_627181 = 0;(v_tile_batch_627181 <= 1); (++v_tile_batch_627181)){
        int v_virtual_tile_id_627182 = (GPE_TILE_ID() + (v_tile_batch_627181 * 2));
        int v_i_215187 = v_virtual_tile_id_627182;
        if ((v_virtual_tile_id_627182 < 2)){
            {
                for (int v_gpe_batch_627183 = 0;(v_gpe_batch_627183 <= (v_N_215178 / 4)); (++v_gpe_batch_627183)){
                    
__asm__ __volatile__ (
"dmb\n\t"
);
    ; 
                    int v_i_215188 = GPEQ_POP();
                    if ((v_i_215188 < v_N_215178)){
                        // For each element reduced sequentially
                        v_user_func_627123_215703[(v_i_215188 + (2 * v_N_215178 * v_i_215186) + (v_N_215178 * v_i_215187))] = 0.0f; 
                        for (int v_i_215189 = 0;(v_i_215189 <= (-1 + v_K_215180)); (++v_i_215189)){
                            v_user_func_627123_215703[(v_i_215188 + (2 * v_N_215178 * v_i_215186) + (v_N_215178 * v_i_215187))] = multAndSumUp(v_user_func_627123_215703[(v_i_215188 + (2 * v_N_215178 * v_i_215186) + (v_N_215178 * v_i_215187))], v_initial_param_627106_215190[(v_i_215189 + (2 * v_K_215180 * v_i_215186) + (v_K_215180 * v_i_215187))], v_initial_param_627107_215191[(v_i_215189 + (v_K_215180 * v_i_215188))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}