

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef MULTANDSUMUP_H
#define MULTANDSUMUP_H
; 
float multAndSumUp(float acc, float l, float r){
    { return acc + (l * r); }; 
}

#endif
 ; 
void lift_matrixmul_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    float * v_initial_param_72_15 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_73_16 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_117_81 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_5 = GPEQ_POP();
    int v_K_6 = GPEQ_POP();
    int v_N_4 = GPEQ_POP();
    for (int v_tile_batch_164 = 0;(v_tile_batch_164 <= (v_M_5 / 2)); (++v_tile_batch_164)){
        int v_virtual_tile_id_165 = (GPE_TILE_ID() + (v_tile_batch_164 * 2));
        int v_i_12 = v_virtual_tile_id_165;
        if ((v_virtual_tile_id_165 < v_M_5)){
            {
                for (int v_gpe_batch_166 = 0;(v_gpe_batch_166 <= (v_N_4 / 4)); (++v_gpe_batch_166)){
                    ; 
                    int v_i_13 = GPEQ_POP();
                    if ((v_i_13 < v_N_4)){
                        // For each element reduced sequentially
                        v_user_func_117_81[(v_i_13 + (v_N_4 * v_i_12))] = 0.0f; 
                        for (int v_i_14 = 0;(v_i_14 <= (-1 + v_K_6)); (++v_i_14)){
                            v_user_func_117_81[(v_i_13 + (v_N_4 * v_i_12))] = multAndSumUp(v_user_func_117_81[(v_i_13 + (v_N_4 * v_i_12))], v_initial_param_72_15[(v_i_14 + (v_K_6 * v_i_12))], v_initial_param_73_16[(v_i_13 + (v_N_4 * v_i_14))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}