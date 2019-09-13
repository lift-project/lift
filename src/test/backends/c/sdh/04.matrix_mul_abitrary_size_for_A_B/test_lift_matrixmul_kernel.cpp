

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
    float * v_initial_param_16898_8237 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_16899_8238 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_16909_8302 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_8228 = GPEQ_POP();
    int v_K_8229 = GPEQ_POP();
    int v_N_8227 = GPEQ_POP();
    for (int v_tile_batch_16950 = 0;(v_tile_batch_16950 <= (v_M_8228 / 2)); (++v_tile_batch_16950)){
        int v_virtual_tile_id_16951 = (GPE_TILE_ID() + (v_tile_batch_16950 * 2));
        int v_i_8234 = v_virtual_tile_id_16951;
        if ((v_virtual_tile_id_16951 < v_M_8228)){
            {
                for (int v_gpe_batch_16952 = 0;(v_gpe_batch_16952 <= (v_N_8227 / 4)); (++v_gpe_batch_16952)){
                    ; 
                    int v_i_8235 = GPEQ_POP();
                    if ((v_i_8235 < v_N_8227)){
                        {
                            
                        }
                        {
                            
                        }
                        // For each element reduced sequentially
                        v_user_func_16909_8302[(v_i_8235 + (v_N_8227 * v_i_8234))] = 0.0f; 
                        for (int v_i_8236 = 0;(v_i_8236 <= (-1 + v_K_8229)); (++v_i_8236)){
                            v_user_func_16909_8302[(v_i_8235 + (v_N_8227 * v_i_8234))] = multAndSumUp(v_user_func_16909_8302[(v_i_8235 + (v_N_8227 * v_i_8234))], v_initial_param_16898_8237[(v_i_8236 + (v_K_8229 * v_i_8234))], v_initial_param_16899_8238[(v_i_8236 + (v_K_8229 * v_i_8235))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}