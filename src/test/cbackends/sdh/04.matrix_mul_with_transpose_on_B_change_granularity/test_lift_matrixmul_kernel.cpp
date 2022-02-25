

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
    float * v_initial_param_72_19 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_73_20 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_120_533 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_5 = GPEQ_POP();
    int v_K_6 = GPEQ_POP();
    int v_N_4 = GPEQ_POP();
    for (int v_tile_batch_192 = 0;(v_tile_batch_192 <= (((v_M_5)/((v_M_5 / 2))) / 2)); (++v_tile_batch_192)){
        int v_virtual_tile_id_193 = (GPE_TILE_ID() + (v_tile_batch_192 * 2));
        int v_i_14 = v_virtual_tile_id_193;
        if ((v_virtual_tile_id_193 < ((v_M_5)/((v_M_5 / 2))))){
            
        }
    }
}