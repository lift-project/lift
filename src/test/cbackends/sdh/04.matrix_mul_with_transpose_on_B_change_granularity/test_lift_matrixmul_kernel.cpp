

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
    float * v_initial_param_16607_7621 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_16608_7622 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_16621_8134 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_7608 = GPEQ_POP();
    int v_K_7609 = GPEQ_POP();
    int v_N_7607 = GPEQ_POP();
    for (int v_tile_batch_16693 = 0;(v_tile_batch_16693 <= (((v_M_7608)/((v_M_7608 / 2))) / 2)); (++v_tile_batch_16693)){
        int v_virtual_tile_id_16694 = (GPE_TILE_ID() + (v_tile_batch_16693 * 2));
        int v_i_7616 = v_virtual_tile_id_16694;
        if ((v_virtual_tile_id_16694 < ((v_M_7608)/((v_M_7608 / 2))))){
            
        }
    }
}