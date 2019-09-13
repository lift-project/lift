

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
double absolute_uf(double x){
    { return x>=0? x : x * (-1.0); }; 
}

#endif
 ; 
void lift_absolute_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15891_7402 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15895_7466 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16511 = 0;(v_tile_batch_16511 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16511)){
        int v_virtual_tile_id_16512 = (GPE_TILE_ID() + (v_tile_batch_16511 * 2));
        int v_i_7399 = v_virtual_tile_id_16512;
        if ((v_virtual_tile_id_16512 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16513 = 0;(v_gpe_batch_16513 <= 1); (++v_gpe_batch_16513)){
                    ; 
                    int v_i_7400 = GPEQ_POP();
                    if ((v_i_7400 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7401 = 0;(v_i_7401 < 2); v_i_7401 = (v_i_7401 + 1)){
                            v_user_func_15895_7466[(v_i_7401 + (2 * v_i_7400) + (8 * v_i_7399))] = absolute_uf(v_initial_param_15891_7402[(v_i_7401 + (2 * v_i_7400) + (8 * v_i_7399))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}