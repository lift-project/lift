

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
double trunc_uf(double x){
    return trunc(x);; 
}

#endif
 ; 
void lift_trunc_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14632_6314 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14636_6378 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16335 = 0;(v_tile_batch_16335 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16335)){
        int v_virtual_tile_id_16336 = (GPE_TILE_ID() + (v_tile_batch_16335 * 2));
        int v_i_6311 = v_virtual_tile_id_16336;
        if ((v_virtual_tile_id_16336 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16337 = 0;(v_gpe_batch_16337 <= 1); (++v_gpe_batch_16337)){
                    ; 
                    int v_i_6312 = GPEQ_POP();
                    if ((v_i_6312 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6313 = 0;(v_i_6313 < 2); v_i_6313 = (v_i_6313 + 1)){
                            v_user_func_14636_6378[(v_i_6313 + (2 * v_i_6312) + (8 * v_i_6311))] = trunc_uf(v_initial_param_14632_6314[(v_i_6313 + (2 * v_i_6312) + (8 * v_i_6311))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}