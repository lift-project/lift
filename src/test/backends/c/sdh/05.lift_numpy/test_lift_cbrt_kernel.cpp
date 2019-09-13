

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef CBRT_UF_H
#define CBRT_UF_H
; 
double cbrt_uf(double x){
    { return cbrt(x); }; 
}

#endif
 ; 
void lift_cbrt_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15795_7266 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15799_7330 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16489 = 0;(v_tile_batch_16489 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16489)){
        int v_virtual_tile_id_16490 = (GPE_TILE_ID() + (v_tile_batch_16489 * 2));
        int v_i_7263 = v_virtual_tile_id_16490;
        if ((v_virtual_tile_id_16490 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16491 = 0;(v_gpe_batch_16491 <= 1); (++v_gpe_batch_16491)){
                    ; 
                    int v_i_7264 = GPEQ_POP();
                    if ((v_i_7264 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7265 = 0;(v_i_7265 < 2); v_i_7265 = (v_i_7265 + 1)){
                            v_user_func_15799_7330[(v_i_7265 + (2 * v_i_7264) + (8 * v_i_7263))] = cbrt_uf(v_initial_param_15795_7266[(v_i_7265 + (2 * v_i_7264) + (8 * v_i_7263))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}