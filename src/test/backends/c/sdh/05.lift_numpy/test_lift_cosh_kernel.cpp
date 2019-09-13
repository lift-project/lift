

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef COSH_UF_H
#define COSH_UF_H
; 
double cosh_uf(double x){
    { return cosh(x); }; 
}

#endif
 ; 
void lift_cosh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14152_5566 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14156_5630 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16214 = 0;(v_tile_batch_16214 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16214)){
        int v_virtual_tile_id_16215 = (GPE_TILE_ID() + (v_tile_batch_16214 * 2));
        int v_i_5563 = v_virtual_tile_id_16215;
        if ((v_virtual_tile_id_16215 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16216 = 0;(v_gpe_batch_16216 <= 1); (++v_gpe_batch_16216)){
                    ; 
                    int v_i_5564 = GPEQ_POP();
                    if ((v_i_5564 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5565 = 0;(v_i_5565 < 2); v_i_5565 = (v_i_5565 + 1)){
                            v_user_func_14156_5630[(v_i_5565 + (2 * v_i_5564) + (8 * v_i_5563))] = cosh_uf(v_initial_param_14152_5566[(v_i_5565 + (2 * v_i_5564) + (8 * v_i_5563))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}