

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef EXPM1_UF_H
#define EXPM1_UF_H
; 
double expm1_uf(double x){
    return exp(x) - 1 ;; 
}

#endif
 ; 
void lift_expm1_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14925_6450 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14929_6514 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16357 = 0;(v_tile_batch_16357 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16357)){
        int v_virtual_tile_id_16358 = (GPE_TILE_ID() + (v_tile_batch_16357 * 2));
        int v_i_6447 = v_virtual_tile_id_16358;
        if ((v_virtual_tile_id_16358 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16359 = 0;(v_gpe_batch_16359 <= 1); (++v_gpe_batch_16359)){
                    ; 
                    int v_i_6448 = GPEQ_POP();
                    if ((v_i_6448 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6449 = 0;(v_i_6449 < 2); v_i_6449 = (v_i_6449 + 1)){
                            v_user_func_14929_6514[(v_i_6449 + (2 * v_i_6448) + (8 * v_i_6447))] = expm1_uf(v_initial_param_14925_6450[(v_i_6449 + (2 * v_i_6448) + (8 * v_i_6447))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}