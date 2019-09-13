

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
double negative_uf(double x){
    return (-1.0f)*x; 
}

#endif
 ; 
void lift_negative_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15496_7130 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15500_7194 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16467 = 0;(v_tile_batch_16467 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16467)){
        int v_virtual_tile_id_16468 = (GPE_TILE_ID() + (v_tile_batch_16467 * 2));
        int v_i_7127 = v_virtual_tile_id_16468;
        if ((v_virtual_tile_id_16468 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16469 = 0;(v_gpe_batch_16469 <= 1); (++v_gpe_batch_16469)){
                    ; 
                    int v_i_7128 = GPEQ_POP();
                    if ((v_i_7128 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7129 = 0;(v_i_7129 < 2); v_i_7129 = (v_i_7129 + 1)){
                            v_user_func_15500_7194[(v_i_7129 + (2 * v_i_7128) + (8 * v_i_7127))] = negative_uf(v_initial_param_15496_7130[(v_i_7129 + (2 * v_i_7128) + (8 * v_i_7127))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}