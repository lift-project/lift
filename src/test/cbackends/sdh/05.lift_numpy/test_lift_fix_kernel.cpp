

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef FIX_UF_H
#define FIX_UF_H
; 
double fix_uf(double x){
    return trunc(x) ;; 
}

#endif
 ; 
void lift_fix_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_877_1495 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_881_1559 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2725 = 0;(v_tile_batch_2725 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2725)){
        int v_virtual_tile_id_2726 = (GPE_TILE_ID() + (v_tile_batch_2725 * 2));
        int v_i_1492 = v_virtual_tile_id_2726;
        if ((v_virtual_tile_id_2726 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2727 = 0;(v_gpe_batch_2727 <= 1); (++v_gpe_batch_2727)){
                    ; 
                    int v_i_1493 = GPEQ_POP();
                    if ((v_i_1493 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1494 = 0;(v_i_1494 < 2); v_i_1494 = (v_i_1494 + 1)){
                            v_user_func_881_1559[(v_i_1494 + (2 * v_i_1493) + (8 * v_i_1492))] = fix_uf(v_initial_param_877_1495[(v_i_1494 + (2 * v_i_1493) + (8 * v_i_1492))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}