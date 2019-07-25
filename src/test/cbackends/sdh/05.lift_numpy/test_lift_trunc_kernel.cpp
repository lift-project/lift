

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
    double * v_initial_param_1021_1699 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1025_1763 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2758 = 0;(v_tile_batch_2758 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2758)){
        int v_virtual_tile_id_2759 = (GPE_TILE_ID() + (v_tile_batch_2758 * 2));
        int v_i_1696 = v_virtual_tile_id_2759;
        if ((v_virtual_tile_id_2759 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2760 = 0;(v_gpe_batch_2760 <= 1); (++v_gpe_batch_2760)){
                    ; 
                    int v_i_1697 = GPEQ_POP();
                    if ((v_i_1697 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1698 = 0;(v_i_1698 < 2); v_i_1698 = (v_i_1698 + 1)){
                            v_user_func_1025_1763[(v_i_1698 + (2 * v_i_1697) + (8 * v_i_1696))] = trunc_uf(v_initial_param_1021_1699[(v_i_1698 + (2 * v_i_1697) + (8 * v_i_1696))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}