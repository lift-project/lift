

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCCOSH_UF_H
#define ARCCOSH_UF_H
; 
double arccosh_uf(double x){
    { return acosh(x); }; 
}

#endif
 ; 
void lift_arccosh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_685_1155 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_689_1219 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2670 = 0;(v_tile_batch_2670 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2670)){
        int v_virtual_tile_id_2671 = (GPE_TILE_ID() + (v_tile_batch_2670 * 2));
        int v_i_1152 = v_virtual_tile_id_2671;
        if ((v_virtual_tile_id_2671 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2672 = 0;(v_gpe_batch_2672 <= 1); (++v_gpe_batch_2672)){
                    ; 
                    int v_i_1153 = GPEQ_POP();
                    if ((v_i_1153 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1154 = 0;(v_i_1154 < 2); v_i_1154 = (v_i_1154 + 1)){
                            v_user_func_689_1219[(v_i_1154 + (2 * v_i_1153) + (8 * v_i_1152))] = arccosh_uf(v_initial_param_685_1155[(v_i_1154 + (2 * v_i_1153) + (8 * v_i_1152))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}