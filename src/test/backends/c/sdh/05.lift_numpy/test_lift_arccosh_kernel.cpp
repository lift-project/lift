

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
    double * v_initial_param_14296_5770 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14300_5834 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16247 = 0;(v_tile_batch_16247 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16247)){
        int v_virtual_tile_id_16248 = (GPE_TILE_ID() + (v_tile_batch_16247 * 2));
        int v_i_5767 = v_virtual_tile_id_16248;
        if ((v_virtual_tile_id_16248 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16249 = 0;(v_gpe_batch_16249 <= 1); (++v_gpe_batch_16249)){
                    ; 
                    int v_i_5768 = GPEQ_POP();
                    if ((v_i_5768 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5769 = 0;(v_i_5769 < 2); v_i_5769 = (v_i_5769 + 1)){
                            v_user_func_14300_5834[(v_i_5769 + (2 * v_i_5768) + (8 * v_i_5767))] = arccosh_uf(v_initial_param_14296_5770[(v_i_5769 + (2 * v_i_5768) + (8 * v_i_5767))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}