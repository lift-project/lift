

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef RECIPROCAL_UF_H
#define RECIPROCAL_UF_H
; 
double reciprocal_uf(double x){
    return 1.0f/x; 
}

#endif
 ; 
void lift_reciprocal_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1823_2379 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1827_2443 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2868 = 0;(v_tile_batch_2868 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2868)){
        int v_virtual_tile_id_2869 = (GPE_TILE_ID() + (v_tile_batch_2868 * 2));
        int v_i_2376 = v_virtual_tile_id_2869;
        if ((v_virtual_tile_id_2869 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2870 = 0;(v_gpe_batch_2870 <= 1); (++v_gpe_batch_2870)){
                    ; 
                    int v_i_2377 = GPEQ_POP();
                    if ((v_i_2377 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2378 = 0;(v_i_2378 < 2); v_i_2378 = (v_i_2378 + 1)){
                            v_user_func_1827_2443[(v_i_2378 + (2 * v_i_2377) + (8 * v_i_2376))] = reciprocal_uf(v_initial_param_1823_2379[(v_i_2378 + (2 * v_i_2377) + (8 * v_i_2376))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}