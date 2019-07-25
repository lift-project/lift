

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SQRT_UF_H
#define SQRT_UF_H
; 
double sqrt_uf(double x){
    { return sqrt(x); }; 
}

#endif
 ; 
void lift_sqrt_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_2170_2583 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_2174_2647 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2901 = 0;(v_tile_batch_2901 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2901)){
        int v_virtual_tile_id_2902 = (GPE_TILE_ID() + (v_tile_batch_2901 * 2));
        int v_i_2580 = v_virtual_tile_id_2902;
        if ((v_virtual_tile_id_2902 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2903 = 0;(v_gpe_batch_2903 <= 1); (++v_gpe_batch_2903)){
                    ; 
                    int v_i_2581 = GPEQ_POP();
                    if ((v_i_2581 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2582 = 0;(v_i_2582 < 2); v_i_2582 = (v_i_2582 + 1)){
                            v_user_func_2174_2647[(v_i_2582 + (2 * v_i_2581) + (8 * v_i_2580))] = sqrt_uf(v_initial_param_2170_2583[(v_i_2582 + (2 * v_i_2581) + (8 * v_i_2580))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}