

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef TANH_UF_H
#define TANH_UF_H
; 
double tanh_uf(double x){
    { return tanh(x); }; 
}

#endif
 ; 
void lift_tanh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_589_1019 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_593_1083 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2648 = 0;(v_tile_batch_2648 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2648)){
        int v_virtual_tile_id_2649 = (GPE_TILE_ID() + (v_tile_batch_2648 * 2));
        int v_i_1016 = v_virtual_tile_id_2649;
        if ((v_virtual_tile_id_2649 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2650 = 0;(v_gpe_batch_2650 <= 1); (++v_gpe_batch_2650)){
                    ; 
                    int v_i_1017 = GPEQ_POP();
                    if ((v_i_1017 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1018 = 0;(v_i_1018 < 2); v_i_1018 = (v_i_1018 + 1)){
                            v_user_func_593_1083[(v_i_1018 + (2 * v_i_1017) + (8 * v_i_1016))] = tanh_uf(v_initial_param_589_1019[(v_i_1018 + (2 * v_i_1017) + (8 * v_i_1016))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}