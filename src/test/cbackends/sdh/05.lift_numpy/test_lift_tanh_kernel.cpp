

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
    double * v_initial_param_14200_5634 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14204_5698 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16225 = 0;(v_tile_batch_16225 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16225)){
        int v_virtual_tile_id_16226 = (GPE_TILE_ID() + (v_tile_batch_16225 * 2));
        int v_i_5631 = v_virtual_tile_id_16226;
        if ((v_virtual_tile_id_16226 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16227 = 0;(v_gpe_batch_16227 <= 1); (++v_gpe_batch_16227)){
                    ; 
                    int v_i_5632 = GPEQ_POP();
                    if ((v_i_5632 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5633 = 0;(v_i_5633 < 2); v_i_5633 = (v_i_5633 + 1)){
                            v_user_func_14204_5698[(v_i_5633 + (2 * v_i_5632) + (8 * v_i_5631))] = tanh_uf(v_initial_param_14200_5634[(v_i_5633 + (2 * v_i_5632) + (8 * v_i_5631))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}