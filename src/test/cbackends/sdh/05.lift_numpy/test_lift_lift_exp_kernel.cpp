

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef EXP_UF_H
#define EXP_UF_H
; 
double exp_uf(double x){
    return exp(x) ;; 
}

#endif
 ; 
void lift_lift_exp_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14877_6382 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14881_6446 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16346 = 0;(v_tile_batch_16346 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16346)){
        int v_virtual_tile_id_16347 = (GPE_TILE_ID() + (v_tile_batch_16346 * 2));
        int v_i_6379 = v_virtual_tile_id_16347;
        if ((v_virtual_tile_id_16347 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16348 = 0;(v_gpe_batch_16348 <= 1); (++v_gpe_batch_16348)){
                    ; 
                    int v_i_6380 = GPEQ_POP();
                    if ((v_i_6380 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6381 = 0;(v_i_6381 < 2); v_i_6381 = (v_i_6381 + 1)){
                            v_user_func_14881_6446[(v_i_6381 + (2 * v_i_6380) + (8 * v_i_6379))] = exp_uf(v_initial_param_14877_6382[(v_i_6381 + (2 * v_i_6380) + (8 * v_i_6379))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}