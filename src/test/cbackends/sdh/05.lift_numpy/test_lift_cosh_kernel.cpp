

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef COSH_UF_H
#define COSH_UF_H
; 
double cosh_uf(double x){
    { return cosh(x); }; 
}

#endif
 ; 
void lift_cosh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_541_951 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_545_1015 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2637 = 0;(v_tile_batch_2637 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2637)){
        int v_virtual_tile_id_2638 = (GPE_TILE_ID() + (v_tile_batch_2637 * 2));
        int v_i_948 = v_virtual_tile_id_2638;
        if ((v_virtual_tile_id_2638 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2639 = 0;(v_gpe_batch_2639 <= 1); (++v_gpe_batch_2639)){
                    ; 
                    int v_i_949 = GPEQ_POP();
                    if ((v_i_949 < 4)){
                        // For each element processed sequentially
                        for (int v_i_950 = 0;(v_i_950 < 2); v_i_950 = (v_i_950 + 1)){
                            v_user_func_545_1015[(v_i_950 + (2 * v_i_949) + (8 * v_i_948))] = cosh_uf(v_initial_param_541_951[(v_i_950 + (2 * v_i_949) + (8 * v_i_948))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}