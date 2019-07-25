

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H
; 
double arcsinh_uf(double x){
    { return asinh(x); }; 
}

#endif
 ; 
void lift_arcsinh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_637_1087 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_641_1151 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2659 = 0;(v_tile_batch_2659 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2659)){
        int v_virtual_tile_id_2660 = (GPE_TILE_ID() + (v_tile_batch_2659 * 2));
        int v_i_1084 = v_virtual_tile_id_2660;
        if ((v_virtual_tile_id_2660 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2661 = 0;(v_gpe_batch_2661 <= 1); (++v_gpe_batch_2661)){
                    ; 
                    int v_i_1085 = GPEQ_POP();
                    if ((v_i_1085 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1086 = 0;(v_i_1086 < 2); v_i_1086 = (v_i_1086 + 1)){
                            v_user_func_641_1151[(v_i_1086 + (2 * v_i_1085) + (8 * v_i_1084))] = arcsinh_uf(v_initial_param_637_1087[(v_i_1086 + (2 * v_i_1085) + (8 * v_i_1084))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}