

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
    double * v_initial_param_14248_5702 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14252_5766 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16236 = 0;(v_tile_batch_16236 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16236)){
        int v_virtual_tile_id_16237 = (GPE_TILE_ID() + (v_tile_batch_16236 * 2));
        int v_i_5699 = v_virtual_tile_id_16237;
        if ((v_virtual_tile_id_16237 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16238 = 0;(v_gpe_batch_16238 <= 1); (++v_gpe_batch_16238)){
                    ; 
                    int v_i_5700 = GPEQ_POP();
                    if ((v_i_5700 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5701 = 0;(v_i_5701 < 2); v_i_5701 = (v_i_5701 + 1)){
                            v_user_func_14252_5766[(v_i_5701 + (2 * v_i_5700) + (8 * v_i_5699))] = arcsinh_uf(v_initial_param_14248_5702[(v_i_5701 + (2 * v_i_5700) + (8 * v_i_5699))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}