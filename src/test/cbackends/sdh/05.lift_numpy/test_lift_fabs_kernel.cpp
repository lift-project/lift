

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
double absolute_uf(double x){
    { return x>=0? x : x * (-1.0); }; 
}

#endif
 ; 
void lift_fabs_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_2314_2855 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_2318_2919 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2945 = 0;(v_tile_batch_2945 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2945)){
        int v_virtual_tile_id_2946 = (GPE_TILE_ID() + (v_tile_batch_2945 * 2));
        int v_i_2852 = v_virtual_tile_id_2946;
        if ((v_virtual_tile_id_2946 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2947 = 0;(v_gpe_batch_2947 <= 1); (++v_gpe_batch_2947)){
                    ; 
                    int v_i_2853 = GPEQ_POP();
                    if ((v_i_2853 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2854 = 0;(v_i_2854 < 2); v_i_2854 = (v_i_2854 + 1)){
                            v_user_func_2318_2919[(v_i_2854 + (2 * v_i_2853) + (8 * v_i_2852))] = absolute_uf(v_initial_param_2314_2855[(v_i_2854 + (2 * v_i_2853) + (8 * v_i_2852))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}