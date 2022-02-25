

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
void lift_absolute_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_2314_2787 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_2318_2851 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2934 = 0;(v_tile_batch_2934 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2934)){
        int v_virtual_tile_id_2935 = (GPE_TILE_ID() + (v_tile_batch_2934 * 2));
        int v_i_2784 = v_virtual_tile_id_2935;
        if ((v_virtual_tile_id_2935 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2936 = 0;(v_gpe_batch_2936 <= 1); (++v_gpe_batch_2936)){
                    ; 
                    int v_i_2785 = GPEQ_POP();
                    if ((v_i_2785 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2786 = 0;(v_i_2786 < 2); v_i_2786 = (v_i_2786 + 1)){
                            v_user_func_2318_2851[(v_i_2786 + (2 * v_i_2785) + (8 * v_i_2784))] = absolute_uf(v_initial_param_2314_2787[(v_i_2786 + (2 * v_i_2785) + (8 * v_i_2784))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}