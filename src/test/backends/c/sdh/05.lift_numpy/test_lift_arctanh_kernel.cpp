

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
double arctanh_uf(double x){
    { return atanh(x); }; 
}

#endif
 ; 
void lift_arctanh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14344_5838 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14348_5902 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16258 = 0;(v_tile_batch_16258 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16258)){
        int v_virtual_tile_id_16259 = (GPE_TILE_ID() + (v_tile_batch_16258 * 2));
        int v_i_5835 = v_virtual_tile_id_16259;
        if ((v_virtual_tile_id_16259 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16260 = 0;(v_gpe_batch_16260 <= 1); (++v_gpe_batch_16260)){
                    ; 
                    int v_i_5836 = GPEQ_POP();
                    if ((v_i_5836 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5837 = 0;(v_i_5837 < 2); v_i_5837 = (v_i_5837 + 1)){
                            v_user_func_14348_5902[(v_i_5837 + (2 * v_i_5836) + (8 * v_i_5835))] = arctanh_uf(v_initial_param_14344_5838[(v_i_5837 + (2 * v_i_5836) + (8 * v_i_5835))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}