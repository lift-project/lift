

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef FIX_UF_H
#define FIX_UF_H
; 
double fix_uf(double x){
    return trunc(x) ;; 
}

#endif
 ; 
void lift_fix_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14488_6110 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14492_6174 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16302 = 0;(v_tile_batch_16302 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16302)){
        int v_virtual_tile_id_16303 = (GPE_TILE_ID() + (v_tile_batch_16302 * 2));
        int v_i_6107 = v_virtual_tile_id_16303;
        if ((v_virtual_tile_id_16303 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16304 = 0;(v_gpe_batch_16304 <= 1); (++v_gpe_batch_16304)){
                    ; 
                    int v_i_6108 = GPEQ_POP();
                    if ((v_i_6108 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6109 = 0;(v_i_6109 < 2); v_i_6109 = (v_i_6109 + 1)){
                            v_user_func_14492_6174[(v_i_6109 + (2 * v_i_6108) + (8 * v_i_6107))] = fix_uf(v_initial_param_14488_6110[(v_i_6109 + (2 * v_i_6108) + (8 * v_i_6107))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}