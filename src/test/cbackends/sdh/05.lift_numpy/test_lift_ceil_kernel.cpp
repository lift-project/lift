

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef CEIL_UF_H
#define CEIL_UF_H
; 
double ceil_uf(double x){
    return ceil(x);; 
}

#endif
 ; 
void lift_ceil_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14584_6246 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14588_6310 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16324 = 0;(v_tile_batch_16324 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16324)){
        int v_virtual_tile_id_16325 = (GPE_TILE_ID() + (v_tile_batch_16324 * 2));
        int v_i_6243 = v_virtual_tile_id_16325;
        if ((v_virtual_tile_id_16325 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16326 = 0;(v_gpe_batch_16326 <= 1); (++v_gpe_batch_16326)){
                    ; 
                    int v_i_6244 = GPEQ_POP();
                    if ((v_i_6244 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6245 = 0;(v_i_6245 < 2); v_i_6245 = (v_i_6245 + 1)){
                            v_user_func_14588_6310[(v_i_6245 + (2 * v_i_6244) + (8 * v_i_6243))] = ceil_uf(v_initial_param_14584_6246[(v_i_6245 + (2 * v_i_6244) + (8 * v_i_6243))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}