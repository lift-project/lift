

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
    double * v_initial_param_15891_7470 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15895_7534 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16522 = 0;(v_tile_batch_16522 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16522)){
        int v_virtual_tile_id_16523 = (GPE_TILE_ID() + (v_tile_batch_16522 * 2));
        int v_i_7467 = v_virtual_tile_id_16523;
        if ((v_virtual_tile_id_16523 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16524 = 0;(v_gpe_batch_16524 <= 1); (++v_gpe_batch_16524)){
                    ; 
                    int v_i_7468 = GPEQ_POP();
                    if ((v_i_7468 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7469 = 0;(v_i_7469 < 2); v_i_7469 = (v_i_7469 + 1)){
                            v_user_func_15895_7534[(v_i_7469 + (2 * v_i_7468) + (8 * v_i_7467))] = absolute_uf(v_initial_param_15891_7470[(v_i_7469 + (2 * v_i_7468) + (8 * v_i_7467))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}