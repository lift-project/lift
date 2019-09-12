

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef EXP2_UF_H
#define EXP2_UF_H
; 
double exp2_uf(double x){
    return pow(2,x) ;; 
}

#endif
 ; 
void lift_exp2_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14973_6518 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14977_6582 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16368 = 0;(v_tile_batch_16368 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16368)){
        int v_virtual_tile_id_16369 = (GPE_TILE_ID() + (v_tile_batch_16368 * 2));
        int v_i_6515 = v_virtual_tile_id_16369;
        if ((v_virtual_tile_id_16369 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16370 = 0;(v_gpe_batch_16370 <= 1); (++v_gpe_batch_16370)){
                    ; 
                    int v_i_6516 = GPEQ_POP();
                    if ((v_i_6516 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6517 = 0;(v_i_6517 < 2); v_i_6517 = (v_i_6517 + 1)){
                            v_user_func_14977_6582[(v_i_6517 + (2 * v_i_6516) + (8 * v_i_6515))] = exp2_uf(v_initial_param_14973_6518[(v_i_6517 + (2 * v_i_6516) + (8 * v_i_6515))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}