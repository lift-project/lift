

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SINH_UF_H
#define SINH_UF_H
; 
double sinh_uf(double x){
    { return sinh(x); }; 
}

#endif
 ; 
void lift_sinh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14104_5498 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14108_5562 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16203 = 0;(v_tile_batch_16203 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16203)){
        int v_virtual_tile_id_16204 = (GPE_TILE_ID() + (v_tile_batch_16203 * 2));
        int v_i_5495 = v_virtual_tile_id_16204;
        if ((v_virtual_tile_id_16204 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16205 = 0;(v_gpe_batch_16205 <= 1); (++v_gpe_batch_16205)){
                    ; 
                    int v_i_5496 = GPEQ_POP();
                    if ((v_i_5496 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5497 = 0;(v_i_5497 < 2); v_i_5497 = (v_i_5497 + 1)){
                            v_user_func_14108_5562[(v_i_5497 + (2 * v_i_5496) + (8 * v_i_5495))] = sinh_uf(v_initial_param_14104_5498[(v_i_5497 + (2 * v_i_5496) + (8 * v_i_5495))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}