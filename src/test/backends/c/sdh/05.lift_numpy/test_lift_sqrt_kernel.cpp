

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SQRT_UF_H
#define SQRT_UF_H
; 
double sqrt_uf(double x){
    { return sqrt(x); }; 
}

#endif
 ; 
void lift_sqrt_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15747_7198 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15751_7262 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16478 = 0;(v_tile_batch_16478 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16478)){
        int v_virtual_tile_id_16479 = (GPE_TILE_ID() + (v_tile_batch_16478 * 2));
        int v_i_7195 = v_virtual_tile_id_16479;
        if ((v_virtual_tile_id_16479 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16480 = 0;(v_gpe_batch_16480 <= 1); (++v_gpe_batch_16480)){
                    ; 
                    int v_i_7196 = GPEQ_POP();
                    if ((v_i_7196 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7197 = 0;(v_i_7197 < 2); v_i_7197 = (v_i_7197 + 1)){
                            v_user_func_15751_7262[(v_i_7197 + (2 * v_i_7196) + (8 * v_i_7195))] = sqrt_uf(v_initial_param_15747_7198[(v_i_7197 + (2 * v_i_7196) + (8 * v_i_7195))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}