

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SIN_UF_H
#define SIN_UF_H
; 
double sin_uf(double x){
    { return sin(x); }; 
}

#endif
 ; 
void lift_lift_sin_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_72_202 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_76_266 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2516 = 0;(v_tile_batch_2516 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2516)){
        int v_virtual_tile_id_2517 = (GPE_TILE_ID() + (v_tile_batch_2516 * 2));
        int v_i_199 = v_virtual_tile_id_2517;
        if ((v_virtual_tile_id_2517 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2518 = 0;(v_gpe_batch_2518 <= 1); (++v_gpe_batch_2518)){
                    ; 
                    int v_i_200 = GPEQ_POP();
                    if ((v_i_200 < 4)){
                        // For each element processed sequentially
                        for (int v_i_201 = 0;(v_i_201 < 2); v_i_201 = (v_i_201 + 1)){
                            v_user_func_76_266[(v_i_201 + (2 * v_i_200) + (8 * v_i_199))] = sin_uf(v_initial_param_72_202[(v_i_201 + (2 * v_i_200) + (8 * v_i_199))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}