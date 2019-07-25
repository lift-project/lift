

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H
; 
double arcsin_uf(double x){
    { return asin(x); }; 
}

#endif
 ; 
void lift_arcsin_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_216_407 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_220_471 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2549 = 0;(v_tile_batch_2549 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2549)){
        int v_virtual_tile_id_2550 = (GPE_TILE_ID() + (v_tile_batch_2549 * 2));
        int v_i_404 = v_virtual_tile_id_2550;
        if ((v_virtual_tile_id_2550 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2551 = 0;(v_gpe_batch_2551 <= 1); (++v_gpe_batch_2551)){
                    ; 
                    int v_i_405 = GPEQ_POP();
                    if ((v_i_405 < 4)){
                        // For each element processed sequentially
                        for (int v_i_406 = 0;(v_i_406 < 2); v_i_406 = (v_i_406 + 1)){
                            v_user_func_220_471[(v_i_406 + (2 * v_i_405) + (8 * v_i_404))] = arcsin_uf(v_initial_param_216_407[(v_i_406 + (2 * v_i_405) + (8 * v_i_404))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}