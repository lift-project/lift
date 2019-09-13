

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SQUARE_UF_H
#define SQUARE_UF_H
; 
double square_uf(double x){
    { return x*x; }; 
}

#endif
 ; 
void lift_square_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15843_7334 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15847_7398 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16500 = 0;(v_tile_batch_16500 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16500)){
        int v_virtual_tile_id_16501 = (GPE_TILE_ID() + (v_tile_batch_16500 * 2));
        int v_i_7331 = v_virtual_tile_id_16501;
        if ((v_virtual_tile_id_16501 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16502 = 0;(v_gpe_batch_16502 <= 1); (++v_gpe_batch_16502)){
                    ; 
                    int v_i_7332 = GPEQ_POP();
                    if ((v_i_7332 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7333 = 0;(v_i_7333 < 2); v_i_7333 = (v_i_7333 + 1)){
                            v_user_func_15847_7398[(v_i_7333 + (2 * v_i_7332) + (8 * v_i_7331))] = square_uf(v_initial_param_15843_7334[(v_i_7333 + (2 * v_i_7332) + (8 * v_i_7331))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}