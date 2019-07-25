

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
double arccos_uf(double x){
    { return acos(x); }; 
}

#endif
 ; 
void lift_arccos_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_264_475 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_268_539 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2560 = 0;(v_tile_batch_2560 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2560)){
        int v_virtual_tile_id_2561 = (GPE_TILE_ID() + (v_tile_batch_2560 * 2));
        int v_i_472 = v_virtual_tile_id_2561;
        if ((v_virtual_tile_id_2561 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2562 = 0;(v_gpe_batch_2562 <= 1); (++v_gpe_batch_2562)){
                    ; 
                    int v_i_473 = GPEQ_POP();
                    if ((v_i_473 < 4)){
                        // For each element processed sequentially
                        for (int v_i_474 = 0;(v_i_474 < 2); v_i_474 = (v_i_474 + 1)){
                            v_user_func_268_539[(v_i_474 + (2 * v_i_473) + (8 * v_i_472))] = arccos_uf(v_initial_param_264_475[(v_i_474 + (2 * v_i_473) + (8 * v_i_472))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}