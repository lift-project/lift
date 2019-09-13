

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ROUND_UF_H
#define ROUND_UF_H
; 
double round_uf(double x){
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;; 
}

#endif
 ; 
void lift_around_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14392_5906 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14396_5970 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16269 = 0;(v_tile_batch_16269 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16269)){
        int v_virtual_tile_id_16270 = (GPE_TILE_ID() + (v_tile_batch_16269 * 2));
        int v_i_5903 = v_virtual_tile_id_16270;
        if ((v_virtual_tile_id_16270 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16271 = 0;(v_gpe_batch_16271 <= 1); (++v_gpe_batch_16271)){
                    ; 
                    int v_i_5904 = GPEQ_POP();
                    if ((v_i_5904 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5905 = 0;(v_i_5905 < 2); v_i_5905 = (v_i_5905 + 1)){
                            v_user_func_14396_5970[(v_i_5905 + (2 * v_i_5904) + (8 * v_i_5903))] = round_uf(v_initial_param_14392_5906[(v_i_5905 + (2 * v_i_5904) + (8 * v_i_5903))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}