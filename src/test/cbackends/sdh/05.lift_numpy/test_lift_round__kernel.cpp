

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
void lift_round__kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_781_1359 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_785_1423 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2703 = 0;(v_tile_batch_2703 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2703)){
        int v_virtual_tile_id_2704 = (GPE_TILE_ID() + (v_tile_batch_2703 * 2));
        int v_i_1356 = v_virtual_tile_id_2704;
        if ((v_virtual_tile_id_2704 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2705 = 0;(v_gpe_batch_2705 <= 1); (++v_gpe_batch_2705)){
                    ; 
                    int v_i_1357 = GPEQ_POP();
                    if ((v_i_1357 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1358 = 0;(v_i_1358 < 2); v_i_1358 = (v_i_1358 + 1)){
                            v_user_func_785_1423[(v_i_1358 + (2 * v_i_1357) + (8 * v_i_1356))] = round_uf(v_initial_param_781_1359[(v_i_1358 + (2 * v_i_1357) + (8 * v_i_1356))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}