

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
    double * v_initial_param_14392_5974 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14396_6038 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16280 = 0;(v_tile_batch_16280 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16280)){
        int v_virtual_tile_id_16281 = (GPE_TILE_ID() + (v_tile_batch_16280 * 2));
        int v_i_5971 = v_virtual_tile_id_16281;
        if ((v_virtual_tile_id_16281 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16282 = 0;(v_gpe_batch_16282 <= 1); (++v_gpe_batch_16282)){
                    ; 
                    int v_i_5972 = GPEQ_POP();
                    if ((v_i_5972 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5973 = 0;(v_i_5973 < 2); v_i_5973 = (v_i_5973 + 1)){
                            v_user_func_14396_6038[(v_i_5973 + (2 * v_i_5972) + (8 * v_i_5971))] = round_uf(v_initial_param_14392_5974[(v_i_5973 + (2 * v_i_5972) + (8 * v_i_5971))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}