

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
    double * v_initial_param_13875_5090 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13879_5154 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16137 = 0;(v_tile_batch_16137 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16137)){
        int v_virtual_tile_id_16138 = (GPE_TILE_ID() + (v_tile_batch_16137 * 2));
        int v_i_5087 = v_virtual_tile_id_16138;
        if ((v_virtual_tile_id_16138 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16139 = 0;(v_gpe_batch_16139 <= 1); (++v_gpe_batch_16139)){
                    ; 
                    int v_i_5088 = GPEQ_POP();
                    if ((v_i_5088 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5089 = 0;(v_i_5089 < 2); v_i_5089 = (v_i_5089 + 1)){
                            v_user_func_13879_5154[(v_i_5089 + (2 * v_i_5088) + (8 * v_i_5087))] = arccos_uf(v_initial_param_13875_5090[(v_i_5089 + (2 * v_i_5088) + (8 * v_i_5087))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}