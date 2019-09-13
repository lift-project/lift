

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
    double * v_initial_param_13827_5022 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13831_5086 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16126 = 0;(v_tile_batch_16126 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16126)){
        int v_virtual_tile_id_16127 = (GPE_TILE_ID() + (v_tile_batch_16126 * 2));
        int v_i_5019 = v_virtual_tile_id_16127;
        if ((v_virtual_tile_id_16127 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16128 = 0;(v_gpe_batch_16128 <= 1); (++v_gpe_batch_16128)){
                    ; 
                    int v_i_5020 = GPEQ_POP();
                    if ((v_i_5020 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5021 = 0;(v_i_5021 < 2); v_i_5021 = (v_i_5021 + 1)){
                            v_user_func_13831_5086[(v_i_5021 + (2 * v_i_5020) + (8 * v_i_5019))] = arcsin_uf(v_initial_param_13827_5022[(v_i_5021 + (2 * v_i_5020) + (8 * v_i_5019))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}