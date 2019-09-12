

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
    double * v_initial_param_13683_4818 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13687_4882 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16093 = 0;(v_tile_batch_16093 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16093)){
        int v_virtual_tile_id_16094 = (GPE_TILE_ID() + (v_tile_batch_16093 * 2));
        int v_i_4815 = v_virtual_tile_id_16094;
        if ((v_virtual_tile_id_16094 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16095 = 0;(v_gpe_batch_16095 <= 1); (++v_gpe_batch_16095)){
                    ; 
                    int v_i_4816 = GPEQ_POP();
                    if ((v_i_4816 < 4)){
                        // For each element processed sequentially
                        for (int v_i_4817 = 0;(v_i_4817 < 2); v_i_4817 = (v_i_4817 + 1)){
                            v_user_func_13687_4882[(v_i_4817 + (2 * v_i_4816) + (8 * v_i_4815))] = sin_uf(v_initial_param_13683_4818[(v_i_4817 + (2 * v_i_4816) + (8 * v_i_4815))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}