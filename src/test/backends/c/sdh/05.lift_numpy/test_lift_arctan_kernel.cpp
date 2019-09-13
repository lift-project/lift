

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
double arctan_uf(double x){
    { return atan(x); }; 
}

#endif
 ; 
void lift_arctan_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_13923_5158 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13927_5222 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16148 = 0;(v_tile_batch_16148 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16148)){
        int v_virtual_tile_id_16149 = (GPE_TILE_ID() + (v_tile_batch_16148 * 2));
        int v_i_5155 = v_virtual_tile_id_16149;
        if ((v_virtual_tile_id_16149 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16150 = 0;(v_gpe_batch_16150 <= 1); (++v_gpe_batch_16150)){
                    ; 
                    int v_i_5156 = GPEQ_POP();
                    if ((v_i_5156 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5157 = 0;(v_i_5157 < 2); v_i_5157 = (v_i_5157 + 1)){
                            v_user_func_13927_5222[(v_i_5157 + (2 * v_i_5156) + (8 * v_i_5155))] = arctan_uf(v_initial_param_13923_5158[(v_i_5157 + (2 * v_i_5156) + (8 * v_i_5155))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}