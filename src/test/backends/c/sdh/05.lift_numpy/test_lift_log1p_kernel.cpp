

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef LOG1P_UF_H
#define LOG1P_UF_H
; 
double log1p_uf(double x){
    return log(1+x) ;; 
}

#endif
 ; 
void lift_log1p_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15165_6790 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15169_6854 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16412 = 0;(v_tile_batch_16412 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16412)){
        int v_virtual_tile_id_16413 = (GPE_TILE_ID() + (v_tile_batch_16412 * 2));
        int v_i_6787 = v_virtual_tile_id_16413;
        if ((v_virtual_tile_id_16413 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16414 = 0;(v_gpe_batch_16414 <= 1); (++v_gpe_batch_16414)){
                    ; 
                    int v_i_6788 = GPEQ_POP();
                    if ((v_i_6788 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6789 = 0;(v_i_6789 < 2); v_i_6789 = (v_i_6789 + 1)){
                            v_user_func_15169_6854[(v_i_6789 + (2 * v_i_6788) + (8 * v_i_6787))] = log1p_uf(v_initial_param_15165_6790[(v_i_6789 + (2 * v_i_6788) + (8 * v_i_6787))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}