

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
    double * v_initial_param_1588_2175 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1592_2239 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2835 = 0;(v_tile_batch_2835 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2835)){
        int v_virtual_tile_id_2836 = (GPE_TILE_ID() + (v_tile_batch_2835 * 2));
        int v_i_2172 = v_virtual_tile_id_2836;
        if ((v_virtual_tile_id_2836 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2837 = 0;(v_gpe_batch_2837 <= 1); (++v_gpe_batch_2837)){
                    ; 
                    int v_i_2173 = GPEQ_POP();
                    if ((v_i_2173 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2174 = 0;(v_i_2174 < 2); v_i_2174 = (v_i_2174 + 1)){
                            v_user_func_1592_2239[(v_i_2174 + (2 * v_i_2173) + (8 * v_i_2172))] = log1p_uf(v_initial_param_1588_2175[(v_i_2174 + (2 * v_i_2173) + (8 * v_i_2172))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}