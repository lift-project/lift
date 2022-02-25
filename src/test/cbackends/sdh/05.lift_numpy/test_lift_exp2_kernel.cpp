

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef EXP2_UF_H
#define EXP2_UF_H
; 
double exp2_uf(double x){
    return pow(2,x) ;; 
}

#endif
 ; 
void lift_exp2_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1396_1903 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1400_1967 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2791 = 0;(v_tile_batch_2791 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2791)){
        int v_virtual_tile_id_2792 = (GPE_TILE_ID() + (v_tile_batch_2791 * 2));
        int v_i_1900 = v_virtual_tile_id_2792;
        if ((v_virtual_tile_id_2792 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2793 = 0;(v_gpe_batch_2793 <= 1); (++v_gpe_batch_2793)){
                    ; 
                    int v_i_1901 = GPEQ_POP();
                    if ((v_i_1901 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1902 = 0;(v_i_1902 < 2); v_i_1902 = (v_i_1902 + 1)){
                            v_user_func_1400_1967[(v_i_1902 + (2 * v_i_1901) + (8 * v_i_1900))] = exp2_uf(v_initial_param_1396_1903[(v_i_1902 + (2 * v_i_1901) + (8 * v_i_1900))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}