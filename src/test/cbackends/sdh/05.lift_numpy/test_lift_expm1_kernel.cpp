

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef EXPM1_UF_H
#define EXPM1_UF_H
; 
double expm1_uf(double x){
    return exp(x) - 1 ;; 
}

#endif
 ; 
void lift_expm1_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1348_1835 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1352_1899 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2780 = 0;(v_tile_batch_2780 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2780)){
        int v_virtual_tile_id_2781 = (GPE_TILE_ID() + (v_tile_batch_2780 * 2));
        int v_i_1832 = v_virtual_tile_id_2781;
        if ((v_virtual_tile_id_2781 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2782 = 0;(v_gpe_batch_2782 <= 1); (++v_gpe_batch_2782)){
                    ; 
                    int v_i_1833 = GPEQ_POP();
                    if ((v_i_1833 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1834 = 0;(v_i_1834 < 2); v_i_1834 = (v_i_1834 + 1)){
                            v_user_func_1352_1899[(v_i_1834 + (2 * v_i_1833) + (8 * v_i_1832))] = expm1_uf(v_initial_param_1348_1835[(v_i_1834 + (2 * v_i_1833) + (8 * v_i_1832))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}