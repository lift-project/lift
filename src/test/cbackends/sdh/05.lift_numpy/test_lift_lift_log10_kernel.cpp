

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef LOG10_UF_H
#define LOG10_UF_H
; 
double log10_uf(double x){
    return log10(x) ;; 
}

#endif
 ; 
void lift_lift_log10_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1492_2039 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1496_2103 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2813 = 0;(v_tile_batch_2813 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2813)){
        int v_virtual_tile_id_2814 = (GPE_TILE_ID() + (v_tile_batch_2813 * 2));
        int v_i_2036 = v_virtual_tile_id_2814;
        if ((v_virtual_tile_id_2814 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2815 = 0;(v_gpe_batch_2815 <= 1); (++v_gpe_batch_2815)){
                    ; 
                    int v_i_2037 = GPEQ_POP();
                    if ((v_i_2037 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2038 = 0;(v_i_2038 < 2); v_i_2038 = (v_i_2038 + 1)){
                            v_user_func_1496_2103[(v_i_2038 + (2 * v_i_2037) + (8 * v_i_2036))] = log10_uf(v_initial_param_1492_2039[(v_i_2038 + (2 * v_i_2037) + (8 * v_i_2036))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}