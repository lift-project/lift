

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef LOG_UF_H
#define LOG_UF_H
; 
double log_uf(double x){
    return log(x) ;; 
}

#endif
 ; 
void lift_lift_log_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1444_1971 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1448_2035 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2802 = 0;(v_tile_batch_2802 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2802)){
        int v_virtual_tile_id_2803 = (GPE_TILE_ID() + (v_tile_batch_2802 * 2));
        int v_i_1968 = v_virtual_tile_id_2803;
        if ((v_virtual_tile_id_2803 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2804 = 0;(v_gpe_batch_2804 <= 1); (++v_gpe_batch_2804)){
                    ; 
                    int v_i_1969 = GPEQ_POP();
                    if ((v_i_1969 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1970 = 0;(v_i_1970 < 2); v_i_1970 = (v_i_1970 + 1)){
                            v_user_func_1448_2035[(v_i_1970 + (2 * v_i_1969) + (8 * v_i_1968))] = log_uf(v_initial_param_1444_1971[(v_i_1970 + (2 * v_i_1969) + (8 * v_i_1968))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}