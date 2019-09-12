

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
    double * v_initial_param_15021_6586 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15025_6650 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16379 = 0;(v_tile_batch_16379 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16379)){
        int v_virtual_tile_id_16380 = (GPE_TILE_ID() + (v_tile_batch_16379 * 2));
        int v_i_6583 = v_virtual_tile_id_16380;
        if ((v_virtual_tile_id_16380 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16381 = 0;(v_gpe_batch_16381 <= 1); (++v_gpe_batch_16381)){
                    ; 
                    int v_i_6584 = GPEQ_POP();
                    if ((v_i_6584 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6585 = 0;(v_i_6585 < 2); v_i_6585 = (v_i_6585 + 1)){
                            v_user_func_15025_6650[(v_i_6585 + (2 * v_i_6584) + (8 * v_i_6583))] = log_uf(v_initial_param_15021_6586[(v_i_6585 + (2 * v_i_6584) + (8 * v_i_6583))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}