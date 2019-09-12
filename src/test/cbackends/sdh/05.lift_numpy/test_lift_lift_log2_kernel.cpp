

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef LOG2_UF_H
#define LOG2_UF_H
; 
double log2_uf(double x){
    return log2(x) ;; 
}

#endif
 ; 
void lift_lift_log2_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15117_6722 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15121_6786 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16401 = 0;(v_tile_batch_16401 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16401)){
        int v_virtual_tile_id_16402 = (GPE_TILE_ID() + (v_tile_batch_16401 * 2));
        int v_i_6719 = v_virtual_tile_id_16402;
        if ((v_virtual_tile_id_16402 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16403 = 0;(v_gpe_batch_16403 <= 1); (++v_gpe_batch_16403)){
                    ; 
                    int v_i_6720 = GPEQ_POP();
                    if ((v_i_6720 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6721 = 0;(v_i_6721 < 2); v_i_6721 = (v_i_6721 + 1)){
                            v_user_func_15121_6786[(v_i_6721 + (2 * v_i_6720) + (8 * v_i_6719))] = log2_uf(v_initial_param_15117_6722[(v_i_6721 + (2 * v_i_6720) + (8 * v_i_6719))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}