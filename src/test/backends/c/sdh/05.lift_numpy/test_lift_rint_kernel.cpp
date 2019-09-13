

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef RINT_UF_H
#define RINT_UF_H
; 
double rint_uf(double x){
    return round(x) ;; 
}

#endif
 ; 
void lift_rint_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_14440_6042 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14444_6106 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16291 = 0;(v_tile_batch_16291 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16291)){
        int v_virtual_tile_id_16292 = (GPE_TILE_ID() + (v_tile_batch_16291 * 2));
        int v_i_6039 = v_virtual_tile_id_16292;
        if ((v_virtual_tile_id_16292 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16293 = 0;(v_gpe_batch_16293 <= 1); (++v_gpe_batch_16293)){
                    ; 
                    int v_i_6040 = GPEQ_POP();
                    if ((v_i_6040 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6041 = 0;(v_i_6041 < 2); v_i_6041 = (v_i_6041 + 1)){
                            v_user_func_14444_6106[(v_i_6041 + (2 * v_i_6040) + (8 * v_i_6039))] = rint_uf(v_initial_param_14440_6042[(v_i_6041 + (2 * v_i_6040) + (8 * v_i_6039))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}