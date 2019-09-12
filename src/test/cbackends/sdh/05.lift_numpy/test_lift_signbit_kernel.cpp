

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SIGNBIT_UF_H
#define SIGNBIT_UF_H
; 
double signbit_uf(double x){
    return x<0? 1:0 ;; 
}

#endif
 ; 
void lift_signbit_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15303_6926 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15307_6990 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16434 = 0;(v_tile_batch_16434 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16434)){
        int v_virtual_tile_id_16435 = (GPE_TILE_ID() + (v_tile_batch_16434 * 2));
        int v_i_6923 = v_virtual_tile_id_16435;
        if ((v_virtual_tile_id_16435 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16436 = 0;(v_gpe_batch_16436 <= 1); (++v_gpe_batch_16436)){
                    ; 
                    int v_i_6924 = GPEQ_POP();
                    if ((v_i_6924 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6925 = 0;(v_i_6925 < 2); v_i_6925 = (v_i_6925 + 1)){
                            v_user_func_15307_6990[(v_i_6925 + (2 * v_i_6924) + (8 * v_i_6923))] = signbit_uf(v_initial_param_15303_6926[(v_i_6925 + (2 * v_i_6924) + (8 * v_i_6923))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}