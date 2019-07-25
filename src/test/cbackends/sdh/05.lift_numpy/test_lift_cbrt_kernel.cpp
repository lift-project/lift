

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef CBRT_UF_H
#define CBRT_UF_H
; 
double cbrt_uf(double x){
    { return cbrt(x); }; 
}

#endif
 ; 
void lift_cbrt_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_2218_2651 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_2222_2715 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2912 = 0;(v_tile_batch_2912 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2912)){
        int v_virtual_tile_id_2913 = (GPE_TILE_ID() + (v_tile_batch_2912 * 2));
        int v_i_2648 = v_virtual_tile_id_2913;
        if ((v_virtual_tile_id_2913 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2914 = 0;(v_gpe_batch_2914 <= 1); (++v_gpe_batch_2914)){
                    ; 
                    int v_i_2649 = GPEQ_POP();
                    if ((v_i_2649 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2650 = 0;(v_i_2650 < 2); v_i_2650 = (v_i_2650 + 1)){
                            v_user_func_2222_2715[(v_i_2650 + (2 * v_i_2649) + (8 * v_i_2648))] = cbrt_uf(v_initial_param_2218_2651[(v_i_2650 + (2 * v_i_2649) + (8 * v_i_2648))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}