

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
double arctanh_uf(double x){
    { return atanh(x); }; 
}

#endif
 ; 
void lift_arctanh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_733_1223 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_737_1287 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2681 = 0;(v_tile_batch_2681 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2681)){
        int v_virtual_tile_id_2682 = (GPE_TILE_ID() + (v_tile_batch_2681 * 2));
        int v_i_1220 = v_virtual_tile_id_2682;
        if ((v_virtual_tile_id_2682 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2683 = 0;(v_gpe_batch_2683 <= 1); (++v_gpe_batch_2683)){
                    ; 
                    int v_i_1221 = GPEQ_POP();
                    if ((v_i_1221 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1222 = 0;(v_i_1222 < 2); v_i_1222 = (v_i_1222 + 1)){
                            v_user_func_737_1287[(v_i_1222 + (2 * v_i_1221) + (8 * v_i_1220))] = arctanh_uf(v_initial_param_733_1223[(v_i_1222 + (2 * v_i_1221) + (8 * v_i_1220))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}