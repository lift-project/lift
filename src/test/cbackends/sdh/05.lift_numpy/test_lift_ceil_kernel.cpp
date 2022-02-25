

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef CEIL_UF_H
#define CEIL_UF_H
; 
double ceil_uf(double x){
    return ceil(x);; 
}

#endif
 ; 
void lift_ceil_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_973_1631 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_977_1695 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2747 = 0;(v_tile_batch_2747 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2747)){
        int v_virtual_tile_id_2748 = (GPE_TILE_ID() + (v_tile_batch_2747 * 2));
        int v_i_1628 = v_virtual_tile_id_2748;
        if ((v_virtual_tile_id_2748 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2749 = 0;(v_gpe_batch_2749 <= 1); (++v_gpe_batch_2749)){
                    ; 
                    int v_i_1629 = GPEQ_POP();
                    if ((v_i_1629 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1630 = 0;(v_i_1630 < 2); v_i_1630 = (v_i_1630 + 1)){
                            v_user_func_977_1695[(v_i_1630 + (2 * v_i_1629) + (8 * v_i_1628))] = ceil_uf(v_initial_param_973_1631[(v_i_1630 + (2 * v_i_1629) + (8 * v_i_1628))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}