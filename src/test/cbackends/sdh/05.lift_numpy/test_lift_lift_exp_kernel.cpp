

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef EXP_UF_H
#define EXP_UF_H
; 
double exp_uf(double x){
    return exp(x) ;; 
}

#endif
 ; 
void lift_lift_exp_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1300_1767 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1304_1831 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2769 = 0;(v_tile_batch_2769 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2769)){
        int v_virtual_tile_id_2770 = (GPE_TILE_ID() + (v_tile_batch_2769 * 2));
        int v_i_1764 = v_virtual_tile_id_2770;
        if ((v_virtual_tile_id_2770 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2771 = 0;(v_gpe_batch_2771 <= 1); (++v_gpe_batch_2771)){
                    ; 
                    int v_i_1765 = GPEQ_POP();
                    if ((v_i_1765 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1766 = 0;(v_i_1766 < 2); v_i_1766 = (v_i_1766 + 1)){
                            v_user_func_1304_1831[(v_i_1766 + (2 * v_i_1765) + (8 * v_i_1764))] = exp_uf(v_initial_param_1300_1767[(v_i_1766 + (2 * v_i_1765) + (8 * v_i_1764))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}