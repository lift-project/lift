

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SINC_UF_H
#define SINC_UF_H
; 
double sinc_uf(double x){
    return sin(M_PI*x)/(M_PI*x) ;; 
}

#endif
 ; 
void lift_sinc_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1678_2243 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1682_2307 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2846 = 0;(v_tile_batch_2846 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2846)){
        int v_virtual_tile_id_2847 = (GPE_TILE_ID() + (v_tile_batch_2846 * 2));
        int v_i_2240 = v_virtual_tile_id_2847;
        if ((v_virtual_tile_id_2847 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2848 = 0;(v_gpe_batch_2848 <= 1); (++v_gpe_batch_2848)){
                    ; 
                    int v_i_2241 = GPEQ_POP();
                    if ((v_i_2241 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2242 = 0;(v_i_2242 < 2); v_i_2242 = (v_i_2242 + 1)){
                            v_user_func_1682_2307[(v_i_2242 + (2 * v_i_2241) + (8 * v_i_2240))] = sinc_uf(v_initial_param_1678_2243[(v_i_2242 + (2 * v_i_2241) + (8 * v_i_2240))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}