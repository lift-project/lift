

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef TAN_UF_H
#define TAN_UF_H
; 
double tan_uf(double x){
    { return tan(x); }; 
}

#endif
 ; 
void lift_tan_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_168_339 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_172_403 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2538 = 0;(v_tile_batch_2538 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2538)){
        int v_virtual_tile_id_2539 = (GPE_TILE_ID() + (v_tile_batch_2538 * 2));
        int v_i_336 = v_virtual_tile_id_2539;
        if ((v_virtual_tile_id_2539 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2540 = 0;(v_gpe_batch_2540 <= 1); (++v_gpe_batch_2540)){
                    ; 
                    int v_i_337 = GPEQ_POP();
                    if ((v_i_337 < 4)){
                        // For each element processed sequentially
                        for (int v_i_338 = 0;(v_i_338 < 2); v_i_338 = (v_i_338 + 1)){
                            v_user_func_172_403[(v_i_338 + (2 * v_i_337) + (8 * v_i_336))] = tan_uf(v_initial_param_168_339[(v_i_338 + (2 * v_i_337) + (8 * v_i_336))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}