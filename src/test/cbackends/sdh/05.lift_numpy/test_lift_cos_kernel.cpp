

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef COS_UF_H
#define COS_UF_H
; 
double cos_uf(double x){
    { return cos(x); }; 
}

#endif
 ; 
void lift_cos_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_120_271 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_124_335 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2527 = 0;(v_tile_batch_2527 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2527)){
        int v_virtual_tile_id_2528 = (GPE_TILE_ID() + (v_tile_batch_2527 * 2));
        int v_i_268 = v_virtual_tile_id_2528;
        if ((v_virtual_tile_id_2528 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2529 = 0;(v_gpe_batch_2529 <= 1); (++v_gpe_batch_2529)){
                    ; 
                    int v_i_269 = GPEQ_POP();
                    if ((v_i_269 < 4)){
                        // For each element processed sequentially
                        for (int v_i_270 = 0;(v_i_270 < 2); v_i_270 = (v_i_270 + 1)){
                            v_user_func_124_335[(v_i_270 + (2 * v_i_269) + (8 * v_i_268))] = cos_uf(v_initial_param_120_271[(v_i_270 + (2 * v_i_269) + (8 * v_i_268))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}