

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
    double * v_initial_param_829_1427 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_833_1491 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2714 = 0;(v_tile_batch_2714 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2714)){
        int v_virtual_tile_id_2715 = (GPE_TILE_ID() + (v_tile_batch_2714 * 2));
        int v_i_1424 = v_virtual_tile_id_2715;
        if ((v_virtual_tile_id_2715 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2716 = 0;(v_gpe_batch_2716 <= 1); (++v_gpe_batch_2716)){
                    ; 
                    int v_i_1425 = GPEQ_POP();
                    if ((v_i_1425 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1426 = 0;(v_i_1426 < 2); v_i_1426 = (v_i_1426 + 1)){
                            v_user_func_833_1491[(v_i_1426 + (2 * v_i_1425) + (8 * v_i_1424))] = rint_uf(v_initial_param_829_1427[(v_i_1426 + (2 * v_i_1425) + (8 * v_i_1424))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}