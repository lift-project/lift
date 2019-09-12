

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ID_DOUBLE_UF_H
#define ID_DOUBLE_UF_H
; 
double id_double_uf(double x){
    { return x; }; 
}

#endif
 ; 
void lift_positive_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15448_7062 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15452_7126 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16456 = 0;(v_tile_batch_16456 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16456)){
        int v_virtual_tile_id_16457 = (GPE_TILE_ID() + (v_tile_batch_16456 * 2));
        int v_i_7059 = v_virtual_tile_id_16457;
        if ((v_virtual_tile_id_16457 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16458 = 0;(v_gpe_batch_16458 <= 1); (++v_gpe_batch_16458)){
                    ; 
                    int v_i_7060 = GPEQ_POP();
                    if ((v_i_7060 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7061 = 0;(v_i_7061 < 2); v_i_7061 = (v_i_7061 + 1)){
                            v_user_func_15452_7126[(v_i_7061 + (2 * v_i_7060) + (8 * v_i_7059))] = id_double_uf(v_initial_param_15448_7062[(v_i_7061 + (2 * v_i_7060) + (8 * v_i_7059))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}