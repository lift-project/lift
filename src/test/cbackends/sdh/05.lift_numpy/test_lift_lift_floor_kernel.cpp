

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
double floor_uf(double x){
    return floor(x);; 
}

#endif
 ; 
void lift_lift_floor_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_925_1563 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_929_1627 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2736 = 0;(v_tile_batch_2736 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2736)){
        int v_virtual_tile_id_2737 = (GPE_TILE_ID() + (v_tile_batch_2736 * 2));
        int v_i_1560 = v_virtual_tile_id_2737;
        if ((v_virtual_tile_id_2737 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2738 = 0;(v_gpe_batch_2738 <= 1); (++v_gpe_batch_2738)){
                    ; 
                    int v_i_1561 = GPEQ_POP();
                    if ((v_i_1561 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1562 = 0;(v_i_1562 < 2); v_i_1562 = (v_i_1562 + 1)){
                            v_user_func_929_1627[(v_i_1562 + (2 * v_i_1561) + (8 * v_i_1560))] = floor_uf(v_initial_param_925_1563[(v_i_1562 + (2 * v_i_1561) + (8 * v_i_1560))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}