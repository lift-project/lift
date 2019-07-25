

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef LOG2_UF_H
#define LOG2_UF_H
; 
double log2_uf(double x){
    return log2(x) ;; 
}

#endif
 ; 
void lift_lift_log2_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1540_2107 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1544_2171 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2824 = 0;(v_tile_batch_2824 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2824)){
        int v_virtual_tile_id_2825 = (GPE_TILE_ID() + (v_tile_batch_2824 * 2));
        int v_i_2104 = v_virtual_tile_id_2825;
        if ((v_virtual_tile_id_2825 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2826 = 0;(v_gpe_batch_2826 <= 1); (++v_gpe_batch_2826)){
                    ; 
                    int v_i_2105 = GPEQ_POP();
                    if ((v_i_2105 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2106 = 0;(v_i_2106 < 2); v_i_2106 = (v_i_2106 + 1)){
                            v_user_func_1544_2171[(v_i_2106 + (2 * v_i_2105) + (8 * v_i_2104))] = log2_uf(v_initial_param_1540_2107[(v_i_2106 + (2 * v_i_2105) + (8 * v_i_2104))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}