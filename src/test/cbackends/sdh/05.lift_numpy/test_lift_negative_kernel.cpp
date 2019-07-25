

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
double negative_uf(double x){
    return (-1.0f)*x; 
}

#endif
 ; 
void lift_negative_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1919_2515 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1923_2579 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2890 = 0;(v_tile_batch_2890 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2890)){
        int v_virtual_tile_id_2891 = (GPE_TILE_ID() + (v_tile_batch_2890 * 2));
        int v_i_2512 = v_virtual_tile_id_2891;
        if ((v_virtual_tile_id_2891 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2892 = 0;(v_gpe_batch_2892 <= 1); (++v_gpe_batch_2892)){
                    ; 
                    int v_i_2513 = GPEQ_POP();
                    if ((v_i_2513 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2514 = 0;(v_i_2514 < 2); v_i_2514 = (v_i_2514 + 1)){
                            v_user_func_1923_2579[(v_i_2514 + (2 * v_i_2513) + (8 * v_i_2512))] = negative_uf(v_initial_param_1919_2515[(v_i_2514 + (2 * v_i_2513) + (8 * v_i_2512))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}