

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
    double * v_initial_param_14536_6178 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14540_6242 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16313 = 0;(v_tile_batch_16313 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16313)){
        int v_virtual_tile_id_16314 = (GPE_TILE_ID() + (v_tile_batch_16313 * 2));
        int v_i_6175 = v_virtual_tile_id_16314;
        if ((v_virtual_tile_id_16314 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16315 = 0;(v_gpe_batch_16315 <= 1); (++v_gpe_batch_16315)){
                    ; 
                    int v_i_6176 = GPEQ_POP();
                    if ((v_i_6176 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6177 = 0;(v_i_6177 < 2); v_i_6177 = (v_i_6177 + 1)){
                            v_user_func_14540_6242[(v_i_6177 + (2 * v_i_6176) + (8 * v_i_6175))] = floor_uf(v_initial_param_14536_6178[(v_i_6177 + (2 * v_i_6176) + (8 * v_i_6175))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}