

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ADD_H
#define ADD_H
; 
double add(double l, double r){
    { return (l + r); }; 
}

#endif
 ; 
void lift_matrixmul_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_72_15 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_75_280 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4 = GPEQ_POP();
    for (int v_tile_batch_210 = 0;(v_tile_batch_210 <= (((v_N_4)/((4 * (v_N_4 / 8)))) / 2)); (++v_tile_batch_210)){
        int v_virtual_tile_id_211 = (GPE_TILE_ID() + (v_tile_batch_210 * 2));
        int v_i_11 = v_virtual_tile_id_211;
        if ((v_virtual_tile_id_211 < ((v_N_4)/((4 * (v_N_4 / 8)))))){
            {
                for (int v_gpe_batch_212 = 0;(v_gpe_batch_212 <= 1); (++v_gpe_batch_212)){
                    ; 
                    int v_i_12 = GPEQ_POP();
                    if ((v_i_12 < 4)){
                        // For each element reduced sequentially
                        v_user_func_138_276[(v_i_12 + (4 * v_i_11))] = 0.0; 
                        for (int v_i_13 = 0;(v_i_13 <= (-1 + (v_N_4 / 8))); (++v_i_13)){
                            v_user_func_138_276[(v_i_12 + (4 * v_i_11))] = add(v_user_func_138_276[(v_i_12 + (4 * v_i_11))], v_initial_param_72_15[(v_i_13 + (4 * v_i_11 * (v_N_4 / 8)) + (v_i_12 * (v_N_4 / 8)))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}