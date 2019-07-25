

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
double arctan_uf(double x){
    { return atan(x); }; 
}

#endif
 ; 
void lift_arctan_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_312_543 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_316_607 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2571 = 0;(v_tile_batch_2571 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2571)){
        int v_virtual_tile_id_2572 = (GPE_TILE_ID() + (v_tile_batch_2571 * 2));
        int v_i_540 = v_virtual_tile_id_2572;
        if ((v_virtual_tile_id_2572 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2573 = 0;(v_gpe_batch_2573 <= 1); (++v_gpe_batch_2573)){
                    ; 
                    int v_i_541 = GPEQ_POP();
                    if ((v_i_541 < 4)){
                        // For each element processed sequentially
                        for (int v_i_542 = 0;(v_i_542 < 2); v_i_542 = (v_i_542 + 1)){
                            v_user_func_316_607[(v_i_542 + (2 * v_i_541) + (8 * v_i_540))] = arctan_uf(v_initial_param_312_543[(v_i_542 + (2 * v_i_541) + (8 * v_i_540))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}