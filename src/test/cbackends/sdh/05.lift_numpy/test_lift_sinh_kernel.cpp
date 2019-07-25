

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SINH_UF_H
#define SINH_UF_H
; 
double sinh_uf(double x){
    { return sinh(x); }; 
}

#endif
 ; 
void lift_sinh_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_493_883 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_497_947 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2626 = 0;(v_tile_batch_2626 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2626)){
        int v_virtual_tile_id_2627 = (GPE_TILE_ID() + (v_tile_batch_2626 * 2));
        int v_i_880 = v_virtual_tile_id_2627;
        if ((v_virtual_tile_id_2627 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2628 = 0;(v_gpe_batch_2628 <= 1); (++v_gpe_batch_2628)){
                    ; 
                    int v_i_881 = GPEQ_POP();
                    if ((v_i_881 < 4)){
                        // For each element processed sequentially
                        for (int v_i_882 = 0;(v_i_882 < 2); v_i_882 = (v_i_882 + 1)){
                            v_user_func_497_947[(v_i_882 + (2 * v_i_881) + (8 * v_i_880))] = sinh_uf(v_initial_param_493_883[(v_i_882 + (2 * v_i_881) + (8 * v_i_880))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}