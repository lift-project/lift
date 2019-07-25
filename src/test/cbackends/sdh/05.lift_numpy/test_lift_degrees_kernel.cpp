

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef R2D_UF_H
#define R2D_UF_H
; 
double r2d_uf(double x){
    { return x*180/M_PI; }; 
}

#endif
 ; 
void lift_degrees_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_397_611 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_401_675 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2582 = 0;(v_tile_batch_2582 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2582)){
        int v_virtual_tile_id_2583 = (GPE_TILE_ID() + (v_tile_batch_2582 * 2));
        int v_i_608 = v_virtual_tile_id_2583;
        if ((v_virtual_tile_id_2583 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2584 = 0;(v_gpe_batch_2584 <= 1); (++v_gpe_batch_2584)){
                    ; 
                    int v_i_609 = GPEQ_POP();
                    if ((v_i_609 < 4)){
                        // For each element processed sequentially
                        for (int v_i_610 = 0;(v_i_610 < 2); v_i_610 = (v_i_610 + 1)){
                            v_user_func_401_675[(v_i_610 + (2 * v_i_609) + (8 * v_i_608))] = r2d_uf(v_initial_param_397_611[(v_i_610 + (2 * v_i_609) + (8 * v_i_608))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}