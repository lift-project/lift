

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
void lift_rad2deg_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_397_815 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_401_879 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2615 = 0;(v_tile_batch_2615 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2615)){
        int v_virtual_tile_id_2616 = (GPE_TILE_ID() + (v_tile_batch_2615 * 2));
        int v_i_812 = v_virtual_tile_id_2616;
        if ((v_virtual_tile_id_2616 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2617 = 0;(v_gpe_batch_2617 <= 1); (++v_gpe_batch_2617)){
                    ; 
                    int v_i_813 = GPEQ_POP();
                    if ((v_i_813 < 4)){
                        // For each element processed sequentially
                        for (int v_i_814 = 0;(v_i_814 < 2); v_i_814 = (v_i_814 + 1)){
                            v_user_func_401_879[(v_i_814 + (2 * v_i_813) + (8 * v_i_812))] = r2d_uf(v_initial_param_397_815[(v_i_814 + (2 * v_i_813) + (8 * v_i_812))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}