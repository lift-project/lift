

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef D2R_UF_H
#define D2R_UF_H
; 
double d2r_uf(double x){
    { return x*M_PI/180; }; 
}

#endif
 ; 
void lift_deg2rad_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_445_747 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_449_811 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2604 = 0;(v_tile_batch_2604 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2604)){
        int v_virtual_tile_id_2605 = (GPE_TILE_ID() + (v_tile_batch_2604 * 2));
        int v_i_744 = v_virtual_tile_id_2605;
        if ((v_virtual_tile_id_2605 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2606 = 0;(v_gpe_batch_2606 <= 1); (++v_gpe_batch_2606)){
                    ; 
                    int v_i_745 = GPEQ_POP();
                    if ((v_i_745 < 4)){
                        // For each element processed sequentially
                        for (int v_i_746 = 0;(v_i_746 < 2); v_i_746 = (v_i_746 + 1)){
                            v_user_func_449_811[(v_i_746 + (2 * v_i_745) + (8 * v_i_744))] = d2r_uf(v_initial_param_445_747[(v_i_746 + (2 * v_i_745) + (8 * v_i_744))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}