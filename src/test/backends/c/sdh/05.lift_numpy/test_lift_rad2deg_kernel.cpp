

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
    double * v_initial_param_14008_5430 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14012_5494 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16192 = 0;(v_tile_batch_16192 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16192)){
        int v_virtual_tile_id_16193 = (GPE_TILE_ID() + (v_tile_batch_16192 * 2));
        int v_i_5427 = v_virtual_tile_id_16193;
        if ((v_virtual_tile_id_16193 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16194 = 0;(v_gpe_batch_16194 <= 1); (++v_gpe_batch_16194)){
                    ; 
                    int v_i_5428 = GPEQ_POP();
                    if ((v_i_5428 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5429 = 0;(v_i_5429 < 2); v_i_5429 = (v_i_5429 + 1)){
                            v_user_func_14012_5494[(v_i_5429 + (2 * v_i_5428) + (8 * v_i_5427))] = r2d_uf(v_initial_param_14008_5430[(v_i_5429 + (2 * v_i_5428) + (8 * v_i_5427))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}