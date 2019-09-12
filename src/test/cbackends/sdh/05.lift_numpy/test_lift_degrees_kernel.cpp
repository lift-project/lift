

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
    double * v_initial_param_14008_5226 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14012_5290 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16159 = 0;(v_tile_batch_16159 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16159)){
        int v_virtual_tile_id_16160 = (GPE_TILE_ID() + (v_tile_batch_16159 * 2));
        int v_i_5223 = v_virtual_tile_id_16160;
        if ((v_virtual_tile_id_16160 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16161 = 0;(v_gpe_batch_16161 <= 1); (++v_gpe_batch_16161)){
                    ; 
                    int v_i_5224 = GPEQ_POP();
                    if ((v_i_5224 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5225 = 0;(v_i_5225 < 2); v_i_5225 = (v_i_5225 + 1)){
                            v_user_func_14012_5290[(v_i_5225 + (2 * v_i_5224) + (8 * v_i_5223))] = r2d_uf(v_initial_param_14008_5226[(v_i_5225 + (2 * v_i_5224) + (8 * v_i_5223))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}