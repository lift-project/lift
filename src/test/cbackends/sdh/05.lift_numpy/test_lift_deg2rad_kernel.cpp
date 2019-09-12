

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
    double * v_initial_param_14056_5362 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_14060_5426 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16181 = 0;(v_tile_batch_16181 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16181)){
        int v_virtual_tile_id_16182 = (GPE_TILE_ID() + (v_tile_batch_16181 * 2));
        int v_i_5359 = v_virtual_tile_id_16182;
        if ((v_virtual_tile_id_16182 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16183 = 0;(v_gpe_batch_16183 <= 1); (++v_gpe_batch_16183)){
                    ; 
                    int v_i_5360 = GPEQ_POP();
                    if ((v_i_5360 < 4)){
                        // For each element processed sequentially
                        for (int v_i_5361 = 0;(v_i_5361 < 2); v_i_5361 = (v_i_5361 + 1)){
                            v_user_func_14060_5426[(v_i_5361 + (2 * v_i_5360) + (8 * v_i_5359))] = d2r_uf(v_initial_param_14056_5362[(v_i_5361 + (2 * v_i_5360) + (8 * v_i_5359))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}