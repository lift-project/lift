

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
void lift_radians_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_445_679 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_449_743 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2593 = 0;(v_tile_batch_2593 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2593)){
        int v_virtual_tile_id_2594 = (GPE_TILE_ID() + (v_tile_batch_2593 * 2));
        int v_i_676 = v_virtual_tile_id_2594;
        if ((v_virtual_tile_id_2594 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2595 = 0;(v_gpe_batch_2595 <= 1); (++v_gpe_batch_2595)){
                    ; 
                    int v_i_677 = GPEQ_POP();
                    if ((v_i_677 < 4)){
                        // For each element processed sequentially
                        for (int v_i_678 = 0;(v_i_678 < 2); v_i_678 = (v_i_678 + 1)){
                            v_user_func_449_743[(v_i_678 + (2 * v_i_677) + (8 * v_i_676))] = d2r_uf(v_initial_param_445_679[(v_i_678 + (2 * v_i_677) + (8 * v_i_676))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}