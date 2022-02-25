

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SQUARE_UF_H
#define SQUARE_UF_H
; 
double square_uf(double x){
    { return x*x; }; 
}

#endif
 ; 
void lift_square_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_2266_2719 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_2270_2783 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2923 = 0;(v_tile_batch_2923 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2923)){
        int v_virtual_tile_id_2924 = (GPE_TILE_ID() + (v_tile_batch_2923 * 2));
        int v_i_2716 = v_virtual_tile_id_2924;
        if ((v_virtual_tile_id_2924 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2925 = 0;(v_gpe_batch_2925 <= 1); (++v_gpe_batch_2925)){
                    ; 
                    int v_i_2717 = GPEQ_POP();
                    if ((v_i_2717 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2718 = 0;(v_i_2718 < 2); v_i_2718 = (v_i_2718 + 1)){
                            v_user_func_2270_2783[(v_i_2718 + (2 * v_i_2717) + (8 * v_i_2716))] = square_uf(v_initial_param_2266_2719[(v_i_2718 + (2 * v_i_2717) + (8 * v_i_2716))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}