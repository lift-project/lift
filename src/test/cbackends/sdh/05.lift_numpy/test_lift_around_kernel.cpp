

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ROUND_UF_H
#define ROUND_UF_H
; 
double round_uf(double x){
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;; 
}

#endif
 ; 
void lift_around_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_781_1291 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_785_1355 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2692 = 0;(v_tile_batch_2692 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2692)){
        int v_virtual_tile_id_2693 = (GPE_TILE_ID() + (v_tile_batch_2692 * 2));
        int v_i_1288 = v_virtual_tile_id_2693;
        if ((v_virtual_tile_id_2693 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2694 = 0;(v_gpe_batch_2694 <= 1); (++v_gpe_batch_2694)){
                    ; 
                    int v_i_1289 = GPEQ_POP();
                    if ((v_i_1289 < 4)){
                        // For each element processed sequentially
                        for (int v_i_1290 = 0;(v_i_1290 < 2); v_i_1290 = (v_i_1290 + 1)){
                            v_user_func_785_1355[(v_i_1290 + (2 * v_i_1289) + (8 * v_i_1288))] = round_uf(v_initial_param_781_1291[(v_i_1290 + (2 * v_i_1289) + (8 * v_i_1288))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}