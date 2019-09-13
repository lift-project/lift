

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SIGN_UF_H
#define SIGN_UF_H
; 
double sign_uf(double x){
    { return x==0? 0: ( x< 0 ? -1 : 1 ); }; 
}

#endif
 ; 
void lift_sign_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15939_7538 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15943_7602 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16533 = 0;(v_tile_batch_16533 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16533)){
        int v_virtual_tile_id_16534 = (GPE_TILE_ID() + (v_tile_batch_16533 * 2));
        int v_i_7535 = v_virtual_tile_id_16534;
        if ((v_virtual_tile_id_16534 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16535 = 0;(v_gpe_batch_16535 <= 1); (++v_gpe_batch_16535)){
                    ; 
                    int v_i_7536 = GPEQ_POP();
                    if ((v_i_7536 < 4)){
                        // For each element processed sequentially
                        for (int v_i_7537 = 0;(v_i_7537 < 2); v_i_7537 = (v_i_7537 + 1)){
                            v_user_func_15943_7602[(v_i_7537 + (2 * v_i_7536) + (8 * v_i_7535))] = sign_uf(v_initial_param_15939_7538[(v_i_7537 + (2 * v_i_7536) + (8 * v_i_7535))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}