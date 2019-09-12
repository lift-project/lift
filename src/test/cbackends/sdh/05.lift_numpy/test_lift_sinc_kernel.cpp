

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SINC_UF_H
#define SINC_UF_H
; 
double sinc_uf(double x){
    return sin(M_PI*x)/(M_PI*x) ;; 
}

#endif
 ; 
void lift_sinc_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15255_6858 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15259_6922 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16423 = 0;(v_tile_batch_16423 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16423)){
        int v_virtual_tile_id_16424 = (GPE_TILE_ID() + (v_tile_batch_16423 * 2));
        int v_i_6855 = v_virtual_tile_id_16424;
        if ((v_virtual_tile_id_16424 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16425 = 0;(v_gpe_batch_16425 <= 1); (++v_gpe_batch_16425)){
                    ; 
                    int v_i_6856 = GPEQ_POP();
                    if ((v_i_6856 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6857 = 0;(v_i_6857 < 2); v_i_6857 = (v_i_6857 + 1)){
                            v_user_func_15259_6922[(v_i_6857 + (2 * v_i_6856) + (8 * v_i_6855))] = sinc_uf(v_initial_param_15255_6858[(v_i_6857 + (2 * v_i_6856) + (8 * v_i_6855))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}