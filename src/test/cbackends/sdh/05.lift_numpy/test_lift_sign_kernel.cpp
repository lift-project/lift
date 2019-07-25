

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
    double * v_initial_param_2362_2923 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_2366_2987 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2956 = 0;(v_tile_batch_2956 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2956)){
        int v_virtual_tile_id_2957 = (GPE_TILE_ID() + (v_tile_batch_2956 * 2));
        int v_i_2920 = v_virtual_tile_id_2957;
        if ((v_virtual_tile_id_2957 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2958 = 0;(v_gpe_batch_2958 <= 1); (++v_gpe_batch_2958)){
                    ; 
                    int v_i_2921 = GPEQ_POP();
                    if ((v_i_2921 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2922 = 0;(v_i_2922 < 2); v_i_2922 = (v_i_2922 + 1)){
                            v_user_func_2366_2987[(v_i_2922 + (2 * v_i_2921) + (8 * v_i_2920))] = sign_uf(v_initial_param_2362_2923[(v_i_2922 + (2 * v_i_2921) + (8 * v_i_2920))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}