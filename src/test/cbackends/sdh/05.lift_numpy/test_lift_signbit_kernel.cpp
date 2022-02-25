

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef SIGNBIT_UF_H
#define SIGNBIT_UF_H
; 
double signbit_uf(double x){
    return x<0? 1:0 ;; 
}

#endif
 ; 
void lift_signbit_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1726_2311 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1730_2375 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2857 = 0;(v_tile_batch_2857 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2857)){
        int v_virtual_tile_id_2858 = (GPE_TILE_ID() + (v_tile_batch_2857 * 2));
        int v_i_2308 = v_virtual_tile_id_2858;
        if ((v_virtual_tile_id_2858 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2859 = 0;(v_gpe_batch_2859 <= 1); (++v_gpe_batch_2859)){
                    ; 
                    int v_i_2309 = GPEQ_POP();
                    if ((v_i_2309 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2310 = 0;(v_i_2310 < 2); v_i_2310 = (v_i_2310 + 1)){
                            v_user_func_1730_2375[(v_i_2310 + (2 * v_i_2309) + (8 * v_i_2308))] = signbit_uf(v_initial_param_1726_2311[(v_i_2310 + (2 * v_i_2309) + (8 * v_i_2308))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}