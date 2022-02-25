

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ID_DOUBLE_UF_H
#define ID_DOUBLE_UF_H
; 
double id_double_uf(double x){
    { return x; }; 
}

#endif
 ; 
void lift_positive_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_1871_2447 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_1875_2511 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    for (int v_tile_batch_2879 = 0;(v_tile_batch_2879 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2879)){
        int v_virtual_tile_id_2880 = (GPE_TILE_ID() + (v_tile_batch_2879 * 2));
        int v_i_2444 = v_virtual_tile_id_2880;
        if ((v_virtual_tile_id_2880 < ((v_N_0)/(8)))){
            {
                for (int v_gpe_batch_2881 = 0;(v_gpe_batch_2881 <= 1); (++v_gpe_batch_2881)){
                    ; 
                    int v_i_2445 = GPEQ_POP();
                    if ((v_i_2445 < 4)){
                        // For each element processed sequentially
                        for (int v_i_2446 = 0;(v_i_2446 < 2); v_i_2446 = (v_i_2446 + 1)){
                            v_user_func_1875_2511[(v_i_2446 + (2 * v_i_2445) + (8 * v_i_2444))] = id_double_uf(v_initial_param_1871_2447[(v_i_2446 + (2 * v_i_2445) + (8 * v_i_2444))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}