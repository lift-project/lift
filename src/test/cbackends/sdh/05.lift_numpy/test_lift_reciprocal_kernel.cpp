

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef RECIPROCAL_UF_H
#define RECIPROCAL_UF_H
; 
double reciprocal_uf(double x){
    return 1.0f/x; 
}

#endif
 ; 
void lift_reciprocal_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15400_6994 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15404_7058 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16445 = 0;(v_tile_batch_16445 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16445)){
        int v_virtual_tile_id_16446 = (GPE_TILE_ID() + (v_tile_batch_16445 * 2));
        int v_i_6991 = v_virtual_tile_id_16446;
        if ((v_virtual_tile_id_16446 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16447 = 0;(v_gpe_batch_16447 <= 1); (++v_gpe_batch_16447)){
                    ; 
                    int v_i_6992 = GPEQ_POP();
                    if ((v_i_6992 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6993 = 0;(v_i_6993 < 2); v_i_6993 = (v_i_6993 + 1)){
                            v_user_func_15404_7058[(v_i_6993 + (2 * v_i_6992) + (8 * v_i_6991))] = reciprocal_uf(v_initial_param_15400_6994[(v_i_6993 + (2 * v_i_6992) + (8 * v_i_6991))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}