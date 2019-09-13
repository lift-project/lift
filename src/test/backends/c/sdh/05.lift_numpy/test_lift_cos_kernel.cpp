

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef COS_UF_H
#define COS_UF_H
; 
double cos_uf(double x){
    { return cos(x); }; 
}

#endif
 ; 
void lift_cos_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_13731_4886 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13735_4950 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16104 = 0;(v_tile_batch_16104 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16104)){
        int v_virtual_tile_id_16105 = (GPE_TILE_ID() + (v_tile_batch_16104 * 2));
        int v_i_4883 = v_virtual_tile_id_16105;
        if ((v_virtual_tile_id_16105 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16106 = 0;(v_gpe_batch_16106 <= 1); (++v_gpe_batch_16106)){
                    ; 
                    int v_i_4884 = GPEQ_POP();
                    if ((v_i_4884 < 4)){
                        // For each element processed sequentially
                        for (int v_i_4885 = 0;(v_i_4885 < 2); v_i_4885 = (v_i_4885 + 1)){
                            v_user_func_13735_4950[(v_i_4885 + (2 * v_i_4884) + (8 * v_i_4883))] = cos_uf(v_initial_param_13731_4886[(v_i_4885 + (2 * v_i_4884) + (8 * v_i_4883))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}