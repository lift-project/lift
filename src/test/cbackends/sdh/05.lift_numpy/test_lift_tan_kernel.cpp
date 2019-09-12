

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef TAN_UF_H
#define TAN_UF_H
; 
double tan_uf(double x){
    { return tan(x); }; 
}

#endif
 ; 
void lift_tan_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_13779_4954 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13783_5018 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16115 = 0;(v_tile_batch_16115 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16115)){
        int v_virtual_tile_id_16116 = (GPE_TILE_ID() + (v_tile_batch_16115 * 2));
        int v_i_4951 = v_virtual_tile_id_16116;
        if ((v_virtual_tile_id_16116 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16117 = 0;(v_gpe_batch_16117 <= 1); (++v_gpe_batch_16117)){
                    ; 
                    int v_i_4952 = GPEQ_POP();
                    if ((v_i_4952 < 4)){
                        // For each element processed sequentially
                        for (int v_i_4953 = 0;(v_i_4953 < 2); v_i_4953 = (v_i_4953 + 1)){
                            v_user_func_13783_5018[(v_i_4953 + (2 * v_i_4952) + (8 * v_i_4951))] = tan_uf(v_initial_param_13779_4954[(v_i_4953 + (2 * v_i_4952) + (8 * v_i_4951))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}