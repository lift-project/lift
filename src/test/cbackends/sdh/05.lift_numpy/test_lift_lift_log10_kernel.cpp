

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef LOG10_UF_H
#define LOG10_UF_H
; 
double log10_uf(double x){
    return log10(x) ;; 
}

#endif
 ; 
void lift_lift_log10_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_15069_6654 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_15073_6718 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4617 = GPEQ_POP();
    for (int v_tile_batch_16390 = 0;(v_tile_batch_16390 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16390)){
        int v_virtual_tile_id_16391 = (GPE_TILE_ID() + (v_tile_batch_16390 * 2));
        int v_i_6651 = v_virtual_tile_id_16391;
        if ((v_virtual_tile_id_16391 < ((v_N_4617)/(8)))){
            {
                for (int v_gpe_batch_16392 = 0;(v_gpe_batch_16392 <= 1); (++v_gpe_batch_16392)){
                    ; 
                    int v_i_6652 = GPEQ_POP();
                    if ((v_i_6652 < 4)){
                        // For each element processed sequentially
                        for (int v_i_6653 = 0;(v_i_6653 < 2); v_i_6653 = (v_i_6653 + 1)){
                            v_user_func_15073_6718[(v_i_6653 + (2 * v_i_6652) + (8 * v_i_6651))] = log10_uf(v_initial_param_15069_6654[(v_i_6653 + (2 * v_i_6652) + (8 * v_i_6651))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}