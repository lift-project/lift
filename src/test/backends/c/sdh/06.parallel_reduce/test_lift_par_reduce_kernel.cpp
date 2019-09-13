

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef ADD_H
#define ADD_H
; 
double add(double l, double r){
    { return (l + r); }; 
}

#endif
 ; 
void lift_matrixmul_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    double * v_initial_param_13505_4352 = reinterpret_cast<double *>(GPEQ_POP());
    double * v_user_func_13508_4616 = reinterpret_cast<double *>(GPEQ_POP());
    int v_N_4342 = GPEQ_POP();
    for (int v_tile_batch_13609 = 0;(v_tile_batch_13609 <= (((v_N_4342)/((v_N_4342 / 8) * 4)) / 2)); (++v_tile_batch_13609)){
        int v_virtual_tile_id_13610 = (GPE_TILE_ID() + (v_tile_batch_13609 * 2));
        int v_i_4348 = v_virtual_tile_id_13610;
        if ((v_virtual_tile_id_13610 < ((v_N_4342)/((v_N_4342 / 8) * 4)))){
            {
                for (int v_gpe_batch_13611 = 0;(v_gpe_batch_13611 <= 1); (++v_gpe_batch_13611)){
                    ; 
                    int v_i_4349 = GPEQ_POP();
                    if ((v_i_4349 < 4)){
                        // For each element reduced sequentially
                        v_user_func_13537_4612[(v_i_4349 + (4 * v_i_4348))] = 0.0; 
                        for (int v_i_4350 = 0;(v_i_4350 <= (-1 + (v_N_4342 / 8))); (++v_i_4350)){
                            v_user_func_13537_4612[(v_i_4349 + (4 * v_i_4348))] = add(v_user_func_13537_4612[(v_i_4349 + (4 * v_i_4348))], v_initial_param_13505_4352[(v_i_4350 + (4 * v_i_4348 * (v_N_4342 / 8)) + (v_i_4349 * (v_N_4342 / 8)))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}