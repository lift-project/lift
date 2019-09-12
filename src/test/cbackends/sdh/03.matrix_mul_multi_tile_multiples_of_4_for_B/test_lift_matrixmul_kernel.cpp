

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 

#ifndef MULTANDSUMUP_H
#define MULTANDSUMUP_H
; 
float multAndSumUp(float acc, float l, float r){
    { return acc + (l * r); }; 
}

#endif
 ; 
void lift_execute_kernel(int argc, vector<TMData *> argv){
    // Pop input, output pointers and sizes
    float * v_initial_param_13356_3824 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_13357_3825 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_13373_4337 = reinterpret_cast<float *>(GPEQ_POP());
    int v_M_3813 = GPEQ_POP();
    int v_K_3814 = GPEQ_POP();
    int v_N_3812 = GPEQ_POP();
    for (int v_tile_batch_13431 = 0;(v_tile_batch_13431 <= 1); (++v_tile_batch_13431)){
        int v_virtual_tile_id_13432 = (GPE_TILE_ID() + (v_tile_batch_13431 * 2));
        int v_i_3821 = v_virtual_tile_id_13432;
        if ((v_virtual_tile_id_13432 < 2)){
            {
                for (int v_gpe_batch_13433 = 0;(v_gpe_batch_13433 <= (v_N_3812 / 4)); (++v_gpe_batch_13433)){
                    ; 
                    int v_i_3822 = GPEQ_POP();
                    if ((v_i_3822 < v_N_3812)){
                        {
                            
                        }
                        {
                            
                        }
                        // For each element reduced sequentially
                        v_user_func_13373_4337[(v_i_3822 + (2 * v_N_3812 * v_i_3820) + (v_N_3812 * v_i_3821))] = 0.0f; 
                        for (int v_i_3823 = 0;(v_i_3823 <= (-1 + v_K_3814)); (++v_i_3823)){
                            v_user_func_13373_4337[(v_i_3822 + (2 * v_N_3812 * v_i_3820) + (v_N_3812 * v_i_3821))] = multAndSumUp(v_user_func_13373_4337[(v_i_3822 + (2 * v_N_3812 * v_i_3820) + (v_N_3812 * v_i_3821))], v_initial_param_13356_3824[(v_i_3823 + (2 * v_K_3814 * v_i_3820) + (v_K_3814 * v_i_3821))], v_initial_param_13357_3825[(v_i_3823 + (v_K_3814 * v_i_3822))]); 
                        }
                    }
                    LCPQ_PUSH(1); 
                }
            }
        }
    }
}