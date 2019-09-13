

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(double * v_initial_param_13505_4352, double * & v_user_func_13508_4616, int v_N_4342){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13600 = 0;(gpe_loop_cvar_13600 < 4); (++gpe_loop_cvar_13600)){
        GPEQ_PUSH(gpe_loop_cvar_13600, reinterpret_cast<uintptr_t>(v_initial_param_13505_4352)); 
        GPEQ_PUSH(gpe_loop_cvar_13600, reinterpret_cast<uintptr_t>(v_user_func_13508_4616)); 
        GPEQ_PUSH(gpe_loop_cvar_13600, v_N_4342); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_13601 = 0;(v_tile_batch_13601 <= (((v_N_4342)/((v_N_4342 / 8) * 4)) / 2)); (++v_tile_batch_13601)){
        int v_virtual_tile_id_13602 = (LCP_TILE_ID() + (v_tile_batch_13601 * 2));
        if ((v_virtual_tile_id_13602 < ((v_N_4342)/((v_N_4342 / 8) * 4))))for (int v_gpe_batch_13603 = 0;(v_gpe_batch_13603 <= 1); (++v_gpe_batch_13603)){
            for (int v_gpe_13604 = 0;(v_gpe_13604 < 4); (++v_gpe_13604)){
                GPEQ_PUSH(v_gpe_13604, (v_gpe_13604 + (4 * v_gpe_batch_13603))); 
            }
            {
                
            }
            for (int v_gpe_13605 = 0;(v_gpe_13605 < 4); (++v_gpe_13605)){
                ; 
                LCPQ_POP(v_gpe_13605); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
    // LCPSingle
    if ((LCP_TILE_ID() == 0)){
        // For each element reduced sequentially
        v_user_func_13508_4616[0] = 0.0; 
        for (int v_i_4351 = 0;(v_i_4351 <= (-1 + ((v_N_4342)/((v_N_4342 / 8))))); (++v_i_4351)){
            v_user_func_13508_4616[0] = add(v_user_func_13508_4616[0], v_initial_param_13505_4352[(v_i_4351 * (v_N_4342 / 8))]); 
        }
    }
}