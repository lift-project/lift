

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_fabs_sched(double * v_initial_param_15891_7470, double * & v_user_func_15895_7534, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15938 = 0;(gpe_loop_cvar_15938 < 4); (++gpe_loop_cvar_15938)){
        GPEQ_PUSH(gpe_loop_cvar_15938, reinterpret_cast<uintptr_t>(v_initial_param_15891_7470)); 
        GPEQ_PUSH(gpe_loop_cvar_15938, reinterpret_cast<uintptr_t>(v_user_func_15895_7534)); 
        GPEQ_PUSH(gpe_loop_cvar_15938, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16514 = 0;(v_tile_batch_16514 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16514)){
        int v_virtual_tile_id_16515 = (LCP_TILE_ID() + (v_tile_batch_16514 * 2));
        if ((v_virtual_tile_id_16515 < ((v_N_4617)/(8))))for (int v_gpe_batch_16516 = 0;(v_gpe_batch_16516 <= 1); (++v_gpe_batch_16516)){
            for (int v_gpe_16517 = 0;(v_gpe_16517 < 4); (++v_gpe_16517)){
                GPEQ_PUSH(v_gpe_16517, (v_gpe_16517 + (4 * v_gpe_batch_16516))); 
            }
            {
                
            }
            for (int v_gpe_16518 = 0;(v_gpe_16518 < 4); (++v_gpe_16518)){
                ; 
                LCPQ_POP(v_gpe_16518); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}