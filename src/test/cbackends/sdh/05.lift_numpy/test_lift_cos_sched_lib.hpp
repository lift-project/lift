

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_cos_sched(double * v_initial_param_120_271, double * & v_user_func_124_335, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_167 = 0;(gpe_loop_cvar_167 < 4); (++gpe_loop_cvar_167)){
        GPEQ_PUSH(gpe_loop_cvar_167, reinterpret_cast<uintptr_t>(v_initial_param_120_271)); 
        GPEQ_PUSH(gpe_loop_cvar_167, reinterpret_cast<uintptr_t>(v_user_func_124_335)); 
        GPEQ_PUSH(gpe_loop_cvar_167, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2519 = 0;(v_tile_batch_2519 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2519)){
        int v_virtual_tile_id_2520 = (LCP_TILE_ID() + (v_tile_batch_2519 * 2));
        if ((v_virtual_tile_id_2520 < ((v_N_0)/(8))))for (int v_gpe_batch_2521 = 0;(v_gpe_batch_2521 <= 1); (++v_gpe_batch_2521)){
            for (int v_gpe_2522 = 0;(v_gpe_2522 < 4); (++v_gpe_2522)){
                GPEQ_PUSH(v_gpe_2522, (v_gpe_2522 + (4 * v_gpe_batch_2521))); 
            }
            {
                
            }
            for (int v_gpe_2523 = 0;(v_gpe_2523 < 4); (++v_gpe_2523)){
                ; 
                LCPQ_POP(v_gpe_2523); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}