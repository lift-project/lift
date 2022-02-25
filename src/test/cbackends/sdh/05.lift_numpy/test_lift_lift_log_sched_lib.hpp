

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_log_sched(double * v_initial_param_1444_1971, double * & v_user_func_1448_2035, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1491 = 0;(gpe_loop_cvar_1491 < 4); (++gpe_loop_cvar_1491)){
        GPEQ_PUSH(gpe_loop_cvar_1491, reinterpret_cast<uintptr_t>(v_initial_param_1444_1971)); 
        GPEQ_PUSH(gpe_loop_cvar_1491, reinterpret_cast<uintptr_t>(v_user_func_1448_2035)); 
        GPEQ_PUSH(gpe_loop_cvar_1491, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2794 = 0;(v_tile_batch_2794 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2794)){
        int v_virtual_tile_id_2795 = (LCP_TILE_ID() + (v_tile_batch_2794 * 2));
        if ((v_virtual_tile_id_2795 < ((v_N_0)/(8))))for (int v_gpe_batch_2796 = 0;(v_gpe_batch_2796 <= 1); (++v_gpe_batch_2796)){
            for (int v_gpe_2797 = 0;(v_gpe_2797 < 4); (++v_gpe_2797)){
                GPEQ_PUSH(v_gpe_2797, (v_gpe_2797 + (4 * v_gpe_batch_2796))); 
            }
            {
                
            }
            for (int v_gpe_2798 = 0;(v_gpe_2798 < 4); (++v_gpe_2798)){
                ; 
                LCPQ_POP(v_gpe_2798); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}