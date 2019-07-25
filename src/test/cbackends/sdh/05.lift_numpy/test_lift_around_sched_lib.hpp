

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_around_sched(double * v_initial_param_781_1291, double * & v_user_func_785_1355, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_828 = 0;(gpe_loop_cvar_828 < 4); (++gpe_loop_cvar_828)){
        GPEQ_PUSH(gpe_loop_cvar_828, reinterpret_cast<uintptr_t>(v_initial_param_781_1291)); 
        GPEQ_PUSH(gpe_loop_cvar_828, reinterpret_cast<uintptr_t>(v_user_func_785_1355)); 
        GPEQ_PUSH(gpe_loop_cvar_828, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2684 = 0;(v_tile_batch_2684 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2684)){
        int v_virtual_tile_id_2685 = (LCP_TILE_ID() + (v_tile_batch_2684 * 2));
        if ((v_virtual_tile_id_2685 < ((v_N_0)/(8))))for (int v_gpe_batch_2686 = 0;(v_gpe_batch_2686 <= 1); (++v_gpe_batch_2686)){
            for (int v_gpe_2687 = 0;(v_gpe_2687 < 4); (++v_gpe_2687)){
                GPEQ_PUSH(v_gpe_2687, (v_gpe_2687 + (4 * v_gpe_batch_2686))); 
            }
            {
                
            }
            for (int v_gpe_2688 = 0;(v_gpe_2688 < 4); (++v_gpe_2688)){
                ; 
                LCPQ_POP(v_gpe_2688); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}