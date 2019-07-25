

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_log1p_sched(double * v_initial_param_1588_2175, double * & v_user_func_1592_2239, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1635 = 0;(gpe_loop_cvar_1635 < 4); (++gpe_loop_cvar_1635)){
        GPEQ_PUSH(gpe_loop_cvar_1635, reinterpret_cast<uintptr_t>(v_initial_param_1588_2175)); 
        GPEQ_PUSH(gpe_loop_cvar_1635, reinterpret_cast<uintptr_t>(v_user_func_1592_2239)); 
        GPEQ_PUSH(gpe_loop_cvar_1635, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2827 = 0;(v_tile_batch_2827 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2827)){
        int v_virtual_tile_id_2828 = (LCP_TILE_ID() + (v_tile_batch_2827 * 2));
        if ((v_virtual_tile_id_2828 < ((v_N_0)/(8))))for (int v_gpe_batch_2829 = 0;(v_gpe_batch_2829 <= 1); (++v_gpe_batch_2829)){
            for (int v_gpe_2830 = 0;(v_gpe_2830 < 4); (++v_gpe_2830)){
                GPEQ_PUSH(v_gpe_2830, (v_gpe_2830 + (4 * v_gpe_batch_2829))); 
            }
            {
                
            }
            for (int v_gpe_2831 = 0;(v_gpe_2831 < 4); (++v_gpe_2831)){
                ; 
                LCPQ_POP(v_gpe_2831); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}