

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_radians_sched(double * v_initial_param_14056_5294, double * & v_user_func_14060_5358, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14103 = 0;(gpe_loop_cvar_14103 < 4); (++gpe_loop_cvar_14103)){
        GPEQ_PUSH(gpe_loop_cvar_14103, reinterpret_cast<uintptr_t>(v_initial_param_14056_5294)); 
        GPEQ_PUSH(gpe_loop_cvar_14103, reinterpret_cast<uintptr_t>(v_user_func_14060_5358)); 
        GPEQ_PUSH(gpe_loop_cvar_14103, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16162 = 0;(v_tile_batch_16162 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16162)){
        int v_virtual_tile_id_16163 = (LCP_TILE_ID() + (v_tile_batch_16162 * 2));
        if ((v_virtual_tile_id_16163 < ((v_N_4617)/(8))))for (int v_gpe_batch_16164 = 0;(v_gpe_batch_16164 <= 1); (++v_gpe_batch_16164)){
            for (int v_gpe_16165 = 0;(v_gpe_16165 < 4); (++v_gpe_16165)){
                GPEQ_PUSH(v_gpe_16165, (v_gpe_16165 + (4 * v_gpe_batch_16164))); 
            }
            {
                
            }
            for (int v_gpe_16166 = 0;(v_gpe_16166 < 4); (++v_gpe_16166)){
                ; 
                LCPQ_POP(v_gpe_16166); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}