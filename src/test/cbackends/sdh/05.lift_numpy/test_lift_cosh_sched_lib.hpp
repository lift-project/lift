

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_cosh_sched(double * v_initial_param_14152_5566, double * & v_user_func_14156_5630, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14199 = 0;(gpe_loop_cvar_14199 < 4); (++gpe_loop_cvar_14199)){
        GPEQ_PUSH(gpe_loop_cvar_14199, reinterpret_cast<uintptr_t>(v_initial_param_14152_5566)); 
        GPEQ_PUSH(gpe_loop_cvar_14199, reinterpret_cast<uintptr_t>(v_user_func_14156_5630)); 
        GPEQ_PUSH(gpe_loop_cvar_14199, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16206 = 0;(v_tile_batch_16206 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16206)){
        int v_virtual_tile_id_16207 = (LCP_TILE_ID() + (v_tile_batch_16206 * 2));
        if ((v_virtual_tile_id_16207 < ((v_N_4617)/(8))))for (int v_gpe_batch_16208 = 0;(v_gpe_batch_16208 <= 1); (++v_gpe_batch_16208)){
            for (int v_gpe_16209 = 0;(v_gpe_16209 < 4); (++v_gpe_16209)){
                GPEQ_PUSH(v_gpe_16209, (v_gpe_16209 + (4 * v_gpe_batch_16208))); 
            }
            {
                
            }
            for (int v_gpe_16210 = 0;(v_gpe_16210 < 4); (++v_gpe_16210)){
                ; 
                LCPQ_POP(v_gpe_16210); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}