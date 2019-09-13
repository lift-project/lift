

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_log1p_sched(double * v_initial_param_15165_6790, double * & v_user_func_15169_6854, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15212 = 0;(gpe_loop_cvar_15212 < 4); (++gpe_loop_cvar_15212)){
        GPEQ_PUSH(gpe_loop_cvar_15212, reinterpret_cast<uintptr_t>(v_initial_param_15165_6790)); 
        GPEQ_PUSH(gpe_loop_cvar_15212, reinterpret_cast<uintptr_t>(v_user_func_15169_6854)); 
        GPEQ_PUSH(gpe_loop_cvar_15212, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16404 = 0;(v_tile_batch_16404 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16404)){
        int v_virtual_tile_id_16405 = (LCP_TILE_ID() + (v_tile_batch_16404 * 2));
        if ((v_virtual_tile_id_16405 < ((v_N_4617)/(8))))for (int v_gpe_batch_16406 = 0;(v_gpe_batch_16406 <= 1); (++v_gpe_batch_16406)){
            for (int v_gpe_16407 = 0;(v_gpe_16407 < 4); (++v_gpe_16407)){
                GPEQ_PUSH(v_gpe_16407, (v_gpe_16407 + (4 * v_gpe_batch_16406))); 
            }
            {
                
            }
            for (int v_gpe_16408 = 0;(v_gpe_16408 < 4); (++v_gpe_16408)){
                ; 
                LCPQ_POP(v_gpe_16408); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}