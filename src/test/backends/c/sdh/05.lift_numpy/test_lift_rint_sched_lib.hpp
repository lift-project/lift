

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_rint_sched(double * v_initial_param_14440_6042, double * & v_user_func_14444_6106, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14487 = 0;(gpe_loop_cvar_14487 < 4); (++gpe_loop_cvar_14487)){
        GPEQ_PUSH(gpe_loop_cvar_14487, reinterpret_cast<uintptr_t>(v_initial_param_14440_6042)); 
        GPEQ_PUSH(gpe_loop_cvar_14487, reinterpret_cast<uintptr_t>(v_user_func_14444_6106)); 
        GPEQ_PUSH(gpe_loop_cvar_14487, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16283 = 0;(v_tile_batch_16283 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16283)){
        int v_virtual_tile_id_16284 = (LCP_TILE_ID() + (v_tile_batch_16283 * 2));
        if ((v_virtual_tile_id_16284 < ((v_N_4617)/(8))))for (int v_gpe_batch_16285 = 0;(v_gpe_batch_16285 <= 1); (++v_gpe_batch_16285)){
            for (int v_gpe_16286 = 0;(v_gpe_16286 < 4); (++v_gpe_16286)){
                GPEQ_PUSH(v_gpe_16286, (v_gpe_16286 + (4 * v_gpe_batch_16285))); 
            }
            {
                
            }
            for (int v_gpe_16287 = 0;(v_gpe_16287 < 4); (++v_gpe_16287)){
                ; 
                LCPQ_POP(v_gpe_16287); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}