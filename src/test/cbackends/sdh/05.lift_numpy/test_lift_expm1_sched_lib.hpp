

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_expm1_sched(double * v_initial_param_14925_6450, double * & v_user_func_14929_6514, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14972 = 0;(gpe_loop_cvar_14972 < 4); (++gpe_loop_cvar_14972)){
        GPEQ_PUSH(gpe_loop_cvar_14972, reinterpret_cast<uintptr_t>(v_initial_param_14925_6450)); 
        GPEQ_PUSH(gpe_loop_cvar_14972, reinterpret_cast<uintptr_t>(v_user_func_14929_6514)); 
        GPEQ_PUSH(gpe_loop_cvar_14972, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16349 = 0;(v_tile_batch_16349 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16349)){
        int v_virtual_tile_id_16350 = (LCP_TILE_ID() + (v_tile_batch_16349 * 2));
        if ((v_virtual_tile_id_16350 < ((v_N_4617)/(8))))for (int v_gpe_batch_16351 = 0;(v_gpe_batch_16351 <= 1); (++v_gpe_batch_16351)){
            for (int v_gpe_16352 = 0;(v_gpe_16352 < 4); (++v_gpe_16352)){
                GPEQ_PUSH(v_gpe_16352, (v_gpe_16352 + (4 * v_gpe_batch_16351))); 
            }
            {
                
            }
            for (int v_gpe_16353 = 0;(v_gpe_16353 < 4); (++v_gpe_16353)){
                ; 
                LCPQ_POP(v_gpe_16353); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}