

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_negative_sched(double * v_initial_param_15496_7130, double * & v_user_func_15500_7194, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15543 = 0;(gpe_loop_cvar_15543 < 4); (++gpe_loop_cvar_15543)){
        GPEQ_PUSH(gpe_loop_cvar_15543, reinterpret_cast<uintptr_t>(v_initial_param_15496_7130)); 
        GPEQ_PUSH(gpe_loop_cvar_15543, reinterpret_cast<uintptr_t>(v_user_func_15500_7194)); 
        GPEQ_PUSH(gpe_loop_cvar_15543, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16459 = 0;(v_tile_batch_16459 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16459)){
        int v_virtual_tile_id_16460 = (LCP_TILE_ID() + (v_tile_batch_16459 * 2));
        if ((v_virtual_tile_id_16460 < ((v_N_4617)/(8))))for (int v_gpe_batch_16461 = 0;(v_gpe_batch_16461 <= 1); (++v_gpe_batch_16461)){
            for (int v_gpe_16462 = 0;(v_gpe_16462 < 4); (++v_gpe_16462)){
                GPEQ_PUSH(v_gpe_16462, (v_gpe_16462 + (4 * v_gpe_batch_16461))); 
            }
            {
                
            }
            for (int v_gpe_16463 = 0;(v_gpe_16463 < 4); (++v_gpe_16463)){
                ; 
                LCPQ_POP(v_gpe_16463); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}