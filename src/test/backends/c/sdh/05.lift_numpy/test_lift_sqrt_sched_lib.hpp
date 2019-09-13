

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sqrt_sched(double * v_initial_param_15747_7198, double * & v_user_func_15751_7262, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15794 = 0;(gpe_loop_cvar_15794 < 4); (++gpe_loop_cvar_15794)){
        GPEQ_PUSH(gpe_loop_cvar_15794, reinterpret_cast<uintptr_t>(v_initial_param_15747_7198)); 
        GPEQ_PUSH(gpe_loop_cvar_15794, reinterpret_cast<uintptr_t>(v_user_func_15751_7262)); 
        GPEQ_PUSH(gpe_loop_cvar_15794, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16470 = 0;(v_tile_batch_16470 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16470)){
        int v_virtual_tile_id_16471 = (LCP_TILE_ID() + (v_tile_batch_16470 * 2));
        if ((v_virtual_tile_id_16471 < ((v_N_4617)/(8))))for (int v_gpe_batch_16472 = 0;(v_gpe_batch_16472 <= 1); (++v_gpe_batch_16472)){
            for (int v_gpe_16473 = 0;(v_gpe_16473 < 4); (++v_gpe_16473)){
                GPEQ_PUSH(v_gpe_16473, (v_gpe_16473 + (4 * v_gpe_batch_16472))); 
            }
            {
                
            }
            for (int v_gpe_16474 = 0;(v_gpe_16474 < 4); (++v_gpe_16474)){
                ; 
                LCPQ_POP(v_gpe_16474); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}