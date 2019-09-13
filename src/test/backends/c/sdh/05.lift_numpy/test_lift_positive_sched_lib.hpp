

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_positive_sched(double * v_initial_param_15448_7062, double * & v_user_func_15452_7126, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15495 = 0;(gpe_loop_cvar_15495 < 4); (++gpe_loop_cvar_15495)){
        GPEQ_PUSH(gpe_loop_cvar_15495, reinterpret_cast<uintptr_t>(v_initial_param_15448_7062)); 
        GPEQ_PUSH(gpe_loop_cvar_15495, reinterpret_cast<uintptr_t>(v_user_func_15452_7126)); 
        GPEQ_PUSH(gpe_loop_cvar_15495, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16448 = 0;(v_tile_batch_16448 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16448)){
        int v_virtual_tile_id_16449 = (LCP_TILE_ID() + (v_tile_batch_16448 * 2));
        if ((v_virtual_tile_id_16449 < ((v_N_4617)/(8))))for (int v_gpe_batch_16450 = 0;(v_gpe_batch_16450 <= 1); (++v_gpe_batch_16450)){
            for (int v_gpe_16451 = 0;(v_gpe_16451 < 4); (++v_gpe_16451)){
                GPEQ_PUSH(v_gpe_16451, (v_gpe_16451 + (4 * v_gpe_batch_16450))); 
            }
            {
                
            }
            for (int v_gpe_16452 = 0;(v_gpe_16452 < 4); (++v_gpe_16452)){
                ; 
                LCPQ_POP(v_gpe_16452); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}