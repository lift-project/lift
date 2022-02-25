

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_fix_sched(double * v_initial_param_877_1495, double * & v_user_func_881_1559, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_924 = 0;(gpe_loop_cvar_924 < 4); (++gpe_loop_cvar_924)){
        GPEQ_PUSH(gpe_loop_cvar_924, reinterpret_cast<uintptr_t>(v_initial_param_877_1495)); 
        GPEQ_PUSH(gpe_loop_cvar_924, reinterpret_cast<uintptr_t>(v_user_func_881_1559)); 
        GPEQ_PUSH(gpe_loop_cvar_924, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2717 = 0;(v_tile_batch_2717 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2717)){
        int v_virtual_tile_id_2718 = (LCP_TILE_ID() + (v_tile_batch_2717 * 2));
        if ((v_virtual_tile_id_2718 < ((v_N_0)/(8))))for (int v_gpe_batch_2719 = 0;(v_gpe_batch_2719 <= 1); (++v_gpe_batch_2719)){
            for (int v_gpe_2720 = 0;(v_gpe_2720 < 4); (++v_gpe_2720)){
                GPEQ_PUSH(v_gpe_2720, (v_gpe_2720 + (4 * v_gpe_batch_2719))); 
            }
            {
                
            }
            for (int v_gpe_2721 = 0;(v_gpe_2721 < 4); (++v_gpe_2721)){
                ; 
                LCPQ_POP(v_gpe_2721); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}