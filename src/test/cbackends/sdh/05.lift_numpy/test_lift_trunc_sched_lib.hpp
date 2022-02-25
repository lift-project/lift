

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_trunc_sched(double * v_initial_param_1021_1699, double * & v_user_func_1025_1763, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1068 = 0;(gpe_loop_cvar_1068 < 4); (++gpe_loop_cvar_1068)){
        GPEQ_PUSH(gpe_loop_cvar_1068, reinterpret_cast<uintptr_t>(v_initial_param_1021_1699)); 
        GPEQ_PUSH(gpe_loop_cvar_1068, reinterpret_cast<uintptr_t>(v_user_func_1025_1763)); 
        GPEQ_PUSH(gpe_loop_cvar_1068, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2750 = 0;(v_tile_batch_2750 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2750)){
        int v_virtual_tile_id_2751 = (LCP_TILE_ID() + (v_tile_batch_2750 * 2));
        if ((v_virtual_tile_id_2751 < ((v_N_0)/(8))))for (int v_gpe_batch_2752 = 0;(v_gpe_batch_2752 <= 1); (++v_gpe_batch_2752)){
            for (int v_gpe_2753 = 0;(v_gpe_2753 < 4); (++v_gpe_2753)){
                GPEQ_PUSH(v_gpe_2753, (v_gpe_2753 + (4 * v_gpe_batch_2752))); 
            }
            {
                
            }
            for (int v_gpe_2754 = 0;(v_gpe_2754 < 4); (++v_gpe_2754)){
                ; 
                LCPQ_POP(v_gpe_2754); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}