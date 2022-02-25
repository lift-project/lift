

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arctanh_sched(double * v_initial_param_733_1223, double * & v_user_func_737_1287, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_780 = 0;(gpe_loop_cvar_780 < 4); (++gpe_loop_cvar_780)){
        GPEQ_PUSH(gpe_loop_cvar_780, reinterpret_cast<uintptr_t>(v_initial_param_733_1223)); 
        GPEQ_PUSH(gpe_loop_cvar_780, reinterpret_cast<uintptr_t>(v_user_func_737_1287)); 
        GPEQ_PUSH(gpe_loop_cvar_780, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2673 = 0;(v_tile_batch_2673 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2673)){
        int v_virtual_tile_id_2674 = (LCP_TILE_ID() + (v_tile_batch_2673 * 2));
        if ((v_virtual_tile_id_2674 < ((v_N_0)/(8))))for (int v_gpe_batch_2675 = 0;(v_gpe_batch_2675 <= 1); (++v_gpe_batch_2675)){
            for (int v_gpe_2676 = 0;(v_gpe_2676 < 4); (++v_gpe_2676)){
                GPEQ_PUSH(v_gpe_2676, (v_gpe_2676 + (4 * v_gpe_batch_2675))); 
            }
            {
                
            }
            for (int v_gpe_2677 = 0;(v_gpe_2677 < 4); (++v_gpe_2677)){
                ; 
                LCPQ_POP(v_gpe_2677); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}