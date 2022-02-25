

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arcsinh_sched(double * v_initial_param_637_1087, double * & v_user_func_641_1151, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_684 = 0;(gpe_loop_cvar_684 < 4); (++gpe_loop_cvar_684)){
        GPEQ_PUSH(gpe_loop_cvar_684, reinterpret_cast<uintptr_t>(v_initial_param_637_1087)); 
        GPEQ_PUSH(gpe_loop_cvar_684, reinterpret_cast<uintptr_t>(v_user_func_641_1151)); 
        GPEQ_PUSH(gpe_loop_cvar_684, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2651 = 0;(v_tile_batch_2651 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2651)){
        int v_virtual_tile_id_2652 = (LCP_TILE_ID() + (v_tile_batch_2651 * 2));
        if ((v_virtual_tile_id_2652 < ((v_N_0)/(8))))for (int v_gpe_batch_2653 = 0;(v_gpe_batch_2653 <= 1); (++v_gpe_batch_2653)){
            for (int v_gpe_2654 = 0;(v_gpe_2654 < 4); (++v_gpe_2654)){
                GPEQ_PUSH(v_gpe_2654, (v_gpe_2654 + (4 * v_gpe_batch_2653))); 
            }
            {
                
            }
            for (int v_gpe_2655 = 0;(v_gpe_2655 < 4); (++v_gpe_2655)){
                ; 
                LCPQ_POP(v_gpe_2655); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}