

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_ceil_sched(double * v_initial_param_973_1631, double * & v_user_func_977_1695, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1020 = 0;(gpe_loop_cvar_1020 < 4); (++gpe_loop_cvar_1020)){
        GPEQ_PUSH(gpe_loop_cvar_1020, reinterpret_cast<uintptr_t>(v_initial_param_973_1631)); 
        GPEQ_PUSH(gpe_loop_cvar_1020, reinterpret_cast<uintptr_t>(v_user_func_977_1695)); 
        GPEQ_PUSH(gpe_loop_cvar_1020, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2739 = 0;(v_tile_batch_2739 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2739)){
        int v_virtual_tile_id_2740 = (LCP_TILE_ID() + (v_tile_batch_2739 * 2));
        if ((v_virtual_tile_id_2740 < ((v_N_0)/(8))))for (int v_gpe_batch_2741 = 0;(v_gpe_batch_2741 <= 1); (++v_gpe_batch_2741)){
            for (int v_gpe_2742 = 0;(v_gpe_2742 < 4); (++v_gpe_2742)){
                GPEQ_PUSH(v_gpe_2742, (v_gpe_2742 + (4 * v_gpe_batch_2741))); 
            }
            {
                
            }
            for (int v_gpe_2743 = 0;(v_gpe_2743 < 4); (++v_gpe_2743)){
                ; 
                LCPQ_POP(v_gpe_2743); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}