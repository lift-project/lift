

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_exp_sched(double * v_initial_param_1300_1767, double * & v_user_func_1304_1831, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1347 = 0;(gpe_loop_cvar_1347 < 4); (++gpe_loop_cvar_1347)){
        GPEQ_PUSH(gpe_loop_cvar_1347, reinterpret_cast<uintptr_t>(v_initial_param_1300_1767)); 
        GPEQ_PUSH(gpe_loop_cvar_1347, reinterpret_cast<uintptr_t>(v_user_func_1304_1831)); 
        GPEQ_PUSH(gpe_loop_cvar_1347, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2761 = 0;(v_tile_batch_2761 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2761)){
        int v_virtual_tile_id_2762 = (LCP_TILE_ID() + (v_tile_batch_2761 * 2));
        if ((v_virtual_tile_id_2762 < ((v_N_0)/(8))))for (int v_gpe_batch_2763 = 0;(v_gpe_batch_2763 <= 1); (++v_gpe_batch_2763)){
            for (int v_gpe_2764 = 0;(v_gpe_2764 < 4); (++v_gpe_2764)){
                GPEQ_PUSH(v_gpe_2764, (v_gpe_2764 + (4 * v_gpe_batch_2763))); 
            }
            {
                
            }
            for (int v_gpe_2765 = 0;(v_gpe_2765 < 4); (++v_gpe_2765)){
                ; 
                LCPQ_POP(v_gpe_2765); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}