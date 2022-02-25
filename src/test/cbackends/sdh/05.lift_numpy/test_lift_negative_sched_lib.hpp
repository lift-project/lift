

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_negative_sched(double * v_initial_param_1919_2515, double * & v_user_func_1923_2579, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1966 = 0;(gpe_loop_cvar_1966 < 4); (++gpe_loop_cvar_1966)){
        GPEQ_PUSH(gpe_loop_cvar_1966, reinterpret_cast<uintptr_t>(v_initial_param_1919_2515)); 
        GPEQ_PUSH(gpe_loop_cvar_1966, reinterpret_cast<uintptr_t>(v_user_func_1923_2579)); 
        GPEQ_PUSH(gpe_loop_cvar_1966, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2882 = 0;(v_tile_batch_2882 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2882)){
        int v_virtual_tile_id_2883 = (LCP_TILE_ID() + (v_tile_batch_2882 * 2));
        if ((v_virtual_tile_id_2883 < ((v_N_0)/(8))))for (int v_gpe_batch_2884 = 0;(v_gpe_batch_2884 <= 1); (++v_gpe_batch_2884)){
            for (int v_gpe_2885 = 0;(v_gpe_2885 < 4); (++v_gpe_2885)){
                GPEQ_PUSH(v_gpe_2885, (v_gpe_2885 + (4 * v_gpe_batch_2884))); 
            }
            {
                
            }
            for (int v_gpe_2886 = 0;(v_gpe_2886 < 4); (++v_gpe_2886)){
                ; 
                LCPQ_POP(v_gpe_2886); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}