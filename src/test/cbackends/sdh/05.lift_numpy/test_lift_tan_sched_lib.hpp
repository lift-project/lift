

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_tan_sched(double * v_initial_param_168_339, double * & v_user_func_172_403, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_215 = 0;(gpe_loop_cvar_215 < 4); (++gpe_loop_cvar_215)){
        GPEQ_PUSH(gpe_loop_cvar_215, reinterpret_cast<uintptr_t>(v_initial_param_168_339)); 
        GPEQ_PUSH(gpe_loop_cvar_215, reinterpret_cast<uintptr_t>(v_user_func_172_403)); 
        GPEQ_PUSH(gpe_loop_cvar_215, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2530 = 0;(v_tile_batch_2530 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2530)){
        int v_virtual_tile_id_2531 = (LCP_TILE_ID() + (v_tile_batch_2530 * 2));
        if ((v_virtual_tile_id_2531 < ((v_N_0)/(8))))for (int v_gpe_batch_2532 = 0;(v_gpe_batch_2532 <= 1); (++v_gpe_batch_2532)){
            for (int v_gpe_2533 = 0;(v_gpe_2533 < 4); (++v_gpe_2533)){
                GPEQ_PUSH(v_gpe_2533, (v_gpe_2533 + (4 * v_gpe_batch_2532))); 
            }
            {
                
            }
            for (int v_gpe_2534 = 0;(v_gpe_2534 < 4); (++v_gpe_2534)){
                ; 
                LCPQ_POP(v_gpe_2534); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}