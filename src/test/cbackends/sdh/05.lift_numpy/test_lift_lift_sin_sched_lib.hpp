

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_sin_sched(double * v_initial_param_72_202, double * & v_user_func_76_266, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_119 = 0;(gpe_loop_cvar_119 < 4); (++gpe_loop_cvar_119)){
        GPEQ_PUSH(gpe_loop_cvar_119, reinterpret_cast<uintptr_t>(v_initial_param_72_202)); 
        GPEQ_PUSH(gpe_loop_cvar_119, reinterpret_cast<uintptr_t>(v_user_func_76_266)); 
        GPEQ_PUSH(gpe_loop_cvar_119, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2508 = 0;(v_tile_batch_2508 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2508)){
        int v_virtual_tile_id_2509 = (LCP_TILE_ID() + (v_tile_batch_2508 * 2));
        if ((v_virtual_tile_id_2509 < ((v_N_0)/(8))))for (int v_gpe_batch_2510 = 0;(v_gpe_batch_2510 <= 1); (++v_gpe_batch_2510)){
            for (int v_gpe_2511 = 0;(v_gpe_2511 < 4); (++v_gpe_2511)){
                GPEQ_PUSH(v_gpe_2511, (v_gpe_2511 + (4 * v_gpe_batch_2510))); 
            }
            {
                
            }
            for (int v_gpe_2512 = 0;(v_gpe_2512 < 4); (++v_gpe_2512)){
                ; 
                LCPQ_POP(v_gpe_2512); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}