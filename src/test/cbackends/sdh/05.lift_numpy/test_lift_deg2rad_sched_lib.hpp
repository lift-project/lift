

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_deg2rad_sched(double * v_initial_param_445_747, double * & v_user_func_449_811, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_492 = 0;(gpe_loop_cvar_492 < 4); (++gpe_loop_cvar_492)){
        GPEQ_PUSH(gpe_loop_cvar_492, reinterpret_cast<uintptr_t>(v_initial_param_445_747)); 
        GPEQ_PUSH(gpe_loop_cvar_492, reinterpret_cast<uintptr_t>(v_user_func_449_811)); 
        GPEQ_PUSH(gpe_loop_cvar_492, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2596 = 0;(v_tile_batch_2596 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2596)){
        int v_virtual_tile_id_2597 = (LCP_TILE_ID() + (v_tile_batch_2596 * 2));
        if ((v_virtual_tile_id_2597 < ((v_N_0)/(8))))for (int v_gpe_batch_2598 = 0;(v_gpe_batch_2598 <= 1); (++v_gpe_batch_2598)){
            for (int v_gpe_2599 = 0;(v_gpe_2599 < 4); (++v_gpe_2599)){
                GPEQ_PUSH(v_gpe_2599, (v_gpe_2599 + (4 * v_gpe_batch_2598))); 
            }
            {
                
            }
            for (int v_gpe_2600 = 0;(v_gpe_2600 < 4); (++v_gpe_2600)){
                ; 
                LCPQ_POP(v_gpe_2600); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}