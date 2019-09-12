

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_cbrt_sched(double * v_initial_param_15795_7266, double * & v_user_func_15799_7330, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15842 = 0;(gpe_loop_cvar_15842 < 4); (++gpe_loop_cvar_15842)){
        GPEQ_PUSH(gpe_loop_cvar_15842, reinterpret_cast<uintptr_t>(v_initial_param_15795_7266)); 
        GPEQ_PUSH(gpe_loop_cvar_15842, reinterpret_cast<uintptr_t>(v_user_func_15799_7330)); 
        GPEQ_PUSH(gpe_loop_cvar_15842, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16481 = 0;(v_tile_batch_16481 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16481)){
        int v_virtual_tile_id_16482 = (LCP_TILE_ID() + (v_tile_batch_16481 * 2));
        if ((v_virtual_tile_id_16482 < ((v_N_4617)/(8))))for (int v_gpe_batch_16483 = 0;(v_gpe_batch_16483 <= 1); (++v_gpe_batch_16483)){
            for (int v_gpe_16484 = 0;(v_gpe_16484 < 4); (++v_gpe_16484)){
                GPEQ_PUSH(v_gpe_16484, (v_gpe_16484 + (4 * v_gpe_batch_16483))); 
            }
            {
                
            }
            for (int v_gpe_16485 = 0;(v_gpe_16485 < 4); (++v_gpe_16485)){
                ; 
                LCPQ_POP(v_gpe_16485); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}