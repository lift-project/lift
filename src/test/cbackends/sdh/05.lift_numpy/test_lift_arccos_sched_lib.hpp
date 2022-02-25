

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arccos_sched(double * v_initial_param_264_475, double * & v_user_func_268_539, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_311 = 0;(gpe_loop_cvar_311 < 4); (++gpe_loop_cvar_311)){
        GPEQ_PUSH(gpe_loop_cvar_311, reinterpret_cast<uintptr_t>(v_initial_param_264_475)); 
        GPEQ_PUSH(gpe_loop_cvar_311, reinterpret_cast<uintptr_t>(v_user_func_268_539)); 
        GPEQ_PUSH(gpe_loop_cvar_311, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2552 = 0;(v_tile_batch_2552 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2552)){
        int v_virtual_tile_id_2553 = (LCP_TILE_ID() + (v_tile_batch_2552 * 2));
        if ((v_virtual_tile_id_2553 < ((v_N_0)/(8))))for (int v_gpe_batch_2554 = 0;(v_gpe_batch_2554 <= 1); (++v_gpe_batch_2554)){
            for (int v_gpe_2555 = 0;(v_gpe_2555 < 4); (++v_gpe_2555)){
                GPEQ_PUSH(v_gpe_2555, (v_gpe_2555 + (4 * v_gpe_batch_2554))); 
            }
            {
                
            }
            for (int v_gpe_2556 = 0;(v_gpe_2556 < 4); (++v_gpe_2556)){
                ; 
                LCPQ_POP(v_gpe_2556); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}