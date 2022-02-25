

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_absolute_sched(double * v_initial_param_2314_2787, double * & v_user_func_2318_2851, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_2361 = 0;(gpe_loop_cvar_2361 < 4); (++gpe_loop_cvar_2361)){
        GPEQ_PUSH(gpe_loop_cvar_2361, reinterpret_cast<uintptr_t>(v_initial_param_2314_2787)); 
        GPEQ_PUSH(gpe_loop_cvar_2361, reinterpret_cast<uintptr_t>(v_user_func_2318_2851)); 
        GPEQ_PUSH(gpe_loop_cvar_2361, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2926 = 0;(v_tile_batch_2926 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2926)){
        int v_virtual_tile_id_2927 = (LCP_TILE_ID() + (v_tile_batch_2926 * 2));
        if ((v_virtual_tile_id_2927 < ((v_N_0)/(8))))for (int v_gpe_batch_2928 = 0;(v_gpe_batch_2928 <= 1); (++v_gpe_batch_2928)){
            for (int v_gpe_2929 = 0;(v_gpe_2929 < 4); (++v_gpe_2929)){
                GPEQ_PUSH(v_gpe_2929, (v_gpe_2929 + (4 * v_gpe_batch_2928))); 
            }
            {
                
            }
            for (int v_gpe_2930 = 0;(v_gpe_2930 < 4); (++v_gpe_2930)){
                ; 
                LCPQ_POP(v_gpe_2930); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}