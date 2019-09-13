

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_exp_sched(double * v_initial_param_14877_6382, double * & v_user_func_14881_6446, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14924 = 0;(gpe_loop_cvar_14924 < 4); (++gpe_loop_cvar_14924)){
        GPEQ_PUSH(gpe_loop_cvar_14924, reinterpret_cast<uintptr_t>(v_initial_param_14877_6382)); 
        GPEQ_PUSH(gpe_loop_cvar_14924, reinterpret_cast<uintptr_t>(v_user_func_14881_6446)); 
        GPEQ_PUSH(gpe_loop_cvar_14924, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16338 = 0;(v_tile_batch_16338 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16338)){
        int v_virtual_tile_id_16339 = (LCP_TILE_ID() + (v_tile_batch_16338 * 2));
        if ((v_virtual_tile_id_16339 < ((v_N_4617)/(8))))for (int v_gpe_batch_16340 = 0;(v_gpe_batch_16340 <= 1); (++v_gpe_batch_16340)){
            for (int v_gpe_16341 = 0;(v_gpe_16341 < 4); (++v_gpe_16341)){
                GPEQ_PUSH(v_gpe_16341, (v_gpe_16341 + (4 * v_gpe_batch_16340))); 
            }
            {
                
            }
            for (int v_gpe_16342 = 0;(v_gpe_16342 < 4); (++v_gpe_16342)){
                ; 
                LCPQ_POP(v_gpe_16342); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}