

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sign_sched(double * v_initial_param_2362_2923, double * & v_user_func_2366_2987, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_2409 = 0;(gpe_loop_cvar_2409 < 4); (++gpe_loop_cvar_2409)){
        GPEQ_PUSH(gpe_loop_cvar_2409, reinterpret_cast<uintptr_t>(v_initial_param_2362_2923)); 
        GPEQ_PUSH(gpe_loop_cvar_2409, reinterpret_cast<uintptr_t>(v_user_func_2366_2987)); 
        GPEQ_PUSH(gpe_loop_cvar_2409, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2948 = 0;(v_tile_batch_2948 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2948)){
        int v_virtual_tile_id_2949 = (LCP_TILE_ID() + (v_tile_batch_2948 * 2));
        if ((v_virtual_tile_id_2949 < ((v_N_0)/(8))))for (int v_gpe_batch_2950 = 0;(v_gpe_batch_2950 <= 1); (++v_gpe_batch_2950)){
            for (int v_gpe_2951 = 0;(v_gpe_2951 < 4); (++v_gpe_2951)){
                GPEQ_PUSH(v_gpe_2951, (v_gpe_2951 + (4 * v_gpe_batch_2950))); 
            }
            {
                
            }
            for (int v_gpe_2952 = 0;(v_gpe_2952 < 4); (++v_gpe_2952)){
                ; 
                LCPQ_POP(v_gpe_2952); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}