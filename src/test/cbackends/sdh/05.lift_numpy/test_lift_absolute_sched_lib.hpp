

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_absolute_sched(double * v_initial_param_15891_7402, double * & v_user_func_15895_7466, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15938 = 0;(gpe_loop_cvar_15938 < 4); (++gpe_loop_cvar_15938)){
        GPEQ_PUSH(gpe_loop_cvar_15938, reinterpret_cast<uintptr_t>(v_initial_param_15891_7402)); 
        GPEQ_PUSH(gpe_loop_cvar_15938, reinterpret_cast<uintptr_t>(v_user_func_15895_7466)); 
        GPEQ_PUSH(gpe_loop_cvar_15938, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16503 = 0;(v_tile_batch_16503 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16503)){
        int v_virtual_tile_id_16504 = (LCP_TILE_ID() + (v_tile_batch_16503 * 2));
        if ((v_virtual_tile_id_16504 < ((v_N_4617)/(8))))for (int v_gpe_batch_16505 = 0;(v_gpe_batch_16505 <= 1); (++v_gpe_batch_16505)){
            for (int v_gpe_16506 = 0;(v_gpe_16506 < 4); (++v_gpe_16506)){
                GPEQ_PUSH(v_gpe_16506, (v_gpe_16506 + (4 * v_gpe_batch_16505))); 
            }
            {
                
            }
            for (int v_gpe_16507 = 0;(v_gpe_16507 < 4); (++v_gpe_16507)){
                ; 
                LCPQ_POP(v_gpe_16507); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}