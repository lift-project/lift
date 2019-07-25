

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(float * v_initial_param_72_19, float * v_initial_param_73_20, float * & v_user_func_120_533, int v_M_5, int v_K_6, int v_N_4){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_186 = 0;(gpe_loop_cvar_186 < 4); (++gpe_loop_cvar_186)){
        GPEQ_PUSH(gpe_loop_cvar_186, reinterpret_cast<uintptr_t>(v_initial_param_72_19)); 
        GPEQ_PUSH(gpe_loop_cvar_186, reinterpret_cast<uintptr_t>(v_initial_param_73_20)); 
        GPEQ_PUSH(gpe_loop_cvar_186, reinterpret_cast<uintptr_t>(v_user_func_120_533)); 
        GPEQ_PUSH(gpe_loop_cvar_186, v_M_5); 
        GPEQ_PUSH(gpe_loop_cvar_186, v_K_6); 
        GPEQ_PUSH(gpe_loop_cvar_186, v_N_4); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_187 = 0;(v_tile_batch_187 <= (((v_M_5)/((v_M_5 / 2))) / 2)); (++v_tile_batch_187)){
        int v_virtual_tile_id_188 = (LCP_TILE_ID() + (v_tile_batch_187 * 2));
        if ((v_virtual_tile_id_188 < ((v_M_5)/((v_M_5 / 2))))){
            
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}