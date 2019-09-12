

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(float * v_initial_param_16898_8237, float * v_initial_param_16899_8238, float * & v_user_func_16909_8302, int v_M_8228, int v_K_8229, int v_N_8227){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_16941 = 0;(gpe_loop_cvar_16941 < 4); (++gpe_loop_cvar_16941)){
        GPEQ_PUSH(gpe_loop_cvar_16941, reinterpret_cast<uintptr_t>(v_initial_param_16898_8237)); 
        GPEQ_PUSH(gpe_loop_cvar_16941, reinterpret_cast<uintptr_t>(v_initial_param_16899_8238)); 
        GPEQ_PUSH(gpe_loop_cvar_16941, reinterpret_cast<uintptr_t>(v_user_func_16909_8302)); 
        GPEQ_PUSH(gpe_loop_cvar_16941, v_M_8228); 
        GPEQ_PUSH(gpe_loop_cvar_16941, v_K_8229); 
        GPEQ_PUSH(gpe_loop_cvar_16941, v_N_8227); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16942 = 0;(v_tile_batch_16942 <= (v_M_8228 / 2)); (++v_tile_batch_16942)){
        int v_virtual_tile_id_16943 = (LCP_TILE_ID() + (v_tile_batch_16942 * 2));
        if ((v_virtual_tile_id_16943 < v_M_8228))for (int v_gpe_batch_16944 = 0;(v_gpe_batch_16944 <= (v_N_8227 / 4)); (++v_gpe_batch_16944)){
            for (int v_gpe_16945 = 0;(v_gpe_16945 < 4); (++v_gpe_16945)){
                GPEQ_PUSH(v_gpe_16945, (v_gpe_16945 + (4 * v_gpe_batch_16944))); 
            }
            {
                
            }
            for (int v_gpe_16946 = 0;(v_gpe_16946 < 4); (++v_gpe_16946)){
                ; 
                LCPQ_POP(v_gpe_16946); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}