

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_signbit_sched(double * v_initial_param_15303_6926, double * & v_user_func_15307_6990, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15350 = 0;(gpe_loop_cvar_15350 < 4); (++gpe_loop_cvar_15350)){
        GPEQ_PUSH(gpe_loop_cvar_15350, reinterpret_cast<uintptr_t>(v_initial_param_15303_6926)); 
        GPEQ_PUSH(gpe_loop_cvar_15350, reinterpret_cast<uintptr_t>(v_user_func_15307_6990)); 
        GPEQ_PUSH(gpe_loop_cvar_15350, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16426 = 0;(v_tile_batch_16426 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16426)){
        int v_virtual_tile_id_16427 = (LCP_TILE_ID() + (v_tile_batch_16426 * 2));
        if ((v_virtual_tile_id_16427 < ((v_N_4617)/(8))))for (int v_gpe_batch_16428 = 0;(v_gpe_batch_16428 <= 1); (++v_gpe_batch_16428)){
            for (int v_gpe_16429 = 0;(v_gpe_16429 < 4); (++v_gpe_16429)){
                GPEQ_PUSH(v_gpe_16429, (v_gpe_16429 + (4 * v_gpe_batch_16428))); 
            }
            {
                
            }
            for (int v_gpe_16430 = 0;(v_gpe_16430 < 4); (++v_gpe_16430)){
                ; 
                LCPQ_POP(v_gpe_16430); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}