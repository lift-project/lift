

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_rad2deg_sched(double * v_initial_param_14008_5430, double * & v_user_func_14012_5494, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14055 = 0;(gpe_loop_cvar_14055 < 4); (++gpe_loop_cvar_14055)){
        GPEQ_PUSH(gpe_loop_cvar_14055, reinterpret_cast<uintptr_t>(v_initial_param_14008_5430)); 
        GPEQ_PUSH(gpe_loop_cvar_14055, reinterpret_cast<uintptr_t>(v_user_func_14012_5494)); 
        GPEQ_PUSH(gpe_loop_cvar_14055, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16184 = 0;(v_tile_batch_16184 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16184)){
        int v_virtual_tile_id_16185 = (LCP_TILE_ID() + (v_tile_batch_16184 * 2));
        if ((v_virtual_tile_id_16185 < ((v_N_4617)/(8))))for (int v_gpe_batch_16186 = 0;(v_gpe_batch_16186 <= 1); (++v_gpe_batch_16186)){
            for (int v_gpe_16187 = 0;(v_gpe_16187 < 4); (++v_gpe_16187)){
                GPEQ_PUSH(v_gpe_16187, (v_gpe_16187 + (4 * v_gpe_batch_16186))); 
            }
            {
                
            }
            for (int v_gpe_16188 = 0;(v_gpe_16188 < 4); (++v_gpe_16188)){
                ; 
                LCPQ_POP(v_gpe_16188); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}