

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arctanh_sched(double * v_initial_param_14344_5838, double * & v_user_func_14348_5902, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14391 = 0;(gpe_loop_cvar_14391 < 4); (++gpe_loop_cvar_14391)){
        GPEQ_PUSH(gpe_loop_cvar_14391, reinterpret_cast<uintptr_t>(v_initial_param_14344_5838)); 
        GPEQ_PUSH(gpe_loop_cvar_14391, reinterpret_cast<uintptr_t>(v_user_func_14348_5902)); 
        GPEQ_PUSH(gpe_loop_cvar_14391, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16250 = 0;(v_tile_batch_16250 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16250)){
        int v_virtual_tile_id_16251 = (LCP_TILE_ID() + (v_tile_batch_16250 * 2));
        if ((v_virtual_tile_id_16251 < ((v_N_4617)/(8))))for (int v_gpe_batch_16252 = 0;(v_gpe_batch_16252 <= 1); (++v_gpe_batch_16252)){
            for (int v_gpe_16253 = 0;(v_gpe_16253 < 4); (++v_gpe_16253)){
                GPEQ_PUSH(v_gpe_16253, (v_gpe_16253 + (4 * v_gpe_batch_16252))); 
            }
            {
                
            }
            for (int v_gpe_16254 = 0;(v_gpe_16254 < 4); (++v_gpe_16254)){
                ; 
                LCPQ_POP(v_gpe_16254); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}