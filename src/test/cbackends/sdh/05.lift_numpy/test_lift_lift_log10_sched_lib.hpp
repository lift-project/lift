

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_log10_sched(double * v_initial_param_15069_6654, double * & v_user_func_15073_6718, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15116 = 0;(gpe_loop_cvar_15116 < 4); (++gpe_loop_cvar_15116)){
        GPEQ_PUSH(gpe_loop_cvar_15116, reinterpret_cast<uintptr_t>(v_initial_param_15069_6654)); 
        GPEQ_PUSH(gpe_loop_cvar_15116, reinterpret_cast<uintptr_t>(v_user_func_15073_6718)); 
        GPEQ_PUSH(gpe_loop_cvar_15116, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16382 = 0;(v_tile_batch_16382 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16382)){
        int v_virtual_tile_id_16383 = (LCP_TILE_ID() + (v_tile_batch_16382 * 2));
        if ((v_virtual_tile_id_16383 < ((v_N_4617)/(8))))for (int v_gpe_batch_16384 = 0;(v_gpe_batch_16384 <= 1); (++v_gpe_batch_16384)){
            for (int v_gpe_16385 = 0;(v_gpe_16385 < 4); (++v_gpe_16385)){
                GPEQ_PUSH(v_gpe_16385, (v_gpe_16385 + (4 * v_gpe_batch_16384))); 
            }
            {
                
            }
            for (int v_gpe_16386 = 0;(v_gpe_16386 < 4); (++v_gpe_16386)){
                ; 
                LCPQ_POP(v_gpe_16386); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}