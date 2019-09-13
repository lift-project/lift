

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_fix_sched(double * v_initial_param_14488_6110, double * & v_user_func_14492_6174, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14535 = 0;(gpe_loop_cvar_14535 < 4); (++gpe_loop_cvar_14535)){
        GPEQ_PUSH(gpe_loop_cvar_14535, reinterpret_cast<uintptr_t>(v_initial_param_14488_6110)); 
        GPEQ_PUSH(gpe_loop_cvar_14535, reinterpret_cast<uintptr_t>(v_user_func_14492_6174)); 
        GPEQ_PUSH(gpe_loop_cvar_14535, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16294 = 0;(v_tile_batch_16294 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16294)){
        int v_virtual_tile_id_16295 = (LCP_TILE_ID() + (v_tile_batch_16294 * 2));
        if ((v_virtual_tile_id_16295 < ((v_N_4617)/(8))))for (int v_gpe_batch_16296 = 0;(v_gpe_batch_16296 <= 1); (++v_gpe_batch_16296)){
            for (int v_gpe_16297 = 0;(v_gpe_16297 < 4); (++v_gpe_16297)){
                GPEQ_PUSH(v_gpe_16297, (v_gpe_16297 + (4 * v_gpe_batch_16296))); 
            }
            {
                
            }
            for (int v_gpe_16298 = 0;(v_gpe_16298 < 4); (++v_gpe_16298)){
                ; 
                LCPQ_POP(v_gpe_16298); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}