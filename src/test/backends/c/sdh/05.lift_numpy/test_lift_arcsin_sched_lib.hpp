

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arcsin_sched(double * v_initial_param_13827_5022, double * & v_user_func_13831_5086, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13874 = 0;(gpe_loop_cvar_13874 < 4); (++gpe_loop_cvar_13874)){
        GPEQ_PUSH(gpe_loop_cvar_13874, reinterpret_cast<uintptr_t>(v_initial_param_13827_5022)); 
        GPEQ_PUSH(gpe_loop_cvar_13874, reinterpret_cast<uintptr_t>(v_user_func_13831_5086)); 
        GPEQ_PUSH(gpe_loop_cvar_13874, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16118 = 0;(v_tile_batch_16118 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16118)){
        int v_virtual_tile_id_16119 = (LCP_TILE_ID() + (v_tile_batch_16118 * 2));
        if ((v_virtual_tile_id_16119 < ((v_N_4617)/(8))))for (int v_gpe_batch_16120 = 0;(v_gpe_batch_16120 <= 1); (++v_gpe_batch_16120)){
            for (int v_gpe_16121 = 0;(v_gpe_16121 < 4); (++v_gpe_16121)){
                GPEQ_PUSH(v_gpe_16121, (v_gpe_16121 + (4 * v_gpe_batch_16120))); 
            }
            {
                
            }
            for (int v_gpe_16122 = 0;(v_gpe_16122 < 4); (++v_gpe_16122)){
                ; 
                LCPQ_POP(v_gpe_16122); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}