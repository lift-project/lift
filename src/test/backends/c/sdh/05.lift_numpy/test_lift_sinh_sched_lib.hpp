

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sinh_sched(double * v_initial_param_14104_5498, double * & v_user_func_14108_5562, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14151 = 0;(gpe_loop_cvar_14151 < 4); (++gpe_loop_cvar_14151)){
        GPEQ_PUSH(gpe_loop_cvar_14151, reinterpret_cast<uintptr_t>(v_initial_param_14104_5498)); 
        GPEQ_PUSH(gpe_loop_cvar_14151, reinterpret_cast<uintptr_t>(v_user_func_14108_5562)); 
        GPEQ_PUSH(gpe_loop_cvar_14151, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16195 = 0;(v_tile_batch_16195 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16195)){
        int v_virtual_tile_id_16196 = (LCP_TILE_ID() + (v_tile_batch_16195 * 2));
        if ((v_virtual_tile_id_16196 < ((v_N_4617)/(8))))for (int v_gpe_batch_16197 = 0;(v_gpe_batch_16197 <= 1); (++v_gpe_batch_16197)){
            for (int v_gpe_16198 = 0;(v_gpe_16198 < 4); (++v_gpe_16198)){
                GPEQ_PUSH(v_gpe_16198, (v_gpe_16198 + (4 * v_gpe_batch_16197))); 
            }
            {
                
            }
            for (int v_gpe_16199 = 0;(v_gpe_16199 < 4); (++v_gpe_16199)){
                ; 
                LCPQ_POP(v_gpe_16199); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}