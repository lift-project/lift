

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_positive_sched(double * v_initial_param_1871_2447, double * & v_user_func_1875_2511, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1918 = 0;(gpe_loop_cvar_1918 < 4); (++gpe_loop_cvar_1918)){
        GPEQ_PUSH(gpe_loop_cvar_1918, reinterpret_cast<uintptr_t>(v_initial_param_1871_2447)); 
        GPEQ_PUSH(gpe_loop_cvar_1918, reinterpret_cast<uintptr_t>(v_user_func_1875_2511)); 
        GPEQ_PUSH(gpe_loop_cvar_1918, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2871 = 0;(v_tile_batch_2871 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2871)){
        int v_virtual_tile_id_2872 = (LCP_TILE_ID() + (v_tile_batch_2871 * 2));
        if ((v_virtual_tile_id_2872 < ((v_N_0)/(8))))for (int v_gpe_batch_2873 = 0;(v_gpe_batch_2873 <= 1); (++v_gpe_batch_2873)){
            for (int v_gpe_2874 = 0;(v_gpe_2874 < 4); (++v_gpe_2874)){
                GPEQ_PUSH(v_gpe_2874, (v_gpe_2874 + (4 * v_gpe_batch_2873))); 
            }
            {
                
            }
            for (int v_gpe_2875 = 0;(v_gpe_2875 < 4); (++v_gpe_2875)){
                ; 
                LCPQ_POP(v_gpe_2875); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}