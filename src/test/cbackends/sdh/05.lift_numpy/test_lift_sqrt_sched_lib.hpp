

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sqrt_sched(double * v_initial_param_2170_2583, double * & v_user_func_2174_2647, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_2217 = 0;(gpe_loop_cvar_2217 < 4); (++gpe_loop_cvar_2217)){
        GPEQ_PUSH(gpe_loop_cvar_2217, reinterpret_cast<uintptr_t>(v_initial_param_2170_2583)); 
        GPEQ_PUSH(gpe_loop_cvar_2217, reinterpret_cast<uintptr_t>(v_user_func_2174_2647)); 
        GPEQ_PUSH(gpe_loop_cvar_2217, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2893 = 0;(v_tile_batch_2893 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2893)){
        int v_virtual_tile_id_2894 = (LCP_TILE_ID() + (v_tile_batch_2893 * 2));
        if ((v_virtual_tile_id_2894 < ((v_N_0)/(8))))for (int v_gpe_batch_2895 = 0;(v_gpe_batch_2895 <= 1); (++v_gpe_batch_2895)){
            for (int v_gpe_2896 = 0;(v_gpe_2896 < 4); (++v_gpe_2896)){
                GPEQ_PUSH(v_gpe_2896, (v_gpe_2896 + (4 * v_gpe_batch_2895))); 
            }
            {
                
            }
            for (int v_gpe_2897 = 0;(v_gpe_2897 < 4); (++v_gpe_2897)){
                ; 
                LCPQ_POP(v_gpe_2897); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}