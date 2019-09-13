

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arctan_sched(double * v_initial_param_13923_5158, double * & v_user_func_13927_5222, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13970 = 0;(gpe_loop_cvar_13970 < 4); (++gpe_loop_cvar_13970)){
        GPEQ_PUSH(gpe_loop_cvar_13970, reinterpret_cast<uintptr_t>(v_initial_param_13923_5158)); 
        GPEQ_PUSH(gpe_loop_cvar_13970, reinterpret_cast<uintptr_t>(v_user_func_13927_5222)); 
        GPEQ_PUSH(gpe_loop_cvar_13970, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16140 = 0;(v_tile_batch_16140 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16140)){
        int v_virtual_tile_id_16141 = (LCP_TILE_ID() + (v_tile_batch_16140 * 2));
        if ((v_virtual_tile_id_16141 < ((v_N_4617)/(8))))for (int v_gpe_batch_16142 = 0;(v_gpe_batch_16142 <= 1); (++v_gpe_batch_16142)){
            for (int v_gpe_16143 = 0;(v_gpe_16143 < 4); (++v_gpe_16143)){
                GPEQ_PUSH(v_gpe_16143, (v_gpe_16143 + (4 * v_gpe_batch_16142))); 
            }
            {
                
            }
            for (int v_gpe_16144 = 0;(v_gpe_16144 < 4); (++v_gpe_16144)){
                ; 
                LCPQ_POP(v_gpe_16144); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}