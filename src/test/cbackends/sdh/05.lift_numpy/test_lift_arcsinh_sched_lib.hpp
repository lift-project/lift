

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arcsinh_sched(double * v_initial_param_14248_5702, double * & v_user_func_14252_5766, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14295 = 0;(gpe_loop_cvar_14295 < 4); (++gpe_loop_cvar_14295)){
        GPEQ_PUSH(gpe_loop_cvar_14295, reinterpret_cast<uintptr_t>(v_initial_param_14248_5702)); 
        GPEQ_PUSH(gpe_loop_cvar_14295, reinterpret_cast<uintptr_t>(v_user_func_14252_5766)); 
        GPEQ_PUSH(gpe_loop_cvar_14295, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16228 = 0;(v_tile_batch_16228 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16228)){
        int v_virtual_tile_id_16229 = (LCP_TILE_ID() + (v_tile_batch_16228 * 2));
        if ((v_virtual_tile_id_16229 < ((v_N_4617)/(8))))for (int v_gpe_batch_16230 = 0;(v_gpe_batch_16230 <= 1); (++v_gpe_batch_16230)){
            for (int v_gpe_16231 = 0;(v_gpe_16231 < 4); (++v_gpe_16231)){
                GPEQ_PUSH(v_gpe_16231, (v_gpe_16231 + (4 * v_gpe_batch_16230))); 
            }
            {
                
            }
            for (int v_gpe_16232 = 0;(v_gpe_16232 < 4); (++v_gpe_16232)){
                ; 
                LCPQ_POP(v_gpe_16232); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}