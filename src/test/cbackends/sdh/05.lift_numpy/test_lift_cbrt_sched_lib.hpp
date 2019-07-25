

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_cbrt_sched(double * v_initial_param_2218_2651, double * & v_user_func_2222_2715, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_2265 = 0;(gpe_loop_cvar_2265 < 4); (++gpe_loop_cvar_2265)){
        GPEQ_PUSH(gpe_loop_cvar_2265, reinterpret_cast<uintptr_t>(v_initial_param_2218_2651)); 
        GPEQ_PUSH(gpe_loop_cvar_2265, reinterpret_cast<uintptr_t>(v_user_func_2222_2715)); 
        GPEQ_PUSH(gpe_loop_cvar_2265, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2904 = 0;(v_tile_batch_2904 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2904)){
        int v_virtual_tile_id_2905 = (LCP_TILE_ID() + (v_tile_batch_2904 * 2));
        if ((v_virtual_tile_id_2905 < ((v_N_0)/(8))))for (int v_gpe_batch_2906 = 0;(v_gpe_batch_2906 <= 1); (++v_gpe_batch_2906)){
            for (int v_gpe_2907 = 0;(v_gpe_2907 < 4); (++v_gpe_2907)){
                GPEQ_PUSH(v_gpe_2907, (v_gpe_2907 + (4 * v_gpe_batch_2906))); 
            }
            {
                
            }
            for (int v_gpe_2908 = 0;(v_gpe_2908 < 4); (++v_gpe_2908)){
                ; 
                LCPQ_POP(v_gpe_2908); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}