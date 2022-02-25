

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sinc_sched(double * v_initial_param_1678_2243, double * & v_user_func_1682_2307, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1725 = 0;(gpe_loop_cvar_1725 < 4); (++gpe_loop_cvar_1725)){
        GPEQ_PUSH(gpe_loop_cvar_1725, reinterpret_cast<uintptr_t>(v_initial_param_1678_2243)); 
        GPEQ_PUSH(gpe_loop_cvar_1725, reinterpret_cast<uintptr_t>(v_user_func_1682_2307)); 
        GPEQ_PUSH(gpe_loop_cvar_1725, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2838 = 0;(v_tile_batch_2838 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2838)){
        int v_virtual_tile_id_2839 = (LCP_TILE_ID() + (v_tile_batch_2838 * 2));
        if ((v_virtual_tile_id_2839 < ((v_N_0)/(8))))for (int v_gpe_batch_2840 = 0;(v_gpe_batch_2840 <= 1); (++v_gpe_batch_2840)){
            for (int v_gpe_2841 = 0;(v_gpe_2841 < 4); (++v_gpe_2841)){
                GPEQ_PUSH(v_gpe_2841, (v_gpe_2841 + (4 * v_gpe_batch_2840))); 
            }
            {
                
            }
            for (int v_gpe_2842 = 0;(v_gpe_2842 < 4); (++v_gpe_2842)){
                ; 
                LCPQ_POP(v_gpe_2842); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}