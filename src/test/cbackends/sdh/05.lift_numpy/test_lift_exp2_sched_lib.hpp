

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_exp2_sched(double * v_initial_param_1396_1903, double * & v_user_func_1400_1967, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1443 = 0;(gpe_loop_cvar_1443 < 4); (++gpe_loop_cvar_1443)){
        GPEQ_PUSH(gpe_loop_cvar_1443, reinterpret_cast<uintptr_t>(v_initial_param_1396_1903)); 
        GPEQ_PUSH(gpe_loop_cvar_1443, reinterpret_cast<uintptr_t>(v_user_func_1400_1967)); 
        GPEQ_PUSH(gpe_loop_cvar_1443, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2783 = 0;(v_tile_batch_2783 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2783)){
        int v_virtual_tile_id_2784 = (LCP_TILE_ID() + (v_tile_batch_2783 * 2));
        if ((v_virtual_tile_id_2784 < ((v_N_0)/(8))))for (int v_gpe_batch_2785 = 0;(v_gpe_batch_2785 <= 1); (++v_gpe_batch_2785)){
            for (int v_gpe_2786 = 0;(v_gpe_2786 < 4); (++v_gpe_2786)){
                GPEQ_PUSH(v_gpe_2786, (v_gpe_2786 + (4 * v_gpe_batch_2785))); 
            }
            {
                
            }
            for (int v_gpe_2787 = 0;(v_gpe_2787 < 4); (++v_gpe_2787)){
                ; 
                LCPQ_POP(v_gpe_2787); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}