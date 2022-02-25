

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_tanh_sched(double * v_initial_param_589_1019, double * & v_user_func_593_1083, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_636 = 0;(gpe_loop_cvar_636 < 4); (++gpe_loop_cvar_636)){
        GPEQ_PUSH(gpe_loop_cvar_636, reinterpret_cast<uintptr_t>(v_initial_param_589_1019)); 
        GPEQ_PUSH(gpe_loop_cvar_636, reinterpret_cast<uintptr_t>(v_user_func_593_1083)); 
        GPEQ_PUSH(gpe_loop_cvar_636, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2640 = 0;(v_tile_batch_2640 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2640)){
        int v_virtual_tile_id_2641 = (LCP_TILE_ID() + (v_tile_batch_2640 * 2));
        if ((v_virtual_tile_id_2641 < ((v_N_0)/(8))))for (int v_gpe_batch_2642 = 0;(v_gpe_batch_2642 <= 1); (++v_gpe_batch_2642)){
            for (int v_gpe_2643 = 0;(v_gpe_2643 < 4); (++v_gpe_2643)){
                GPEQ_PUSH(v_gpe_2643, (v_gpe_2643 + (4 * v_gpe_batch_2642))); 
            }
            {
                
            }
            for (int v_gpe_2644 = 0;(v_gpe_2644 < 4); (++v_gpe_2644)){
                ; 
                LCPQ_POP(v_gpe_2644); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}