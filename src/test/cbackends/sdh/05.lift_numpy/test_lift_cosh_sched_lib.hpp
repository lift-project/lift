

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_cosh_sched(double * v_initial_param_541_951, double * & v_user_func_545_1015, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_588 = 0;(gpe_loop_cvar_588 < 4); (++gpe_loop_cvar_588)){
        GPEQ_PUSH(gpe_loop_cvar_588, reinterpret_cast<uintptr_t>(v_initial_param_541_951)); 
        GPEQ_PUSH(gpe_loop_cvar_588, reinterpret_cast<uintptr_t>(v_user_func_545_1015)); 
        GPEQ_PUSH(gpe_loop_cvar_588, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2629 = 0;(v_tile_batch_2629 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2629)){
        int v_virtual_tile_id_2630 = (LCP_TILE_ID() + (v_tile_batch_2629 * 2));
        if ((v_virtual_tile_id_2630 < ((v_N_0)/(8))))for (int v_gpe_batch_2631 = 0;(v_gpe_batch_2631 <= 1); (++v_gpe_batch_2631)){
            for (int v_gpe_2632 = 0;(v_gpe_2632 < 4); (++v_gpe_2632)){
                GPEQ_PUSH(v_gpe_2632, (v_gpe_2632 + (4 * v_gpe_batch_2631))); 
            }
            {
                
            }
            for (int v_gpe_2633 = 0;(v_gpe_2633 < 4); (++v_gpe_2633)){
                ; 
                LCPQ_POP(v_gpe_2633); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}