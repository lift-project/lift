

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sinh_sched(double * v_initial_param_493_883, double * & v_user_func_497_947, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_540 = 0;(gpe_loop_cvar_540 < 4); (++gpe_loop_cvar_540)){
        GPEQ_PUSH(gpe_loop_cvar_540, reinterpret_cast<uintptr_t>(v_initial_param_493_883)); 
        GPEQ_PUSH(gpe_loop_cvar_540, reinterpret_cast<uintptr_t>(v_user_func_497_947)); 
        GPEQ_PUSH(gpe_loop_cvar_540, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2618 = 0;(v_tile_batch_2618 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2618)){
        int v_virtual_tile_id_2619 = (LCP_TILE_ID() + (v_tile_batch_2618 * 2));
        if ((v_virtual_tile_id_2619 < ((v_N_0)/(8))))for (int v_gpe_batch_2620 = 0;(v_gpe_batch_2620 <= 1); (++v_gpe_batch_2620)){
            for (int v_gpe_2621 = 0;(v_gpe_2621 < 4); (++v_gpe_2621)){
                GPEQ_PUSH(v_gpe_2621, (v_gpe_2621 + (4 * v_gpe_batch_2620))); 
            }
            {
                
            }
            for (int v_gpe_2622 = 0;(v_gpe_2622 < 4); (++v_gpe_2622)){
                ; 
                LCPQ_POP(v_gpe_2622); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}