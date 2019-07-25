

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_round__sched(double * v_initial_param_781_1359, double * & v_user_func_785_1423, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_828 = 0;(gpe_loop_cvar_828 < 4); (++gpe_loop_cvar_828)){
        GPEQ_PUSH(gpe_loop_cvar_828, reinterpret_cast<uintptr_t>(v_initial_param_781_1359)); 
        GPEQ_PUSH(gpe_loop_cvar_828, reinterpret_cast<uintptr_t>(v_user_func_785_1423)); 
        GPEQ_PUSH(gpe_loop_cvar_828, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2695 = 0;(v_tile_batch_2695 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2695)){
        int v_virtual_tile_id_2696 = (LCP_TILE_ID() + (v_tile_batch_2695 * 2));
        if ((v_virtual_tile_id_2696 < ((v_N_0)/(8))))for (int v_gpe_batch_2697 = 0;(v_gpe_batch_2697 <= 1); (++v_gpe_batch_2697)){
            for (int v_gpe_2698 = 0;(v_gpe_2698 < 4); (++v_gpe_2698)){
                GPEQ_PUSH(v_gpe_2698, (v_gpe_2698 + (4 * v_gpe_batch_2697))); 
            }
            {
                
            }
            for (int v_gpe_2699 = 0;(v_gpe_2699 < 4); (++v_gpe_2699)){
                ; 
                LCPQ_POP(v_gpe_2699); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}