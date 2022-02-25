

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_log10_sched(double * v_initial_param_1492_2039, double * & v_user_func_1496_2103, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1539 = 0;(gpe_loop_cvar_1539 < 4); (++gpe_loop_cvar_1539)){
        GPEQ_PUSH(gpe_loop_cvar_1539, reinterpret_cast<uintptr_t>(v_initial_param_1492_2039)); 
        GPEQ_PUSH(gpe_loop_cvar_1539, reinterpret_cast<uintptr_t>(v_user_func_1496_2103)); 
        GPEQ_PUSH(gpe_loop_cvar_1539, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2805 = 0;(v_tile_batch_2805 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2805)){
        int v_virtual_tile_id_2806 = (LCP_TILE_ID() + (v_tile_batch_2805 * 2));
        if ((v_virtual_tile_id_2806 < ((v_N_0)/(8))))for (int v_gpe_batch_2807 = 0;(v_gpe_batch_2807 <= 1); (++v_gpe_batch_2807)){
            for (int v_gpe_2808 = 0;(v_gpe_2808 < 4); (++v_gpe_2808)){
                GPEQ_PUSH(v_gpe_2808, (v_gpe_2808 + (4 * v_gpe_batch_2807))); 
            }
            {
                
            }
            for (int v_gpe_2809 = 0;(v_gpe_2809 < 4); (++v_gpe_2809)){
                ; 
                LCPQ_POP(v_gpe_2809); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}