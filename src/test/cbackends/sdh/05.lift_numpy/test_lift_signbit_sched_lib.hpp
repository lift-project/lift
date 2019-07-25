

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_signbit_sched(double * v_initial_param_1726_2311, double * & v_user_func_1730_2375, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1773 = 0;(gpe_loop_cvar_1773 < 4); (++gpe_loop_cvar_1773)){
        GPEQ_PUSH(gpe_loop_cvar_1773, reinterpret_cast<uintptr_t>(v_initial_param_1726_2311)); 
        GPEQ_PUSH(gpe_loop_cvar_1773, reinterpret_cast<uintptr_t>(v_user_func_1730_2375)); 
        GPEQ_PUSH(gpe_loop_cvar_1773, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2849 = 0;(v_tile_batch_2849 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2849)){
        int v_virtual_tile_id_2850 = (LCP_TILE_ID() + (v_tile_batch_2849 * 2));
        if ((v_virtual_tile_id_2850 < ((v_N_0)/(8))))for (int v_gpe_batch_2851 = 0;(v_gpe_batch_2851 <= 1); (++v_gpe_batch_2851)){
            for (int v_gpe_2852 = 0;(v_gpe_2852 < 4); (++v_gpe_2852)){
                GPEQ_PUSH(v_gpe_2852, (v_gpe_2852 + (4 * v_gpe_batch_2851))); 
            }
            {
                
            }
            for (int v_gpe_2853 = 0;(v_gpe_2853 < 4); (++v_gpe_2853)){
                ; 
                LCPQ_POP(v_gpe_2853); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}