

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_exp2_sched(double * v_initial_param_14973_6518, double * & v_user_func_14977_6582, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15020 = 0;(gpe_loop_cvar_15020 < 4); (++gpe_loop_cvar_15020)){
        GPEQ_PUSH(gpe_loop_cvar_15020, reinterpret_cast<uintptr_t>(v_initial_param_14973_6518)); 
        GPEQ_PUSH(gpe_loop_cvar_15020, reinterpret_cast<uintptr_t>(v_user_func_14977_6582)); 
        GPEQ_PUSH(gpe_loop_cvar_15020, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16360 = 0;(v_tile_batch_16360 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16360)){
        int v_virtual_tile_id_16361 = (LCP_TILE_ID() + (v_tile_batch_16360 * 2));
        if ((v_virtual_tile_id_16361 < ((v_N_4617)/(8))))for (int v_gpe_batch_16362 = 0;(v_gpe_batch_16362 <= 1); (++v_gpe_batch_16362)){
            for (int v_gpe_16363 = 0;(v_gpe_16363 < 4); (++v_gpe_16363)){
                GPEQ_PUSH(v_gpe_16363, (v_gpe_16363 + (4 * v_gpe_batch_16362))); 
            }
            {
                
            }
            for (int v_gpe_16364 = 0;(v_gpe_16364 < 4); (++v_gpe_16364)){
                ; 
                LCPQ_POP(v_gpe_16364); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}