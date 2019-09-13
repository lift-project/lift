

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arccosh_sched(double * v_initial_param_14296_5770, double * & v_user_func_14300_5834, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14343 = 0;(gpe_loop_cvar_14343 < 4); (++gpe_loop_cvar_14343)){
        GPEQ_PUSH(gpe_loop_cvar_14343, reinterpret_cast<uintptr_t>(v_initial_param_14296_5770)); 
        GPEQ_PUSH(gpe_loop_cvar_14343, reinterpret_cast<uintptr_t>(v_user_func_14300_5834)); 
        GPEQ_PUSH(gpe_loop_cvar_14343, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16239 = 0;(v_tile_batch_16239 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16239)){
        int v_virtual_tile_id_16240 = (LCP_TILE_ID() + (v_tile_batch_16239 * 2));
        if ((v_virtual_tile_id_16240 < ((v_N_4617)/(8))))for (int v_gpe_batch_16241 = 0;(v_gpe_batch_16241 <= 1); (++v_gpe_batch_16241)){
            for (int v_gpe_16242 = 0;(v_gpe_16242 < 4); (++v_gpe_16242)){
                GPEQ_PUSH(v_gpe_16242, (v_gpe_16242 + (4 * v_gpe_batch_16241))); 
            }
            {
                
            }
            for (int v_gpe_16243 = 0;(v_gpe_16243 < 4); (++v_gpe_16243)){
                ; 
                LCPQ_POP(v_gpe_16243); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}