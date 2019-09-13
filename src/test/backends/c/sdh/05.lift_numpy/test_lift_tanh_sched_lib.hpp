

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_tanh_sched(double * v_initial_param_14200_5634, double * & v_user_func_14204_5698, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14247 = 0;(gpe_loop_cvar_14247 < 4); (++gpe_loop_cvar_14247)){
        GPEQ_PUSH(gpe_loop_cvar_14247, reinterpret_cast<uintptr_t>(v_initial_param_14200_5634)); 
        GPEQ_PUSH(gpe_loop_cvar_14247, reinterpret_cast<uintptr_t>(v_user_func_14204_5698)); 
        GPEQ_PUSH(gpe_loop_cvar_14247, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16217 = 0;(v_tile_batch_16217 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16217)){
        int v_virtual_tile_id_16218 = (LCP_TILE_ID() + (v_tile_batch_16217 * 2));
        if ((v_virtual_tile_id_16218 < ((v_N_4617)/(8))))for (int v_gpe_batch_16219 = 0;(v_gpe_batch_16219 <= 1); (++v_gpe_batch_16219)){
            for (int v_gpe_16220 = 0;(v_gpe_16220 < 4); (++v_gpe_16220)){
                GPEQ_PUSH(v_gpe_16220, (v_gpe_16220 + (4 * v_gpe_batch_16219))); 
            }
            {
                
            }
            for (int v_gpe_16221 = 0;(v_gpe_16221 < 4); (++v_gpe_16221)){
                ; 
                LCPQ_POP(v_gpe_16221); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}