

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_degrees_sched(double * v_initial_param_397_611, double * & v_user_func_401_675, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_444 = 0;(gpe_loop_cvar_444 < 4); (++gpe_loop_cvar_444)){
        GPEQ_PUSH(gpe_loop_cvar_444, reinterpret_cast<uintptr_t>(v_initial_param_397_611)); 
        GPEQ_PUSH(gpe_loop_cvar_444, reinterpret_cast<uintptr_t>(v_user_func_401_675)); 
        GPEQ_PUSH(gpe_loop_cvar_444, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2574 = 0;(v_tile_batch_2574 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2574)){
        int v_virtual_tile_id_2575 = (LCP_TILE_ID() + (v_tile_batch_2574 * 2));
        if ((v_virtual_tile_id_2575 < ((v_N_0)/(8))))for (int v_gpe_batch_2576 = 0;(v_gpe_batch_2576 <= 1); (++v_gpe_batch_2576)){
            for (int v_gpe_2577 = 0;(v_gpe_2577 < 4); (++v_gpe_2577)){
                GPEQ_PUSH(v_gpe_2577, (v_gpe_2577 + (4 * v_gpe_batch_2576))); 
            }
            {
                
            }
            for (int v_gpe_2578 = 0;(v_gpe_2578 < 4); (++v_gpe_2578)){
                ; 
                LCPQ_POP(v_gpe_2578); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}