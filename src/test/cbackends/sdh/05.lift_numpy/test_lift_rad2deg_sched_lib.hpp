

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_rad2deg_sched(double * v_initial_param_397_815, double * & v_user_func_401_879, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_444 = 0;(gpe_loop_cvar_444 < 4); (++gpe_loop_cvar_444)){
        GPEQ_PUSH(gpe_loop_cvar_444, reinterpret_cast<uintptr_t>(v_initial_param_397_815)); 
        GPEQ_PUSH(gpe_loop_cvar_444, reinterpret_cast<uintptr_t>(v_user_func_401_879)); 
        GPEQ_PUSH(gpe_loop_cvar_444, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2607 = 0;(v_tile_batch_2607 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2607)){
        int v_virtual_tile_id_2608 = (LCP_TILE_ID() + (v_tile_batch_2607 * 2));
        if ((v_virtual_tile_id_2608 < ((v_N_0)/(8))))for (int v_gpe_batch_2609 = 0;(v_gpe_batch_2609 <= 1); (++v_gpe_batch_2609)){
            for (int v_gpe_2610 = 0;(v_gpe_2610 < 4); (++v_gpe_2610)){
                GPEQ_PUSH(v_gpe_2610, (v_gpe_2610 + (4 * v_gpe_batch_2609))); 
            }
            {
                
            }
            for (int v_gpe_2611 = 0;(v_gpe_2611 < 4); (++v_gpe_2611)){
                ; 
                LCPQ_POP(v_gpe_2611); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}