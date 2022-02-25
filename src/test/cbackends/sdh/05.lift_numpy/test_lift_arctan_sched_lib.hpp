

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arctan_sched(double * v_initial_param_312_543, double * & v_user_func_316_607, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_359 = 0;(gpe_loop_cvar_359 < 4); (++gpe_loop_cvar_359)){
        GPEQ_PUSH(gpe_loop_cvar_359, reinterpret_cast<uintptr_t>(v_initial_param_312_543)); 
        GPEQ_PUSH(gpe_loop_cvar_359, reinterpret_cast<uintptr_t>(v_user_func_316_607)); 
        GPEQ_PUSH(gpe_loop_cvar_359, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2563 = 0;(v_tile_batch_2563 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2563)){
        int v_virtual_tile_id_2564 = (LCP_TILE_ID() + (v_tile_batch_2563 * 2));
        if ((v_virtual_tile_id_2564 < ((v_N_0)/(8))))for (int v_gpe_batch_2565 = 0;(v_gpe_batch_2565 <= 1); (++v_gpe_batch_2565)){
            for (int v_gpe_2566 = 0;(v_gpe_2566 < 4); (++v_gpe_2566)){
                GPEQ_PUSH(v_gpe_2566, (v_gpe_2566 + (4 * v_gpe_batch_2565))); 
            }
            {
                
            }
            for (int v_gpe_2567 = 0;(v_gpe_2567 < 4); (++v_gpe_2567)){
                ; 
                LCPQ_POP(v_gpe_2567); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}