

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_floor_sched(double * v_initial_param_925_1563, double * & v_user_func_929_1627, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_972 = 0;(gpe_loop_cvar_972 < 4); (++gpe_loop_cvar_972)){
        GPEQ_PUSH(gpe_loop_cvar_972, reinterpret_cast<uintptr_t>(v_initial_param_925_1563)); 
        GPEQ_PUSH(gpe_loop_cvar_972, reinterpret_cast<uintptr_t>(v_user_func_929_1627)); 
        GPEQ_PUSH(gpe_loop_cvar_972, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2728 = 0;(v_tile_batch_2728 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2728)){
        int v_virtual_tile_id_2729 = (LCP_TILE_ID() + (v_tile_batch_2728 * 2));
        if ((v_virtual_tile_id_2729 < ((v_N_0)/(8))))for (int v_gpe_batch_2730 = 0;(v_gpe_batch_2730 <= 1); (++v_gpe_batch_2730)){
            for (int v_gpe_2731 = 0;(v_gpe_2731 < 4); (++v_gpe_2731)){
                GPEQ_PUSH(v_gpe_2731, (v_gpe_2731 + (4 * v_gpe_batch_2730))); 
            }
            {
                
            }
            for (int v_gpe_2732 = 0;(v_gpe_2732 < 4); (++v_gpe_2732)){
                ; 
                LCPQ_POP(v_gpe_2732); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}