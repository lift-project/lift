

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arccosh_sched(double * v_initial_param_685_1155, double * & v_user_func_689_1219, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_732 = 0;(gpe_loop_cvar_732 < 4); (++gpe_loop_cvar_732)){
        GPEQ_PUSH(gpe_loop_cvar_732, reinterpret_cast<uintptr_t>(v_initial_param_685_1155)); 
        GPEQ_PUSH(gpe_loop_cvar_732, reinterpret_cast<uintptr_t>(v_user_func_689_1219)); 
        GPEQ_PUSH(gpe_loop_cvar_732, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2662 = 0;(v_tile_batch_2662 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2662)){
        int v_virtual_tile_id_2663 = (LCP_TILE_ID() + (v_tile_batch_2662 * 2));
        if ((v_virtual_tile_id_2663 < ((v_N_0)/(8))))for (int v_gpe_batch_2664 = 0;(v_gpe_batch_2664 <= 1); (++v_gpe_batch_2664)){
            for (int v_gpe_2665 = 0;(v_gpe_2665 < 4); (++v_gpe_2665)){
                GPEQ_PUSH(v_gpe_2665, (v_gpe_2665 + (4 * v_gpe_batch_2664))); 
            }
            {
                
            }
            for (int v_gpe_2666 = 0;(v_gpe_2666 < 4); (++v_gpe_2666)){
                ; 
                LCPQ_POP(v_gpe_2666); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}