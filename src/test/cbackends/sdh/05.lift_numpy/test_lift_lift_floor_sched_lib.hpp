

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_floor_sched(double * v_initial_param_14536_6178, double * & v_user_func_14540_6242, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14583 = 0;(gpe_loop_cvar_14583 < 4); (++gpe_loop_cvar_14583)){
        GPEQ_PUSH(gpe_loop_cvar_14583, reinterpret_cast<uintptr_t>(v_initial_param_14536_6178)); 
        GPEQ_PUSH(gpe_loop_cvar_14583, reinterpret_cast<uintptr_t>(v_user_func_14540_6242)); 
        GPEQ_PUSH(gpe_loop_cvar_14583, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16305 = 0;(v_tile_batch_16305 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16305)){
        int v_virtual_tile_id_16306 = (LCP_TILE_ID() + (v_tile_batch_16305 * 2));
        if ((v_virtual_tile_id_16306 < ((v_N_4617)/(8))))for (int v_gpe_batch_16307 = 0;(v_gpe_batch_16307 <= 1); (++v_gpe_batch_16307)){
            for (int v_gpe_16308 = 0;(v_gpe_16308 < 4); (++v_gpe_16308)){
                GPEQ_PUSH(v_gpe_16308, (v_gpe_16308 + (4 * v_gpe_batch_16307))); 
            }
            {
                
            }
            for (int v_gpe_16309 = 0;(v_gpe_16309 < 4); (++v_gpe_16309)){
                ; 
                LCPQ_POP(v_gpe_16309); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}