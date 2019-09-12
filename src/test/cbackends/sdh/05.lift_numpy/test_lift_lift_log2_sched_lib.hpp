

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_log2_sched(double * v_initial_param_15117_6722, double * & v_user_func_15121_6786, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15164 = 0;(gpe_loop_cvar_15164 < 4); (++gpe_loop_cvar_15164)){
        GPEQ_PUSH(gpe_loop_cvar_15164, reinterpret_cast<uintptr_t>(v_initial_param_15117_6722)); 
        GPEQ_PUSH(gpe_loop_cvar_15164, reinterpret_cast<uintptr_t>(v_user_func_15121_6786)); 
        GPEQ_PUSH(gpe_loop_cvar_15164, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16393 = 0;(v_tile_batch_16393 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16393)){
        int v_virtual_tile_id_16394 = (LCP_TILE_ID() + (v_tile_batch_16393 * 2));
        if ((v_virtual_tile_id_16394 < ((v_N_4617)/(8))))for (int v_gpe_batch_16395 = 0;(v_gpe_batch_16395 <= 1); (++v_gpe_batch_16395)){
            for (int v_gpe_16396 = 0;(v_gpe_16396 < 4); (++v_gpe_16396)){
                GPEQ_PUSH(v_gpe_16396, (v_gpe_16396 + (4 * v_gpe_batch_16395))); 
            }
            {
                
            }
            for (int v_gpe_16397 = 0;(v_gpe_16397 < 4); (++v_gpe_16397)){
                ; 
                LCPQ_POP(v_gpe_16397); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}