

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_square_sched(double * v_initial_param_15843_7334, double * & v_user_func_15847_7398, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15890 = 0;(gpe_loop_cvar_15890 < 4); (++gpe_loop_cvar_15890)){
        GPEQ_PUSH(gpe_loop_cvar_15890, reinterpret_cast<uintptr_t>(v_initial_param_15843_7334)); 
        GPEQ_PUSH(gpe_loop_cvar_15890, reinterpret_cast<uintptr_t>(v_user_func_15847_7398)); 
        GPEQ_PUSH(gpe_loop_cvar_15890, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16492 = 0;(v_tile_batch_16492 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16492)){
        int v_virtual_tile_id_16493 = (LCP_TILE_ID() + (v_tile_batch_16492 * 2));
        if ((v_virtual_tile_id_16493 < ((v_N_4617)/(8))))for (int v_gpe_batch_16494 = 0;(v_gpe_batch_16494 <= 1); (++v_gpe_batch_16494)){
            for (int v_gpe_16495 = 0;(v_gpe_16495 < 4); (++v_gpe_16495)){
                GPEQ_PUSH(v_gpe_16495, (v_gpe_16495 + (4 * v_gpe_batch_16494))); 
            }
            {
                
            }
            for (int v_gpe_16496 = 0;(v_gpe_16496 < 4); (++v_gpe_16496)){
                ; 
                LCPQ_POP(v_gpe_16496); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}