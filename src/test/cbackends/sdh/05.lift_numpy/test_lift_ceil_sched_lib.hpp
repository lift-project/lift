

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_ceil_sched(double * v_initial_param_14584_6246, double * & v_user_func_14588_6310, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14631 = 0;(gpe_loop_cvar_14631 < 4); (++gpe_loop_cvar_14631)){
        GPEQ_PUSH(gpe_loop_cvar_14631, reinterpret_cast<uintptr_t>(v_initial_param_14584_6246)); 
        GPEQ_PUSH(gpe_loop_cvar_14631, reinterpret_cast<uintptr_t>(v_user_func_14588_6310)); 
        GPEQ_PUSH(gpe_loop_cvar_14631, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16316 = 0;(v_tile_batch_16316 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16316)){
        int v_virtual_tile_id_16317 = (LCP_TILE_ID() + (v_tile_batch_16316 * 2));
        if ((v_virtual_tile_id_16317 < ((v_N_4617)/(8))))for (int v_gpe_batch_16318 = 0;(v_gpe_batch_16318 <= 1); (++v_gpe_batch_16318)){
            for (int v_gpe_16319 = 0;(v_gpe_16319 < 4); (++v_gpe_16319)){
                GPEQ_PUSH(v_gpe_16319, (v_gpe_16319 + (4 * v_gpe_batch_16318))); 
            }
            {
                
            }
            for (int v_gpe_16320 = 0;(v_gpe_16320 < 4); (++v_gpe_16320)){
                ; 
                LCPQ_POP(v_gpe_16320); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}