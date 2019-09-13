

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_trunc_sched(double * v_initial_param_14632_6314, double * & v_user_func_14636_6378, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14679 = 0;(gpe_loop_cvar_14679 < 4); (++gpe_loop_cvar_14679)){
        GPEQ_PUSH(gpe_loop_cvar_14679, reinterpret_cast<uintptr_t>(v_initial_param_14632_6314)); 
        GPEQ_PUSH(gpe_loop_cvar_14679, reinterpret_cast<uintptr_t>(v_user_func_14636_6378)); 
        GPEQ_PUSH(gpe_loop_cvar_14679, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16327 = 0;(v_tile_batch_16327 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16327)){
        int v_virtual_tile_id_16328 = (LCP_TILE_ID() + (v_tile_batch_16327 * 2));
        if ((v_virtual_tile_id_16328 < ((v_N_4617)/(8))))for (int v_gpe_batch_16329 = 0;(v_gpe_batch_16329 <= 1); (++v_gpe_batch_16329)){
            for (int v_gpe_16330 = 0;(v_gpe_16330 < 4); (++v_gpe_16330)){
                GPEQ_PUSH(v_gpe_16330, (v_gpe_16330 + (4 * v_gpe_batch_16329))); 
            }
            {
                
            }
            for (int v_gpe_16331 = 0;(v_gpe_16331 < 4); (++v_gpe_16331)){
                ; 
                LCPQ_POP(v_gpe_16331); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}