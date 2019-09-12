

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arccos_sched(double * v_initial_param_13875_5090, double * & v_user_func_13879_5154, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13922 = 0;(gpe_loop_cvar_13922 < 4); (++gpe_loop_cvar_13922)){
        GPEQ_PUSH(gpe_loop_cvar_13922, reinterpret_cast<uintptr_t>(v_initial_param_13875_5090)); 
        GPEQ_PUSH(gpe_loop_cvar_13922, reinterpret_cast<uintptr_t>(v_user_func_13879_5154)); 
        GPEQ_PUSH(gpe_loop_cvar_13922, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16129 = 0;(v_tile_batch_16129 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16129)){
        int v_virtual_tile_id_16130 = (LCP_TILE_ID() + (v_tile_batch_16129 * 2));
        if ((v_virtual_tile_id_16130 < ((v_N_4617)/(8))))for (int v_gpe_batch_16131 = 0;(v_gpe_batch_16131 <= 1); (++v_gpe_batch_16131)){
            for (int v_gpe_16132 = 0;(v_gpe_16132 < 4); (++v_gpe_16132)){
                GPEQ_PUSH(v_gpe_16132, (v_gpe_16132 + (4 * v_gpe_batch_16131))); 
            }
            {
                
            }
            for (int v_gpe_16133 = 0;(v_gpe_16133 < 4); (++v_gpe_16133)){
                ; 
                LCPQ_POP(v_gpe_16133); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}