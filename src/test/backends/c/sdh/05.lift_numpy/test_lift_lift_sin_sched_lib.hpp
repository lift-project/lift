

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_sin_sched(double * v_initial_param_13683_4818, double * & v_user_func_13687_4882, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13730 = 0;(gpe_loop_cvar_13730 < 4); (++gpe_loop_cvar_13730)){
        GPEQ_PUSH(gpe_loop_cvar_13730, reinterpret_cast<uintptr_t>(v_initial_param_13683_4818)); 
        GPEQ_PUSH(gpe_loop_cvar_13730, reinterpret_cast<uintptr_t>(v_user_func_13687_4882)); 
        GPEQ_PUSH(gpe_loop_cvar_13730, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16085 = 0;(v_tile_batch_16085 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16085)){
        int v_virtual_tile_id_16086 = (LCP_TILE_ID() + (v_tile_batch_16085 * 2));
        if ((v_virtual_tile_id_16086 < ((v_N_4617)/(8))))for (int v_gpe_batch_16087 = 0;(v_gpe_batch_16087 <= 1); (++v_gpe_batch_16087)){
            for (int v_gpe_16088 = 0;(v_gpe_16088 < 4); (++v_gpe_16088)){
                GPEQ_PUSH(v_gpe_16088, (v_gpe_16088 + (4 * v_gpe_batch_16087))); 
            }
            {
                
            }
            for (int v_gpe_16089 = 0;(v_gpe_16089 < 4); (++v_gpe_16089)){
                ; 
                LCPQ_POP(v_gpe_16089); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}