

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_log_sched(double * v_initial_param_15021_6586, double * & v_user_func_15025_6650, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15068 = 0;(gpe_loop_cvar_15068 < 4); (++gpe_loop_cvar_15068)){
        GPEQ_PUSH(gpe_loop_cvar_15068, reinterpret_cast<uintptr_t>(v_initial_param_15021_6586)); 
        GPEQ_PUSH(gpe_loop_cvar_15068, reinterpret_cast<uintptr_t>(v_user_func_15025_6650)); 
        GPEQ_PUSH(gpe_loop_cvar_15068, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16371 = 0;(v_tile_batch_16371 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16371)){
        int v_virtual_tile_id_16372 = (LCP_TILE_ID() + (v_tile_batch_16371 * 2));
        if ((v_virtual_tile_id_16372 < ((v_N_4617)/(8))))for (int v_gpe_batch_16373 = 0;(v_gpe_batch_16373 <= 1); (++v_gpe_batch_16373)){
            for (int v_gpe_16374 = 0;(v_gpe_16374 < 4); (++v_gpe_16374)){
                GPEQ_PUSH(v_gpe_16374, (v_gpe_16374 + (4 * v_gpe_batch_16373))); 
            }
            {
                
            }
            for (int v_gpe_16375 = 0;(v_gpe_16375 < 4); (++v_gpe_16375)){
                ; 
                LCPQ_POP(v_gpe_16375); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}