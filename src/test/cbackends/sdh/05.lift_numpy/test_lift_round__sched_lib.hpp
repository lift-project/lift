

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_round__sched(double * v_initial_param_14392_5974, double * & v_user_func_14396_6038, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14439 = 0;(gpe_loop_cvar_14439 < 4); (++gpe_loop_cvar_14439)){
        GPEQ_PUSH(gpe_loop_cvar_14439, reinterpret_cast<uintptr_t>(v_initial_param_14392_5974)); 
        GPEQ_PUSH(gpe_loop_cvar_14439, reinterpret_cast<uintptr_t>(v_user_func_14396_6038)); 
        GPEQ_PUSH(gpe_loop_cvar_14439, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16272 = 0;(v_tile_batch_16272 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16272)){
        int v_virtual_tile_id_16273 = (LCP_TILE_ID() + (v_tile_batch_16272 * 2));
        if ((v_virtual_tile_id_16273 < ((v_N_4617)/(8))))for (int v_gpe_batch_16274 = 0;(v_gpe_batch_16274 <= 1); (++v_gpe_batch_16274)){
            for (int v_gpe_16275 = 0;(v_gpe_16275 < 4); (++v_gpe_16275)){
                GPEQ_PUSH(v_gpe_16275, (v_gpe_16275 + (4 * v_gpe_batch_16274))); 
            }
            {
                
            }
            for (int v_gpe_16276 = 0;(v_gpe_16276 < 4); (++v_gpe_16276)){
                ; 
                LCPQ_POP(v_gpe_16276); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}