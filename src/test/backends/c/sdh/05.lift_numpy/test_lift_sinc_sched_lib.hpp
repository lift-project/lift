

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sinc_sched(double * v_initial_param_15255_6858, double * & v_user_func_15259_6922, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15302 = 0;(gpe_loop_cvar_15302 < 4); (++gpe_loop_cvar_15302)){
        GPEQ_PUSH(gpe_loop_cvar_15302, reinterpret_cast<uintptr_t>(v_initial_param_15255_6858)); 
        GPEQ_PUSH(gpe_loop_cvar_15302, reinterpret_cast<uintptr_t>(v_user_func_15259_6922)); 
        GPEQ_PUSH(gpe_loop_cvar_15302, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16415 = 0;(v_tile_batch_16415 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16415)){
        int v_virtual_tile_id_16416 = (LCP_TILE_ID() + (v_tile_batch_16415 * 2));
        if ((v_virtual_tile_id_16416 < ((v_N_4617)/(8))))for (int v_gpe_batch_16417 = 0;(v_gpe_batch_16417 <= 1); (++v_gpe_batch_16417)){
            for (int v_gpe_16418 = 0;(v_gpe_16418 < 4); (++v_gpe_16418)){
                GPEQ_PUSH(v_gpe_16418, (v_gpe_16418 + (4 * v_gpe_batch_16417))); 
            }
            {
                
            }
            for (int v_gpe_16419 = 0;(v_gpe_16419 < 4); (++v_gpe_16419)){
                ; 
                LCPQ_POP(v_gpe_16419); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}