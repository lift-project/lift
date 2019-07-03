

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(float * v_initial_param_72_15, float * v_initial_param_73_16, float * & v_user_func_117_81, int v_M_5, int v_K_6, int v_N_4){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_155 = 0;(gpe_loop_cvar_155 < 4); (++gpe_loop_cvar_155)){
        GPEQ_PUSH(gpe_loop_cvar_155, reinterpret_cast<uintptr_t>(v_initial_param_72_15)); 
        GPEQ_PUSH(gpe_loop_cvar_155, reinterpret_cast<uintptr_t>(v_initial_param_73_16)); 
        GPEQ_PUSH(gpe_loop_cvar_155, reinterpret_cast<uintptr_t>(v_user_func_117_81)); 
        GPEQ_PUSH(gpe_loop_cvar_155, v_M_5); 
        GPEQ_PUSH(gpe_loop_cvar_155, v_K_6); 
        GPEQ_PUSH(gpe_loop_cvar_155, v_N_4); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_156 = 0;(v_tile_batch_156 <= (v_M_5 / 2)); (++v_tile_batch_156)){
        int v_virtual_tile_id_157 = (LCP_TILE_ID() + (v_tile_batch_156 * 2));
        if ((v_virtual_tile_id_157 < v_M_5))for (int v_gpe_batch_158 = 0;(v_gpe_batch_158 <= (v_N_4 / 4)); (++v_gpe_batch_158)){
            for (int v_gpe_159 = 0;(v_gpe_159 < 4); (++v_gpe_159)){
                GPEQ_PUSH(v_gpe_159, (v_gpe_159 + (4 * v_gpe_batch_158))); 
            }
            {
                
            }
            for (int v_gpe_160 = 0;(v_gpe_160 < 4); (++v_gpe_160)){
                ; 
                LCPQ_POP(v_gpe_160); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}