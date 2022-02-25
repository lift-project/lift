

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(double * v_initial_param_72_15, double * & v_user_func_75_280, int v_N_4){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_201 = 0;(gpe_loop_cvar_201 < 4); (++gpe_loop_cvar_201)){
        GPEQ_PUSH(gpe_loop_cvar_201, reinterpret_cast<uintptr_t>(v_initial_param_72_15)); 
        GPEQ_PUSH(gpe_loop_cvar_201, reinterpret_cast<uintptr_t>(v_user_func_75_280)); 
        GPEQ_PUSH(gpe_loop_cvar_201, v_N_4); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_202 = 0;(v_tile_batch_202 <= (((v_N_4)/((4 * (v_N_4 / 8)))) / 2)); (++v_tile_batch_202)){
        int v_virtual_tile_id_203 = (LCP_TILE_ID() + (v_tile_batch_202 * 2));
        if ((v_virtual_tile_id_203 < ((v_N_4)/((4 * (v_N_4 / 8))))))for (int v_gpe_batch_204 = 0;(v_gpe_batch_204 <= 1); (++v_gpe_batch_204)){
            for (int v_gpe_205 = 0;(v_gpe_205 < 4); (++v_gpe_205)){
                GPEQ_PUSH(v_gpe_205, (v_gpe_205 + (4 * v_gpe_batch_204))); 
            }
            {
                
            }
            for (int v_gpe_206 = 0;(v_gpe_206 < 4); (++v_gpe_206)){
                ; 
                LCPQ_POP(v_gpe_206); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
    // LCPSingle
    if ((LCP_TILE_ID() == 0)){
        // For each element reduced sequentially
        v_user_func_75_280[0] = 0.0; 
        for (int v_i_14 = 0;(v_i_14 <= (-1 + ((v_N_4)/((v_N_4 / 8))))); (++v_i_14)){
            v_user_func_75_280[0] = add(v_user_func_75_280[0], v_initial_param_72_15[(v_i_14 * (v_N_4 / 8))]); 
        }
    }
}