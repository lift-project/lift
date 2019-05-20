

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(float * v_initial_param_1_12, float * v_initial_param_2_13, float * & v_user_func_46_78, int v_M_2, int v_K_3, int v_N_1){
    // Allocate memory for output pointers
    v_user_func_46_78 = reinterpret_cast<float *>(trans_alloc(((v_N_1 * v_M_2) * sizeof(float)))); 
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_78 = 0;(gpe_loop_cvar_78 < 4); (++gpe_loop_cvar_78)){
        GPEQ_PUSH(gpe_loop_cvar_78, reinterpret_cast<uint32_t>(v_initial_param_1_12)); 
        GPEQ_PUSH(gpe_loop_cvar_78, reinterpret_cast<uint32_t>(v_initial_param_2_13)); 
        GPEQ_PUSH(gpe_loop_cvar_78, reinterpret_cast<uint32_t>(v_user_func_46_78)); 
        GPEQ_PUSH(gpe_loop_cvar_78, v_M_2); 
        GPEQ_PUSH(gpe_loop_cvar_78, v_K_3); 
        GPEQ_PUSH(gpe_loop_cvar_78, v_N_1); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_79 = 0;(v_tile_batch_79 <= (v_M_2 / 2)); (++v_tile_batch_79)){
        int v_virtual_tile_id_80 = (LCP_TILE_ID() + (v_tile_batch_79 * 2));
        if ((v_virtual_tile_id_80 < v_M_2))for (int v_gpe_batch_81 = 0;(v_gpe_batch_81 <= (v_N_1 / 4)); (++v_gpe_batch_81)){
            for (int v_gpe_82 = 0;(v_gpe_82 < 4); (++v_gpe_82)){
                GPEQ_PUSH(v_gpe_82, (v_gpe_82 + (4 * v_gpe_batch_81))); 
            }
            {
                
            }
            for (int v_gpe_83 = 0;(v_gpe_83 < 4); (++v_gpe_83)){
                ; 
                LCPQ_POP(v_gpe_83); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}