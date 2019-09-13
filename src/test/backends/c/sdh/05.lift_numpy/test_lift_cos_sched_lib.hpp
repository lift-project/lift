

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_cos_sched(double * v_initial_param_13731_4886, double * & v_user_func_13735_4950, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13778 = 0;(gpe_loop_cvar_13778 < 4); (++gpe_loop_cvar_13778)){
        GPEQ_PUSH(gpe_loop_cvar_13778, reinterpret_cast<uintptr_t>(v_initial_param_13731_4886)); 
        GPEQ_PUSH(gpe_loop_cvar_13778, reinterpret_cast<uintptr_t>(v_user_func_13735_4950)); 
        GPEQ_PUSH(gpe_loop_cvar_13778, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16096 = 0;(v_tile_batch_16096 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16096)){
        int v_virtual_tile_id_16097 = (LCP_TILE_ID() + (v_tile_batch_16096 * 2));
        if ((v_virtual_tile_id_16097 < ((v_N_4617)/(8))))for (int v_gpe_batch_16098 = 0;(v_gpe_batch_16098 <= 1); (++v_gpe_batch_16098)){
            for (int v_gpe_16099 = 0;(v_gpe_16099 < 4); (++v_gpe_16099)){
                GPEQ_PUSH(v_gpe_16099, (v_gpe_16099 + (4 * v_gpe_batch_16098))); 
            }
            {
                
            }
            for (int v_gpe_16100 = 0;(v_gpe_16100 < 4); (++v_gpe_16100)){
                ; 
                LCPQ_POP(v_gpe_16100); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}