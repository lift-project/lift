

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_expm1_sched(double * v_initial_param_1348_1835, double * & v_user_func_1352_1899, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1395 = 0;(gpe_loop_cvar_1395 < 4); (++gpe_loop_cvar_1395)){
        GPEQ_PUSH(gpe_loop_cvar_1395, reinterpret_cast<uintptr_t>(v_initial_param_1348_1835)); 
        GPEQ_PUSH(gpe_loop_cvar_1395, reinterpret_cast<uintptr_t>(v_user_func_1352_1899)); 
        GPEQ_PUSH(gpe_loop_cvar_1395, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2772 = 0;(v_tile_batch_2772 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2772)){
        int v_virtual_tile_id_2773 = (LCP_TILE_ID() + (v_tile_batch_2772 * 2));
        if ((v_virtual_tile_id_2773 < ((v_N_0)/(8))))for (int v_gpe_batch_2774 = 0;(v_gpe_batch_2774 <= 1); (++v_gpe_batch_2774)){
            for (int v_gpe_2775 = 0;(v_gpe_2775 < 4); (++v_gpe_2775)){
                GPEQ_PUSH(v_gpe_2775, (v_gpe_2775 + (4 * v_gpe_batch_2774))); 
            }
            {
                
            }
            for (int v_gpe_2776 = 0;(v_gpe_2776 < 4); (++v_gpe_2776)){
                ; 
                LCPQ_POP(v_gpe_2776); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}