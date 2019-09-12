

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_deg2rad_sched(double * v_initial_param_14056_5362, double * & v_user_func_14060_5426, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14103 = 0;(gpe_loop_cvar_14103 < 4); (++gpe_loop_cvar_14103)){
        GPEQ_PUSH(gpe_loop_cvar_14103, reinterpret_cast<uintptr_t>(v_initial_param_14056_5362)); 
        GPEQ_PUSH(gpe_loop_cvar_14103, reinterpret_cast<uintptr_t>(v_user_func_14060_5426)); 
        GPEQ_PUSH(gpe_loop_cvar_14103, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16173 = 0;(v_tile_batch_16173 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16173)){
        int v_virtual_tile_id_16174 = (LCP_TILE_ID() + (v_tile_batch_16173 * 2));
        if ((v_virtual_tile_id_16174 < ((v_N_4617)/(8))))for (int v_gpe_batch_16175 = 0;(v_gpe_batch_16175 <= 1); (++v_gpe_batch_16175)){
            for (int v_gpe_16176 = 0;(v_gpe_16176 < 4); (++v_gpe_16176)){
                GPEQ_PUSH(v_gpe_16176, (v_gpe_16176 + (4 * v_gpe_batch_16175))); 
            }
            {
                
            }
            for (int v_gpe_16177 = 0;(v_gpe_16177 < 4); (++v_gpe_16177)){
                ; 
                LCPQ_POP(v_gpe_16177); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}