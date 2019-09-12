

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_tan_sched(double * v_initial_param_13779_4954, double * & v_user_func_13783_5018, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13826 = 0;(gpe_loop_cvar_13826 < 4); (++gpe_loop_cvar_13826)){
        GPEQ_PUSH(gpe_loop_cvar_13826, reinterpret_cast<uintptr_t>(v_initial_param_13779_4954)); 
        GPEQ_PUSH(gpe_loop_cvar_13826, reinterpret_cast<uintptr_t>(v_user_func_13783_5018)); 
        GPEQ_PUSH(gpe_loop_cvar_13826, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16107 = 0;(v_tile_batch_16107 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16107)){
        int v_virtual_tile_id_16108 = (LCP_TILE_ID() + (v_tile_batch_16107 * 2));
        if ((v_virtual_tile_id_16108 < ((v_N_4617)/(8))))for (int v_gpe_batch_16109 = 0;(v_gpe_batch_16109 <= 1); (++v_gpe_batch_16109)){
            for (int v_gpe_16110 = 0;(v_gpe_16110 < 4); (++v_gpe_16110)){
                GPEQ_PUSH(v_gpe_16110, (v_gpe_16110 + (4 * v_gpe_batch_16109))); 
            }
            {
                
            }
            for (int v_gpe_16111 = 0;(v_gpe_16111 < 4); (++v_gpe_16111)){
                ; 
                LCPQ_POP(v_gpe_16111); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}