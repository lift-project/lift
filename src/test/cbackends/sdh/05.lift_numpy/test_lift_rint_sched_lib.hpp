

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_rint_sched(double * v_initial_param_829_1427, double * & v_user_func_833_1491, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_876 = 0;(gpe_loop_cvar_876 < 4); (++gpe_loop_cvar_876)){
        GPEQ_PUSH(gpe_loop_cvar_876, reinterpret_cast<uintptr_t>(v_initial_param_829_1427)); 
        GPEQ_PUSH(gpe_loop_cvar_876, reinterpret_cast<uintptr_t>(v_user_func_833_1491)); 
        GPEQ_PUSH(gpe_loop_cvar_876, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2706 = 0;(v_tile_batch_2706 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2706)){
        int v_virtual_tile_id_2707 = (LCP_TILE_ID() + (v_tile_batch_2706 * 2));
        if ((v_virtual_tile_id_2707 < ((v_N_0)/(8))))for (int v_gpe_batch_2708 = 0;(v_gpe_batch_2708 <= 1); (++v_gpe_batch_2708)){
            for (int v_gpe_2709 = 0;(v_gpe_2709 < 4); (++v_gpe_2709)){
                GPEQ_PUSH(v_gpe_2709, (v_gpe_2709 + (4 * v_gpe_batch_2708))); 
            }
            {
                
            }
            for (int v_gpe_2710 = 0;(v_gpe_2710 < 4); (++v_gpe_2710)){
                ; 
                LCPQ_POP(v_gpe_2710); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}