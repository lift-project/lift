

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_lift_log2_sched(double * v_initial_param_1540_2107, double * & v_user_func_1544_2171, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1587 = 0;(gpe_loop_cvar_1587 < 4); (++gpe_loop_cvar_1587)){
        GPEQ_PUSH(gpe_loop_cvar_1587, reinterpret_cast<uintptr_t>(v_initial_param_1540_2107)); 
        GPEQ_PUSH(gpe_loop_cvar_1587, reinterpret_cast<uintptr_t>(v_user_func_1544_2171)); 
        GPEQ_PUSH(gpe_loop_cvar_1587, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2816 = 0;(v_tile_batch_2816 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2816)){
        int v_virtual_tile_id_2817 = (LCP_TILE_ID() + (v_tile_batch_2816 * 2));
        if ((v_virtual_tile_id_2817 < ((v_N_0)/(8))))for (int v_gpe_batch_2818 = 0;(v_gpe_batch_2818 <= 1); (++v_gpe_batch_2818)){
            for (int v_gpe_2819 = 0;(v_gpe_2819 < 4); (++v_gpe_2819)){
                GPEQ_PUSH(v_gpe_2819, (v_gpe_2819 + (4 * v_gpe_batch_2818))); 
            }
            {
                
            }
            for (int v_gpe_2820 = 0;(v_gpe_2820 < 4); (++v_gpe_2820)){
                ; 
                LCPQ_POP(v_gpe_2820); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}