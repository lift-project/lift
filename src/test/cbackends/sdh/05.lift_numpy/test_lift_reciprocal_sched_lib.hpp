

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_reciprocal_sched(double * v_initial_param_1823_2379, double * & v_user_func_1827_2443, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_1870 = 0;(gpe_loop_cvar_1870 < 4); (++gpe_loop_cvar_1870)){
        GPEQ_PUSH(gpe_loop_cvar_1870, reinterpret_cast<uintptr_t>(v_initial_param_1823_2379)); 
        GPEQ_PUSH(gpe_loop_cvar_1870, reinterpret_cast<uintptr_t>(v_user_func_1827_2443)); 
        GPEQ_PUSH(gpe_loop_cvar_1870, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2860 = 0;(v_tile_batch_2860 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2860)){
        int v_virtual_tile_id_2861 = (LCP_TILE_ID() + (v_tile_batch_2860 * 2));
        if ((v_virtual_tile_id_2861 < ((v_N_0)/(8))))for (int v_gpe_batch_2862 = 0;(v_gpe_batch_2862 <= 1); (++v_gpe_batch_2862)){
            for (int v_gpe_2863 = 0;(v_gpe_2863 < 4); (++v_gpe_2863)){
                GPEQ_PUSH(v_gpe_2863, (v_gpe_2863 + (4 * v_gpe_batch_2862))); 
            }
            {
                
            }
            for (int v_gpe_2864 = 0;(v_gpe_2864 < 4); (++v_gpe_2864)){
                ; 
                LCPQ_POP(v_gpe_2864); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}