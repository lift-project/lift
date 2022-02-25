

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_radians_sched(double * v_initial_param_445_679, double * & v_user_func_449_743, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_492 = 0;(gpe_loop_cvar_492 < 4); (++gpe_loop_cvar_492)){
        GPEQ_PUSH(gpe_loop_cvar_492, reinterpret_cast<uintptr_t>(v_initial_param_445_679)); 
        GPEQ_PUSH(gpe_loop_cvar_492, reinterpret_cast<uintptr_t>(v_user_func_449_743)); 
        GPEQ_PUSH(gpe_loop_cvar_492, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2585 = 0;(v_tile_batch_2585 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2585)){
        int v_virtual_tile_id_2586 = (LCP_TILE_ID() + (v_tile_batch_2585 * 2));
        if ((v_virtual_tile_id_2586 < ((v_N_0)/(8))))for (int v_gpe_batch_2587 = 0;(v_gpe_batch_2587 <= 1); (++v_gpe_batch_2587)){
            for (int v_gpe_2588 = 0;(v_gpe_2588 < 4); (++v_gpe_2588)){
                GPEQ_PUSH(v_gpe_2588, (v_gpe_2588 + (4 * v_gpe_batch_2587))); 
            }
            {
                
            }
            for (int v_gpe_2589 = 0;(v_gpe_2589 < 4); (++v_gpe_2589)){
                ; 
                LCPQ_POP(v_gpe_2589); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}