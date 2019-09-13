

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_execute_sched(float * v_initial_param_13356_3824, float * v_initial_param_13357_3825, float * & v_user_func_13373_4337, int v_M_3813, int v_K_3814, int v_N_3812){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_13422 = 0;(gpe_loop_cvar_13422 < 4); (++gpe_loop_cvar_13422)){
        GPEQ_PUSH(gpe_loop_cvar_13422, reinterpret_cast<uintptr_t>(v_initial_param_13356_3824)); 
        GPEQ_PUSH(gpe_loop_cvar_13422, reinterpret_cast<uintptr_t>(v_initial_param_13357_3825)); 
        GPEQ_PUSH(gpe_loop_cvar_13422, reinterpret_cast<uintptr_t>(v_user_func_13373_4337)); 
        GPEQ_PUSH(gpe_loop_cvar_13422, v_M_3813); 
        GPEQ_PUSH(gpe_loop_cvar_13422, v_K_3814); 
        GPEQ_PUSH(gpe_loop_cvar_13422, v_N_3812); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    // For each transmuter chip
    for (int v_i_3820 = 0;(v_i_3820 < ((v_M_3813)/(2))); (++v_i_3820)){
        for (int v_tile_batch_13423 = 0;(v_tile_batch_13423 <= 1); (++v_tile_batch_13423)){
            int v_virtual_tile_id_13424 = (LCP_TILE_ID() + (v_tile_batch_13423 * 2));
            if ((v_virtual_tile_id_13424 < 2))for (int v_gpe_batch_13425 = 0;(v_gpe_batch_13425 <= (v_N_3812 / 4)); (++v_gpe_batch_13425)){
                for (int v_gpe_13426 = 0;(v_gpe_13426 < 4); (++v_gpe_13426)){
                    GPEQ_PUSH(v_gpe_13426, (v_gpe_13426 + (4 * v_gpe_batch_13425))); 
                }
                {
                    
                }
                for (int v_gpe_13427 = 0;(v_gpe_13427 < 4); (++v_gpe_13427)){
                    ; 
                    LCPQ_POP(v_gpe_13427); 
                }
            }
        }
        {
            
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}