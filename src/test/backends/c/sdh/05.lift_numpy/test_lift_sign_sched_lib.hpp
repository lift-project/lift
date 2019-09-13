

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_sign_sched(double * v_initial_param_15939_7538, double * & v_user_func_15943_7602, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15986 = 0;(gpe_loop_cvar_15986 < 4); (++gpe_loop_cvar_15986)){
        GPEQ_PUSH(gpe_loop_cvar_15986, reinterpret_cast<uintptr_t>(v_initial_param_15939_7538)); 
        GPEQ_PUSH(gpe_loop_cvar_15986, reinterpret_cast<uintptr_t>(v_user_func_15943_7602)); 
        GPEQ_PUSH(gpe_loop_cvar_15986, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16525 = 0;(v_tile_batch_16525 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16525)){
        int v_virtual_tile_id_16526 = (LCP_TILE_ID() + (v_tile_batch_16525 * 2));
        if ((v_virtual_tile_id_16526 < ((v_N_4617)/(8))))for (int v_gpe_batch_16527 = 0;(v_gpe_batch_16527 <= 1); (++v_gpe_batch_16527)){
            for (int v_gpe_16528 = 0;(v_gpe_16528 < 4); (++v_gpe_16528)){
                GPEQ_PUSH(v_gpe_16528, (v_gpe_16528 + (4 * v_gpe_batch_16527))); 
            }
            {
                
            }
            for (int v_gpe_16529 = 0;(v_gpe_16529 < 4); (++v_gpe_16529)){
                ; 
                LCPQ_POP(v_gpe_16529); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}