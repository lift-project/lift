

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_square_sched(double * v_initial_param_2266_2719, double * & v_user_func_2270_2783, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_2313 = 0;(gpe_loop_cvar_2313 < 4); (++gpe_loop_cvar_2313)){
        GPEQ_PUSH(gpe_loop_cvar_2313, reinterpret_cast<uintptr_t>(v_initial_param_2266_2719)); 
        GPEQ_PUSH(gpe_loop_cvar_2313, reinterpret_cast<uintptr_t>(v_user_func_2270_2783)); 
        GPEQ_PUSH(gpe_loop_cvar_2313, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2915 = 0;(v_tile_batch_2915 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2915)){
        int v_virtual_tile_id_2916 = (LCP_TILE_ID() + (v_tile_batch_2915 * 2));
        if ((v_virtual_tile_id_2916 < ((v_N_0)/(8))))for (int v_gpe_batch_2917 = 0;(v_gpe_batch_2917 <= 1); (++v_gpe_batch_2917)){
            for (int v_gpe_2918 = 0;(v_gpe_2918 < 4); (++v_gpe_2918)){
                GPEQ_PUSH(v_gpe_2918, (v_gpe_2918 + (4 * v_gpe_batch_2917))); 
            }
            {
                
            }
            for (int v_gpe_2919 = 0;(v_gpe_2919 < 4); (++v_gpe_2919)){
                ; 
                LCPQ_POP(v_gpe_2919); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}