

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_fabs_sched(double * v_initial_param_2314_2855, double * & v_user_func_2318_2919, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_2361 = 0;(gpe_loop_cvar_2361 < 4); (++gpe_loop_cvar_2361)){
        GPEQ_PUSH(gpe_loop_cvar_2361, reinterpret_cast<uintptr_t>(v_initial_param_2314_2855)); 
        GPEQ_PUSH(gpe_loop_cvar_2361, reinterpret_cast<uintptr_t>(v_user_func_2318_2919)); 
        GPEQ_PUSH(gpe_loop_cvar_2361, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2937 = 0;(v_tile_batch_2937 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2937)){
        int v_virtual_tile_id_2938 = (LCP_TILE_ID() + (v_tile_batch_2937 * 2));
        if ((v_virtual_tile_id_2938 < ((v_N_0)/(8))))for (int v_gpe_batch_2939 = 0;(v_gpe_batch_2939 <= 1); (++v_gpe_batch_2939)){
            for (int v_gpe_2940 = 0;(v_gpe_2940 < 4); (++v_gpe_2940)){
                GPEQ_PUSH(v_gpe_2940, (v_gpe_2940 + (4 * v_gpe_batch_2939))); 
            }
            {
                
            }
            for (int v_gpe_2941 = 0;(v_gpe_2941 < 4); (++v_gpe_2941)){
                ; 
                LCPQ_POP(v_gpe_2941); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}