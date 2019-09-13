

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_around_sched(double * v_initial_param_14392_5906, double * & v_user_func_14396_5970, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14439 = 0;(gpe_loop_cvar_14439 < 4); (++gpe_loop_cvar_14439)){
        GPEQ_PUSH(gpe_loop_cvar_14439, reinterpret_cast<uintptr_t>(v_initial_param_14392_5906)); 
        GPEQ_PUSH(gpe_loop_cvar_14439, reinterpret_cast<uintptr_t>(v_user_func_14396_5970)); 
        GPEQ_PUSH(gpe_loop_cvar_14439, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16261 = 0;(v_tile_batch_16261 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16261)){
        int v_virtual_tile_id_16262 = (LCP_TILE_ID() + (v_tile_batch_16261 * 2));
        if ((v_virtual_tile_id_16262 < ((v_N_4617)/(8))))for (int v_gpe_batch_16263 = 0;(v_gpe_batch_16263 <= 1); (++v_gpe_batch_16263)){
            for (int v_gpe_16264 = 0;(v_gpe_16264 < 4); (++v_gpe_16264)){
                GPEQ_PUSH(v_gpe_16264, (v_gpe_16264 + (4 * v_gpe_batch_16263))); 
            }
            {
                
            }
            for (int v_gpe_16265 = 0;(v_gpe_16265 < 4); (++v_gpe_16265)){
                ; 
                LCPQ_POP(v_gpe_16265); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}