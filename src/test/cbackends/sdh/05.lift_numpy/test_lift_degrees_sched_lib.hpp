

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_degrees_sched(double * v_initial_param_14008_5226, double * & v_user_func_14012_5290, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_14055 = 0;(gpe_loop_cvar_14055 < 4); (++gpe_loop_cvar_14055)){
        GPEQ_PUSH(gpe_loop_cvar_14055, reinterpret_cast<uintptr_t>(v_initial_param_14008_5226)); 
        GPEQ_PUSH(gpe_loop_cvar_14055, reinterpret_cast<uintptr_t>(v_user_func_14012_5290)); 
        GPEQ_PUSH(gpe_loop_cvar_14055, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16151 = 0;(v_tile_batch_16151 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16151)){
        int v_virtual_tile_id_16152 = (LCP_TILE_ID() + (v_tile_batch_16151 * 2));
        if ((v_virtual_tile_id_16152 < ((v_N_4617)/(8))))for (int v_gpe_batch_16153 = 0;(v_gpe_batch_16153 <= 1); (++v_gpe_batch_16153)){
            for (int v_gpe_16154 = 0;(v_gpe_16154 < 4); (++v_gpe_16154)){
                GPEQ_PUSH(v_gpe_16154, (v_gpe_16154 + (4 * v_gpe_batch_16153))); 
            }
            {
                
            }
            for (int v_gpe_16155 = 0;(v_gpe_16155 < 4); (++v_gpe_16155)){
                ; 
                LCPQ_POP(v_gpe_16155); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}