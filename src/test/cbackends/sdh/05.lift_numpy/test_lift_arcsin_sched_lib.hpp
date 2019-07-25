

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_arcsin_sched(double * v_initial_param_216_407, double * & v_user_func_220_471, int v_N_0){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_263 = 0;(gpe_loop_cvar_263 < 4); (++gpe_loop_cvar_263)){
        GPEQ_PUSH(gpe_loop_cvar_263, reinterpret_cast<uintptr_t>(v_initial_param_216_407)); 
        GPEQ_PUSH(gpe_loop_cvar_263, reinterpret_cast<uintptr_t>(v_user_func_220_471)); 
        GPEQ_PUSH(gpe_loop_cvar_263, v_N_0); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_2541 = 0;(v_tile_batch_2541 <= (((v_N_0)/(8)) / 2)); (++v_tile_batch_2541)){
        int v_virtual_tile_id_2542 = (LCP_TILE_ID() + (v_tile_batch_2541 * 2));
        if ((v_virtual_tile_id_2542 < ((v_N_0)/(8))))for (int v_gpe_batch_2543 = 0;(v_gpe_batch_2543 <= 1); (++v_gpe_batch_2543)){
            for (int v_gpe_2544 = 0;(v_gpe_2544 < 4); (++v_gpe_2544)){
                GPEQ_PUSH(v_gpe_2544, (v_gpe_2544 + (4 * v_gpe_batch_2543))); 
            }
            {
                
            }
            for (int v_gpe_2545 = 0;(v_gpe_2545 < 4); (++v_gpe_2545)){
                ; 
                LCPQ_POP(v_gpe_2545); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}