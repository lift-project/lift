

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_reciprocal_sched(double * v_initial_param_15400_6994, double * & v_user_func_15404_7058, int v_N_4617){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_15447 = 0;(gpe_loop_cvar_15447 < 4); (++gpe_loop_cvar_15447)){
        GPEQ_PUSH(gpe_loop_cvar_15447, reinterpret_cast<uintptr_t>(v_initial_param_15400_6994)); 
        GPEQ_PUSH(gpe_loop_cvar_15447, reinterpret_cast<uintptr_t>(v_user_func_15404_7058)); 
        GPEQ_PUSH(gpe_loop_cvar_15447, v_N_4617); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16437 = 0;(v_tile_batch_16437 <= (((v_N_4617)/(8)) / 2)); (++v_tile_batch_16437)){
        int v_virtual_tile_id_16438 = (LCP_TILE_ID() + (v_tile_batch_16437 * 2));
        if ((v_virtual_tile_id_16438 < ((v_N_4617)/(8))))for (int v_gpe_batch_16439 = 0;(v_gpe_batch_16439 <= 1); (++v_gpe_batch_16439)){
            for (int v_gpe_16440 = 0;(v_gpe_16440 < 4); (++v_gpe_16440)){
                GPEQ_PUSH(v_gpe_16440, (v_gpe_16440 + (4 * v_gpe_batch_16439))); 
            }
            {
                
            }
            for (int v_gpe_16441 = 0;(v_gpe_16441 < 4); (++v_gpe_16441)){
                ; 
                LCPQ_POP(v_gpe_16441); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}