

using namespace std;

#include <math.h>
#include <iostream>

#include "TMRevere.hpp"

    ; 
void lift_matrixmul_sched(float * v_initial_param_16607_7621, float * v_initial_param_16608_7622, float * & v_user_func_16621_8134, int v_M_7608, int v_K_7609, int v_N_7607){
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_16687 = 0;(gpe_loop_cvar_16687 < 4); (++gpe_loop_cvar_16687)){
        GPEQ_PUSH(gpe_loop_cvar_16687, reinterpret_cast<uintptr_t>(v_initial_param_16607_7621)); 
        GPEQ_PUSH(gpe_loop_cvar_16687, reinterpret_cast<uintptr_t>(v_initial_param_16608_7622)); 
        GPEQ_PUSH(gpe_loop_cvar_16687, reinterpret_cast<uintptr_t>(v_user_func_16621_8134)); 
        GPEQ_PUSH(gpe_loop_cvar_16687, v_M_7608); 
        GPEQ_PUSH(gpe_loop_cvar_16687, v_K_7609); 
        GPEQ_PUSH(gpe_loop_cvar_16687, v_N_7607); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_16688 = 0;(v_tile_batch_16688 <= (((v_M_7608)/((v_M_7608 / 2))) / 2)); (++v_tile_batch_16688)){
        int v_virtual_tile_id_16689 = (LCP_TILE_ID() + (v_tile_batch_16688 * 2));
        if ((v_virtual_tile_id_16689 < ((v_M_7608)/((v_M_7608 / 2))))){
            
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}