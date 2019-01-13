
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <string.h>

#include "util.hpp"

    ; 
float add(float l, float r){
    { return (l + r); }; 
}
int main(){
    // Pop input, output pointers and sizes
    float * v_initial_param_2_11 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_initial_param_3_12 = reinterpret_cast<float *>(GPEQ_POP());
    float * v_user_func_22_14 = reinterpret_cast<float *>(GPEQ_POP());
    int v_N_0 = GPEQ_POP();
    // For each transmuter
    for (int v_i_7 = 0;(v_i_7 <= (-1 + ((v_N_0)/(16)))); (++v_i_7)){
        // For each tile
        int v_i_8 = GPE_TILE_ID();
        // For each GPE. TODO: check if you can get this by API call instead of push and pop
        int v_i_9 = GPEQ_POP();
        // For each element processed sequentially
        for (int v_i_10 = 0;(v_i_10 < 2); v_i_10 = (v_i_10 + 1)){
            v_user_func_22_14[(v_i_10 + (8 * v_i_8) + (2 * v_i_9) + (16 * v_i_7))] = add(v_initial_param_2_11[(v_i_10 + (8 * v_i_8) + (2 * v_i_9) + (16 * v_i_7))], v_initial_param_3_12[(v_i_10 + (8 * v_i_8) + (2 * v_i_9) + (16 * v_i_7))]); 
        }
        // Sync to LCP
        LCPQ_PUSH(1); 
    }
}