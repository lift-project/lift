
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTANDSUMUP_H
#define MULTANDSUMUP_H
; 
float multAndSumUp(float acc, float l, float r){
    { return acc + (l * r); }; 
}

#endif
 ; 
void convolve(float * v_initial_param_831_371, float * v_initial_param_832_372, float * & v_user_func_840_374, int v_M_1, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_840_374 = reinterpret_cast<float *>(malloc(((1 + v_M_1 + (-1 * v_N_0)) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_369 = 0;(v_i_369 <= (v_M_1 + (-1 * v_N_0))); (++v_i_369)){
        // For each element reduced sequentially
        v_user_func_840_374[v_i_369] = 0.0f; 
        for (int v_i_370 = 0;(v_i_370 <= (-1 + v_N_0)); (++v_i_370)){
            v_user_func_840_374[v_i_369] = multAndSumUp(v_user_func_840_374[v_i_369], v_initial_param_831_371[(v_i_369 + v_i_370)], v_initial_param_832_372[v_i_370]); 
        }
    }
}
}; 