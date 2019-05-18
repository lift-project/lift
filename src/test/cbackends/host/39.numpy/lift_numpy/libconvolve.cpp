
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
void convolve(float * v_initial_param_801_346, float * v_initial_param_802_347, float * & v_user_func_810_349, int v_M_1, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_810_349 = reinterpret_cast<float *>(malloc(((1 + v_M_1 + (-1 * v_N_0)) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_344 = 0;(v_i_344 <= (v_M_1 + (-1 * v_N_0))); (++v_i_344)){
        // For each element reduced sequentially
        v_user_func_810_349[v_i_344] = 0.0f; 
        for (int v_i_345 = 0;(v_i_345 <= (-1 + v_N_0)); (++v_i_345)){
            v_user_func_810_349[v_i_344] = multAndSumUp(v_user_func_810_349[v_i_344], v_initial_param_801_346[(v_i_344 + v_i_345)], v_initial_param_802_347[v_i_345]); 
        }
    }
}
}; 