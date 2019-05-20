
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
void convolve(float * v_initial_param_844_377, float * v_initial_param_845_378, float * & v_user_func_853_380, int v_M_1, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_853_380 = reinterpret_cast<float *>(malloc(((1 + v_M_1 + (-1 * v_N_0)) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_375 = 0;(v_i_375 <= (v_M_1 + (-1 * v_N_0))); (++v_i_375)){
        // For each element reduced sequentially
        v_user_func_853_380[v_i_375] = 0.0f; 
        for (int v_i_376 = 0;(v_i_376 <= (-1 + v_N_0)); (++v_i_376)){
            v_user_func_853_380[v_i_375] = multAndSumUp(v_user_func_853_380[v_i_375], v_initial_param_844_377[(v_i_375 + v_i_376)], v_initial_param_845_378[v_i_376]); 
        }
    }
}
}; 