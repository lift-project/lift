
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
void convolve(float * v_initial_param_812_357, float * v_initial_param_813_358, float * & v_user_func_821_360, int v_M_1, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_821_360 = reinterpret_cast<float *>(malloc(((1 + v_M_1 + (-1 * v_N_0)) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_355 = 0;(v_i_355 <= (v_M_1 + (-1 * v_N_0))); (++v_i_355)){
        // For each element reduced sequentially
        v_user_func_821_360[v_i_355] = 0.0f; 
        for (int v_i_356 = 0;(v_i_356 <= (-1 + v_N_0)); (++v_i_356)){
            v_user_func_821_360[v_i_355] = multAndSumUp(v_user_func_821_360[v_i_355], v_initial_param_812_357[(v_i_355 + v_i_356)], v_initial_param_813_358[v_i_356]); 
        }
    }
}
}; 