
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void ediff1d(float * v_initial_param_318_164, float * & v_user_func_321_165, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_321_165 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_162 = 0;(v_i_162 <= (-2 + v_N_0)); (++v_i_162)){
        // For each element reduced sequentially
        v_user_func_321_165[v_i_162] = 0.0f; 
        for (int v_i_163 = 0;(v_i_163 <= 1); (++v_i_163)){
            v_user_func_321_165[v_i_162] = diff2(v_user_func_321_165[v_i_162], v_initial_param_318_164[(v_i_162 + v_i_163)]); 
        }
    }
}
}; 