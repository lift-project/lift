
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
void ediff1d(float * v_initial_param_322_170, float * & v_user_func_325_171, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_325_171 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_168 = 0;(v_i_168 <= (-2 + v_N_0)); (++v_i_168)){
        // For each element reduced sequentially
        v_user_func_325_171[v_i_168] = 0.0f; 
        for (int v_i_169 = 0;(v_i_169 <= 1); (++v_i_169)){
            v_user_func_325_171[v_i_168] = diff2(v_user_func_325_171[v_i_168], v_initial_param_322_170[(v_i_168 + v_i_169)]); 
        }
    }
}
}; 