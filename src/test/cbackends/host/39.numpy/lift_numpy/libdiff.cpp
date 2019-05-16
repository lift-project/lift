
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
void diff(float * v_initial_param_321_165, float * & v_user_func_324_166, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_324_166 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_163 = 0;(v_i_163 <= (-2 + v_N_0)); (++v_i_163)){
        // For each element reduced sequentially
        v_user_func_324_166[v_i_163] = 0.0f; 
        for (int v_i_164 = 0;(v_i_164 <= 1); (++v_i_164)){
            v_user_func_324_166[v_i_163] = diff2(v_user_func_324_166[v_i_163], v_initial_param_321_165[(v_i_163 + v_i_164)]); 
        }
    }
}
}; 