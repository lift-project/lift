
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
void diff(float * v_initial_param_327_171, float * & v_user_func_330_172, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_330_172 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_169 = 0;(v_i_169 <= (-2 + v_N_0)); (++v_i_169)){
        // For each element reduced sequentially
        v_user_func_330_172[v_i_169] = 0.0f; 
        for (int v_i_170 = 0;(v_i_170 <= 1); (++v_i_170)){
            v_user_func_330_172[v_i_169] = diff2(v_user_func_330_172[v_i_169], v_initial_param_327_171[(v_i_169 + v_i_170)]); 
        }
    }
}
}; 