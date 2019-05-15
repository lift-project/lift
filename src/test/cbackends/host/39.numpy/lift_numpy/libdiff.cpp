
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void diff(float * v_initial_param_304_144, float * & v_user_func_307_145, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_307_145 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_142 = 0;(v_i_142 <= (-2 + v_N_0)); (++v_i_142)){
        // For each element reduced sequentially
        v_user_func_307_145[v_i_142] = 0.0f; 
        for (int v_i_143 = 0;(v_i_143 <= 1); (++v_i_143)){
            v_user_func_307_145[v_i_142] = diff2(v_user_func_307_145[v_i_142], v_initial_param_304_144[(v_i_142 + v_i_143)]); 
        }
    }
}
}; 