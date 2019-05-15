
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif; 
void diff(float * v_initial_param_303_142, float * & v_user_func_306_143, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_306_143 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_140 = 0;(v_i_140 <= (-2 + v_N_0)); (++v_i_140)){
        // For each element reduced sequentially
        v_user_func_306_143[v_i_140] = 0.0f; 
        for (int v_i_141 = 0;(v_i_141 <= 1); (++v_i_141)){
            v_user_func_306_143[v_i_140] = diff2(v_user_func_306_143[v_i_140], v_initial_param_303_142[(v_i_140 + v_i_141)]); 
        }
    }
}
}; 