
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
void ediff1d(float * v_initial_param_303_146, float * & v_user_func_306_147, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_306_147 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_144 = 0;(v_i_144 <= (-2 + v_N_0)); (++v_i_144)){
        // For each element reduced sequentially
        v_user_func_306_147[v_i_144] = 0.0f; 
        for (int v_i_145 = 0;(v_i_145 <= 1); (++v_i_145)){
            v_user_func_306_147[v_i_144] = diff2(v_user_func_306_147[v_i_144], v_initial_param_303_146[(v_i_144 + v_i_145)]); 
        }
    }
}
}; 