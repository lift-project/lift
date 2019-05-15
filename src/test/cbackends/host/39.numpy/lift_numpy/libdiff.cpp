
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
void diff(float * v_initial_param_1593_334, float * & v_user_func_1596_335, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1596_335 = reinterpret_cast<float *>(malloc(((-1 + v_N_190) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_332 = 0;(v_i_332 <= (-2 + v_N_190)); (++v_i_332)){
        // For each element reduced sequentially
        v_user_func_1596_335[v_i_332] = 0.0f; 
        for (int v_i_333 = 0;(v_i_333 <= 1); (++v_i_333)){
            v_user_func_1596_335[v_i_332] = diff2(v_user_func_1596_335[v_i_332], v_initial_param_1593_334[(v_i_332 + v_i_333)]); 
        }
    }
}
}; 