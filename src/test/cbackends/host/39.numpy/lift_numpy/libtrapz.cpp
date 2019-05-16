
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRAPZ_H
#define TRAPZ_H
; 
float trapz(float x1, float x2, float y1, float y2){
    { return (x2-x1)*(y2+y1)/2.0f; }; 
}

#endif
 ; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void trapz(float * v_initial_param_379_167, float * v_initial_param_380_168, float * & v_user_func_383_171, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_407_170 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_383_171 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_166 = 0;(v_i_166 <= (-2 + v_N_0)); (++v_i_166)){
        v_user_func_407_170[v_i_166] = trapz(v_initial_param_379_167[v_i_166], v_initial_param_379_167[(1 + v_i_166)], v_initial_param_380_168[v_i_166], v_initial_param_380_168[(1 + v_i_166)]); 
    }
    // For each element reduced sequentially
    v_user_func_383_171[0] = 0.0f; 
    for (int v_i_165 = 0;(v_i_165 <= (-2 + v_N_0)); (++v_i_165)){
        v_user_func_383_171[0] = add(v_user_func_383_171[0], v_user_func_407_170[v_i_165]); 
    }
}
}; 