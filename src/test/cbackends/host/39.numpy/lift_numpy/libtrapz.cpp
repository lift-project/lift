
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
void trapz(float * v_initial_param_380_168, float * v_initial_param_381_169, float * & v_user_func_384_172, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_408_171 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_384_172 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_167 = 0;(v_i_167 <= (-2 + v_N_0)); (++v_i_167)){
        v_user_func_408_171[v_i_167] = trapz(v_initial_param_380_168[v_i_167], v_initial_param_380_168[(1 + v_i_167)], v_initial_param_381_169[v_i_167], v_initial_param_381_169[(1 + v_i_167)]); 
    }
    // For each element reduced sequentially
    v_user_func_384_172[0] = 0.0f; 
    for (int v_i_166 = 0;(v_i_166 <= (-2 + v_N_0)); (++v_i_166)){
        v_user_func_384_172[0] = add(v_user_func_384_172[0], v_user_func_408_171[v_i_166]); 
    }
}
}; 