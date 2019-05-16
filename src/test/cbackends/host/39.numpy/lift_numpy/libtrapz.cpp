
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
void trapz(float * v_initial_param_382_170, float * v_initial_param_383_171, float * & v_user_func_386_174, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_410_173 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_386_174 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_169 = 0;(v_i_169 <= (-2 + v_N_0)); (++v_i_169)){
        v_user_func_410_173[v_i_169] = trapz(v_initial_param_382_170[v_i_169], v_initial_param_382_170[(1 + v_i_169)], v_initial_param_383_171[v_i_169], v_initial_param_383_171[(1 + v_i_169)]); 
    }
    // For each element reduced sequentially
    v_user_func_386_174[0] = 0.0f; 
    for (int v_i_168 = 0;(v_i_168 <= (-2 + v_N_0)); (++v_i_168)){
        v_user_func_386_174[0] = add(v_user_func_386_174[0], v_user_func_410_173[v_i_168]); 
    }
}
}; 