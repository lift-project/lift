
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
void trapz(float * v_initial_param_385_173, float * v_initial_param_386_174, float * & v_user_func_389_177, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_413_176 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_389_177 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_172 = 0;(v_i_172 <= (-2 + v_N_0)); (++v_i_172)){
        v_user_func_413_176[v_i_172] = trapz(v_initial_param_385_173[v_i_172], v_initial_param_385_173[(1 + v_i_172)], v_initial_param_386_174[v_i_172], v_initial_param_386_174[(1 + v_i_172)]); 
    }
    // For each element reduced sequentially
    v_user_func_389_177[0] = 0.0f; 
    for (int v_i_171 = 0;(v_i_171 <= (-2 + v_N_0)); (++v_i_171)){
        v_user_func_389_177[0] = add(v_user_func_389_177[0], v_user_func_413_176[v_i_171]); 
    }
}
}; 