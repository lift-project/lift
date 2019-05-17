
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
void trapz(float * v_initial_param_399_189, float * v_initial_param_400_190, float * & v_user_func_403_193, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_427_192 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_403_193 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_188 = 0;(v_i_188 <= (-2 + v_N_0)); (++v_i_188)){
        v_user_func_427_192[v_i_188] = trapz(v_initial_param_399_189[v_i_188], v_initial_param_399_189[(1 + v_i_188)], v_initial_param_400_190[v_i_188], v_initial_param_400_190[(1 + v_i_188)]); 
    }
    // For each element reduced sequentially
    v_user_func_403_193[0] = 0.0f; 
    for (int v_i_187 = 0;(v_i_187 <= (-2 + v_N_0)); (++v_i_187)){
        v_user_func_403_193[0] = add(v_user_func_403_193[0], v_user_func_427_192[v_i_187]); 
    }
}
}; 