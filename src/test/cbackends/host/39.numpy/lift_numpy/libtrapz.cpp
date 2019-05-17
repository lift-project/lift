
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
void trapz(float * v_initial_param_400_190, float * v_initial_param_401_191, float * & v_user_func_404_194, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_428_193 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_404_194 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_189 = 0;(v_i_189 <= (-2 + v_N_0)); (++v_i_189)){
        v_user_func_428_193[v_i_189] = trapz(v_initial_param_400_190[v_i_189], v_initial_param_400_190[(1 + v_i_189)], v_initial_param_401_191[v_i_189], v_initial_param_401_191[(1 + v_i_189)]); 
    }
    // For each element reduced sequentially
    v_user_func_404_194[0] = 0.0f; 
    for (int v_i_188 = 0;(v_i_188 <= (-2 + v_N_0)); (++v_i_188)){
        v_user_func_404_194[0] = add(v_user_func_404_194[0], v_user_func_428_193[v_i_188]); 
    }
}
}; 