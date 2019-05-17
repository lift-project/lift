
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void ediff1d(float * v_initial_param_324_172, float * & v_user_func_327_173, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_327_173 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_170 = 0;(v_i_170 <= (-2 + v_N_0)); (++v_i_170)){
        // For each element reduced sequentially
        v_user_func_327_173[v_i_170] = 0.0f; 
        for (int v_i_171 = 0;(v_i_171 <= 1); (++v_i_171)){
            v_user_func_327_173[v_i_170] = diff2(v_user_func_327_173[v_i_170], v_initial_param_324_172[(v_i_170 + v_i_171)]); 
        }
    }
}
}; 