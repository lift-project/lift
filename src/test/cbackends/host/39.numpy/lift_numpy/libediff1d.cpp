
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
void ediff1d(float * v_initial_param_319_166, float * & v_user_func_322_167, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_322_167 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_164 = 0;(v_i_164 <= (-2 + v_N_0)); (++v_i_164)){
        // For each element reduced sequentially
        v_user_func_322_167[v_i_164] = 0.0f; 
        for (int v_i_165 = 0;(v_i_165 <= 1); (++v_i_165)){
            v_user_func_322_167[v_i_164] = diff2(v_user_func_322_167[v_i_164], v_initial_param_319_166[(v_i_164 + v_i_165)]); 
        }
    }
}
}; 