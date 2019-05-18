
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
void ediff1d(float * v_initial_param_332_180, float * & v_user_func_335_181, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_335_181 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_178 = 0;(v_i_178 <= (-2 + v_N_0)); (++v_i_178)){
        // For each element reduced sequentially
        v_user_func_335_181[v_i_178] = 0.0f; 
        for (int v_i_179 = 0;(v_i_179 <= 1); (++v_i_179)){
            v_user_func_335_181[v_i_178] = diff2(v_user_func_335_181[v_i_178], v_initial_param_332_180[(v_i_178 + v_i_179)]); 
        }
    }
}
}; 