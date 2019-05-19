
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
void ediff1d(float * v_initial_param_346_196, float * & v_user_func_349_197, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_349_197 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_194 = 0;(v_i_194 <= (-2 + v_N_0)); (++v_i_194)){
        // For each element reduced sequentially
        v_user_func_349_197[v_i_194] = 0.0f; 
        for (int v_i_195 = 0;(v_i_195 <= 1); (++v_i_195)){
            v_user_func_349_197[v_i_194] = diff2(v_user_func_349_197[v_i_194], v_initial_param_346_196[(v_i_194 + v_i_195)]); 
        }
    }
}
}; 