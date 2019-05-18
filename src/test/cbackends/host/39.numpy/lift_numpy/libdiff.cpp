
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
void diff(float * v_initial_param_334_178, float * & v_user_func_337_179, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_337_179 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_176 = 0;(v_i_176 <= (-2 + v_N_0)); (++v_i_176)){
        // For each element reduced sequentially
        v_user_func_337_179[v_i_176] = 0.0f; 
        for (int v_i_177 = 0;(v_i_177 <= 1); (++v_i_177)){
            v_user_func_337_179[v_i_176] = diff2(v_user_func_337_179[v_i_176], v_initial_param_334_178[(v_i_176 + v_i_177)]); 
        }
    }
}
}; 