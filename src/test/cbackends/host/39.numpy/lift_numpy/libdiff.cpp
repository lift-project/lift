
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
void diff(float * v_initial_param_335_179, float * & v_user_func_338_180, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_338_180 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_177 = 0;(v_i_177 <= (-2 + v_N_0)); (++v_i_177)){
        // For each element reduced sequentially
        v_user_func_338_180[v_i_177] = 0.0f; 
        for (int v_i_178 = 0;(v_i_178 <= 1); (++v_i_178)){
            v_user_func_338_180[v_i_177] = diff2(v_user_func_338_180[v_i_177], v_initial_param_335_179[(v_i_177 + v_i_178)]); 
        }
    }
}
}; 