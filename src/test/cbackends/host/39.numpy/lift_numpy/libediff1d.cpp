
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
void ediff1d(float * v_initial_param_329_177, float * & v_user_func_332_178, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_332_178 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_175 = 0;(v_i_175 <= (-2 + v_N_0)); (++v_i_175)){
        // For each element reduced sequentially
        v_user_func_332_178[v_i_175] = 0.0f; 
        for (int v_i_176 = 0;(v_i_176 <= 1); (++v_i_176)){
            v_user_func_332_178[v_i_175] = diff2(v_user_func_332_178[v_i_175], v_initial_param_329_177[(v_i_175 + v_i_176)]); 
        }
    }
}
}; 