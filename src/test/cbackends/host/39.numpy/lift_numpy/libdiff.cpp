
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
void diff(float * v_initial_param_324_168, float * & v_user_func_327_169, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_327_169 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_166 = 0;(v_i_166 <= (-2 + v_N_0)); (++v_i_166)){
        // For each element reduced sequentially
        v_user_func_327_169[v_i_166] = 0.0f; 
        for (int v_i_167 = 0;(v_i_167 <= 1); (++v_i_167)){
            v_user_func_327_169[v_i_166] = diff2(v_user_func_327_169[v_i_166], v_initial_param_324_168[(v_i_166 + v_i_167)]); 
        }
    }
}
}; 