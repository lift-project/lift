
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef GRAD2_UF_H
#define GRAD2_UF_H
; 
float grad2_uf(float l, float r){
    { return (l - r)/2.0f; }; 
}

#endif
 ; 
void gradient(float * v_initial_param_339_170, float * & v_user_func_345_171, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_345_171 = reinterpret_cast<float *>(malloc(((-2 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_169 = 0;(v_i_169 <= (-3 + v_N_0)); (++v_i_169)){
        v_user_func_345_171[v_i_169] = grad2_uf(v_initial_param_339_170[(2 + v_i_169)], v_initial_param_339_170[v_i_169]); 
    }
}
}; 