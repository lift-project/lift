
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H
; 
float arcsinh_uf(float x){
    { return asinh(x); }; 
}

#endif; 
void arcsinh(float * v_initial_param_192_82, float * & v_user_func_194_83, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_194_83 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_81 = 0;(v_i_81 <= (-1 + v_N_0)); (++v_i_81)){
        v_user_func_194_83[v_i_81] = arcsinh_uf(v_initial_param_192_82[v_i_81]); 
    }
}
}; 