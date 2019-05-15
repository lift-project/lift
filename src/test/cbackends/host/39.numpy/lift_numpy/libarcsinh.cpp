
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H
; 
float arcsinh_uf(float x){
    { return asinh(x); }; 
}

#endif
 ; 
void arcsinh(float * v_initial_param_193_91, float * & v_user_func_195_92, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_195_92 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_90 = 0;(v_i_90 <= (-1 + v_N_0)); (++v_i_90)){
        v_user_func_195_92[v_i_90] = arcsinh_uf(v_initial_param_193_91[v_i_90]); 
    }
}
}; 