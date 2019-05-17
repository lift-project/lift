
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H
; 
float arcsinh_uf(float x){
    { return asinh(x); }; 
}

#endif
 ; 
void arcsinh(float * v_initial_param_216_116, float * & v_user_func_218_117, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_218_117 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_115 = 0;(v_i_115 <= (-1 + v_N_0)); (++v_i_115)){
        v_user_func_218_117[v_i_115] = arcsinh_uf(v_initial_param_216_116[v_i_115]); 
    }
}
}; 