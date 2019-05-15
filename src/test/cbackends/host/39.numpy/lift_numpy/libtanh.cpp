
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef TANH_UF_H
#define TANH_UF_H
; 
float tanh_uf(float x){
    { return tanh(x); }; 
}

#endif; 
void tanh(float * v_initial_param_185_79, float * & v_user_func_187_80, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_187_80 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_78 = 0;(v_i_78 <= (-1 + v_N_0)); (++v_i_78)){
        v_user_func_187_80[v_i_78] = tanh_uf(v_initial_param_185_79[v_i_78]); 
    }
}
}; 