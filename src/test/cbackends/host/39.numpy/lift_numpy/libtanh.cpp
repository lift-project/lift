
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
void tanh(float * v_initial_param_185_82, float * & v_user_func_187_83, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_187_83 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_81 = 0;(v_i_81 <= (-1 + v_N_0)); (++v_i_81)){
        v_user_func_187_83[v_i_81] = tanh_uf(v_initial_param_185_82[v_i_81]); 
    }
}
}; 