
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif; 
void trunc(float * v_initial_param_248_112, float * & v_user_func_250_113, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_250_113 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_111 = 0;(v_i_111 <= (-1 + v_N_0)); (++v_i_111)){
        v_user_func_250_113[v_i_111] = trunc_uf(v_initial_param_248_112[v_i_111]); 
    }
}
}; 