
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
void trunc(float * v_initial_param_248_110, float * & v_user_func_250_111, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_250_111 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_109 = 0;(v_i_109 <= (-1 + v_N_0)); (++v_i_109)){
        v_user_func_250_111[v_i_109] = trunc_uf(v_initial_param_248_110[v_i_109]); 
    }
}
}; 