
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTIPLY_UF_H
#define MULTIPLY_UF_H
; 
float multiply_uf(float x, float y){
    return x * y;; 
}

#endif
 ; 
void multiply(float * v_initial_param_614_257, float * v_initial_param_615_258, float * & v_user_func_621_260, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_621_260 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_256 = 0;(v_i_256 <= (-1 + v_N_0)); (++v_i_256)){
        v_user_func_621_260[v_i_256] = multiply_uf(v_initial_param_614_257[v_i_256], v_initial_param_615_258[v_i_256]); 
    }
}
}; 