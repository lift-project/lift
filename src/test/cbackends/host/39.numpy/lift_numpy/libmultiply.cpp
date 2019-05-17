
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
void multiply(float * v_initial_param_616_259, float * v_initial_param_617_260, float * & v_user_func_623_262, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_623_262 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_258 = 0;(v_i_258 <= (-1 + v_N_0)); (++v_i_258)){
        v_user_func_623_262[v_i_258] = multiply_uf(v_initial_param_616_259[v_i_258], v_initial_param_617_260[v_i_258]); 
    }
}
}; 