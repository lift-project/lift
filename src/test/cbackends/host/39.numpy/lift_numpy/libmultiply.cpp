
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
void multiply(float * v_initial_param_670_303, float * v_initial_param_671_304, float * & v_user_func_677_306, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_677_306 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_302 = 0;(v_i_302 <= (-1 + v_N_0)); (++v_i_302)){
        v_user_func_677_306[v_i_302] = multiply_uf(v_initial_param_670_303[v_i_302], v_initial_param_671_304[v_i_302]); 
    }
}
}; 