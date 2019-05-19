
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIVIDE_UF_H
#define DIVIDE_UF_H
; 
float divide_uf(float x, float y){
    return x / y;; 
}

#endif
 ; 
void true_divide(float * v_initial_param_652_303, float * v_initial_param_653_304, float * & v_user_func_659_306, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_659_306 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_302 = 0;(v_i_302 <= (-1 + v_N_0)); (++v_i_302)){
        v_user_func_659_306[v_i_302] = divide_uf(v_initial_param_652_303[v_i_302], v_initial_param_653_304[v_i_302]); 
    }
}
}; 