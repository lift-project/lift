
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
void multiply(float * v_initial_param_621_264, float * v_initial_param_622_265, float * & v_user_func_628_267, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_628_267 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_263 = 0;(v_i_263 <= (-1 + v_N_0)); (++v_i_263)){
        v_user_func_628_267[v_i_263] = multiply_uf(v_initial_param_621_264[v_i_263], v_initial_param_622_265[v_i_263]); 
    }
}
}; 