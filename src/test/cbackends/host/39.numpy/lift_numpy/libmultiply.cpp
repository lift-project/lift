
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
void multiply(float * v_initial_param_620_263, float * v_initial_param_621_264, float * & v_user_func_627_266, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_627_266 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_262 = 0;(v_i_262 <= (-1 + v_N_0)); (++v_i_262)){
        v_user_func_627_266[v_i_262] = multiply_uf(v_initial_param_620_263[v_i_262], v_initial_param_621_264[v_i_262]); 
    }
}
}; 