
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
void divide(float * v_initial_param_630_264, float * v_initial_param_631_265, float * & v_user_func_637_267, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_637_267 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_263 = 0;(v_i_263 <= (-1 + v_N_0)); (++v_i_263)){
        v_user_func_637_267[v_i_263] = divide_uf(v_initial_param_630_264[v_i_263], v_initial_param_631_265[v_i_263]); 
    }
}
}; 