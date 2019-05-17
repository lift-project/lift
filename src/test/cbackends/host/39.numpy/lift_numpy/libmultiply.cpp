
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
void multiply(float * v_initial_param_617_260, float * v_initial_param_618_261, float * & v_user_func_624_263, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_624_263 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_259 = 0;(v_i_259 <= (-1 + v_N_0)); (++v_i_259)){
        v_user_func_624_263[v_i_259] = multiply_uf(v_initial_param_617_260[v_i_259], v_initial_param_618_261[v_i_259]); 
    }
}
}; 