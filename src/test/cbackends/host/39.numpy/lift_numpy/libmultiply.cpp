
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
void multiply(float * v_initial_param_618_261, float * v_initial_param_619_262, float * & v_user_func_625_264, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_625_264 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_260 = 0;(v_i_260 <= (-1 + v_N_0)); (++v_i_260)){
        v_user_func_625_264[v_i_260] = multiply_uf(v_initial_param_618_261[v_i_260], v_initial_param_619_262[v_i_260]); 
    }
}
}; 