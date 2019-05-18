
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
void divide(float * v_initial_param_641_275, float * v_initial_param_642_276, float * & v_user_func_648_278, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_648_278 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_274 = 0;(v_i_274 <= (-1 + v_N_0)); (++v_i_274)){
        v_user_func_648_278[v_i_274] = divide_uf(v_initial_param_641_275[v_i_274], v_initial_param_642_276[v_i_274]); 
    }
}
}; 