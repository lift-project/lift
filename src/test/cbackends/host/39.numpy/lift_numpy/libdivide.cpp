
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
void divide(float * v_initial_param_641_277, float * v_initial_param_642_278, float * & v_user_func_648_280, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_648_280 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_276 = 0;(v_i_276 <= (-1 + v_N_0)); (++v_i_276)){
        v_user_func_648_280[v_i_276] = divide_uf(v_initial_param_641_277[v_i_276], v_initial_param_642_278[v_i_276]); 
    }
}
}; 