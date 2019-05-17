
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
void true_divide(float * v_initial_param_633_282, float * v_initial_param_634_283, float * & v_user_func_640_285, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_640_285 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_281 = 0;(v_i_281 <= (-1 + v_N_0)); (++v_i_281)){
        v_user_func_640_285[v_i_281] = divide_uf(v_initial_param_633_282[v_i_281], v_initial_param_634_283[v_i_281]); 
    }
}
}; 