
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
void divide(float * v_initial_param_631_265, float * v_initial_param_632_266, float * & v_user_func_638_268, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_638_268 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_264 = 0;(v_i_264 <= (-1 + v_N_0)); (++v_i_264)){
        v_user_func_638_268[v_i_264] = divide_uf(v_initial_param_631_265[v_i_264], v_initial_param_632_266[v_i_264]); 
    }
}
}; 