
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef EXPM1_UF_H
#define EXPM1_UF_H
; 
float expm1_uf(float x){
    return exp(x) - 1 ;; 
}

#endif
 ; 
void expm1(float * v_initial_param_435_178, float * & v_user_func_437_179, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_437_179 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_177 = 0;(v_i_177 <= (-1 + v_N_0)); (++v_i_177)){
        v_user_func_437_179[v_i_177] = expm1_uf(v_initial_param_435_178[v_i_177]); 
    }
}
}; 