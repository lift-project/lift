
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
void expm1(float * v_initial_param_453_198, float * & v_user_func_455_199, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_455_199 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_197 = 0;(v_i_197 <= (-1 + v_N_0)); (++v_i_197)){
        v_user_func_455_199[v_i_197] = expm1_uf(v_initial_param_453_198[v_i_197]); 
    }
}
}; 