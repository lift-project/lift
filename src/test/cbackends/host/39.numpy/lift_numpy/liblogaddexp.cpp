
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOGADDEXP_UF_H
#define LOGADDEXP_UF_H
; 
float logaddexp_uf(float x1, float x2){
    { return log(exp(x1) + exp(x2)); }; 
}

#endif
 ; 
void logaddexp(float * v_initial_param_484_203, float * v_initial_param_485_204, float * & v_user_func_491_206, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_491_206 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_202 = 0;(v_i_202 <= (-1 + v_N_0)); (++v_i_202)){
        v_user_func_491_206[v_i_202] = logaddexp_uf(v_initial_param_484_203[v_i_202], v_initial_param_485_204[v_i_202]); 
    }
}
}; 