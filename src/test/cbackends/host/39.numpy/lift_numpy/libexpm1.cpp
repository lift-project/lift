
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
void expm1(float * v_initial_param_437_180, float * & v_user_func_439_181, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_439_181 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_179 = 0;(v_i_179 <= (-1 + v_N_0)); (++v_i_179)){
        v_user_func_439_181[v_i_179] = expm1_uf(v_initial_param_437_180[v_i_179]); 
    }
}
}; 