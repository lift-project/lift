
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COPYSIGN_UF_H
#define COPYSIGN_UF_H
; 
float copysign_uf(float x, float y){
    return y<0? x*(-1):x ;; 
}

#endif
 ; 
void copysign(float * v_initial_param_557_238, float * v_initial_param_558_239, float * & v_user_func_564_241, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_564_241 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_237 = 0;(v_i_237 <= (-1 + v_N_0)); (++v_i_237)){
        v_user_func_564_241[v_i_237] = copysign_uf(v_initial_param_557_238[v_i_237], v_initial_param_558_239[v_i_237]); 
    }
}
}; 