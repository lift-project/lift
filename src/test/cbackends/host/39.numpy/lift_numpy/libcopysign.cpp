
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
void copysign(float * v_initial_param_556_237, float * v_initial_param_557_238, float * & v_user_func_563_240, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_563_240 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_236 = 0;(v_i_236 <= (-1 + v_N_0)); (++v_i_236)){
        v_user_func_563_240[v_i_236] = copysign_uf(v_initial_param_556_237[v_i_236], v_initial_param_557_238[v_i_236]); 
    }
}
}; 