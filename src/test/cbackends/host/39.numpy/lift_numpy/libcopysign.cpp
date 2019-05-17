
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
void copysign(float * v_initial_param_544_225, float * v_initial_param_545_226, float * & v_user_func_551_228, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_551_228 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_224 = 0;(v_i_224 <= (-1 + v_N_0)); (++v_i_224)){
        v_user_func_551_228[v_i_224] = copysign_uf(v_initial_param_544_225[v_i_224], v_initial_param_545_226[v_i_224]); 
    }
}
}; 