
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
void copysign(float * v_initial_param_541_221, float * v_initial_param_542_222, float * & v_user_func_548_224, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_548_224 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_220 = 0;(v_i_220 <= (-1 + v_N_0)); (++v_i_220)){
        v_user_func_548_224[v_i_220] = copysign_uf(v_initial_param_541_221[v_i_220], v_initial_param_542_222[v_i_220]); 
    }
}
}; 