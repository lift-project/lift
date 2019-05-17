
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
void copysign(float * v_initial_param_547_228, float * v_initial_param_548_229, float * & v_user_func_554_231, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_554_231 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_227 = 0;(v_i_227 <= (-1 + v_N_0)); (++v_i_227)){
        v_user_func_554_231[v_i_227] = copysign_uf(v_initial_param_547_228[v_i_227], v_initial_param_548_229[v_i_227]); 
    }
}
}; 