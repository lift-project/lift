
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
void copysign(float * v_initial_param_542_222, float * v_initial_param_543_223, float * & v_user_func_549_225, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_549_225 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_221 = 0;(v_i_221 <= (-1 + v_N_0)); (++v_i_221)){
        v_user_func_549_225[v_i_221] = copysign_uf(v_initial_param_542_222[v_i_221], v_initial_param_543_223[v_i_221]); 
    }
}
}; 