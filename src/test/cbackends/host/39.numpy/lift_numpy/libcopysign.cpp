
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
void copysign(float * v_initial_param_600_271, float * v_initial_param_601_272, float * & v_user_func_607_274, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_607_274 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_270 = 0;(v_i_270 <= (-1 + v_N_0)); (++v_i_270)){
        v_user_func_607_274[v_i_270] = copysign_uf(v_initial_param_600_271[v_i_270], v_initial_param_601_272[v_i_270]); 
    }
}
}; 