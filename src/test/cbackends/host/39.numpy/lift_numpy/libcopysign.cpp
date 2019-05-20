
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
void copysign(float * v_initial_param_587_265, float * v_initial_param_588_266, float * & v_user_func_594_268, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_594_268 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_264 = 0;(v_i_264 <= (-1 + v_N_0)); (++v_i_264)){
        v_user_func_594_268[v_i_264] = copysign_uf(v_initial_param_587_265[v_i_264], v_initial_param_588_266[v_i_264]); 
    }
}
}; 