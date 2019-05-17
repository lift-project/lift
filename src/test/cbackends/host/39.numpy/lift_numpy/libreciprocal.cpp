
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RECIPROCAL_UF_H
#define RECIPROCAL_UF_H
; 
float reciprocal_uf(float x){
    return 1.0f/x; 
}

#endif
 ; 
void reciprocal(float * v_initial_param_598_253, float * & v_user_func_600_254, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_600_254 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_252 = 0;(v_i_252 <= (-1 + v_N_0)); (++v_i_252)){
        v_user_func_600_254[v_i_252] = reciprocal_uf(v_initial_param_598_253[v_i_252]); 
    }
}
}; 