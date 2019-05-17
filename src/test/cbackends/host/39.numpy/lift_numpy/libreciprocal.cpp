
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
void reciprocal(float * v_initial_param_596_251, float * & v_user_func_598_252, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_598_252 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_250 = 0;(v_i_250 <= (-1 + v_N_0)); (++v_i_250)){
        v_user_func_598_252[v_i_250] = reciprocal_uf(v_initial_param_596_251[v_i_250]); 
    }
}
}; 