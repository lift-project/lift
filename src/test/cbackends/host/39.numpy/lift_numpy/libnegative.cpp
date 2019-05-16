
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
float negative_uf(float x){
    return (-1.0f)*x; 
}

#endif
 ; 
void negative(float * v_initial_param_606_253, float * & v_user_func_608_254, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_608_254 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_252 = 0;(v_i_252 <= (-1 + v_N_0)); (++v_i_252)){
        v_user_func_608_254[v_i_252] = negative_uf(v_initial_param_606_253[v_i_252]); 
    }
}
}; 