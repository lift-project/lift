
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef EXP_UF_H
#define EXP_UF_H
; 
float exp_uf(float x){
    return exp(x) ;; 
}

#endif
 ; 
void exp(float * v_initial_param_422_169, float * & v_user_func_424_170, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_424_170 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_168 = 0;(v_i_168 <= (-1 + v_N_0)); (++v_i_168)){
        v_user_func_424_170[v_i_168] = exp_uf(v_initial_param_422_169[v_i_168]); 
    }
}
}; 