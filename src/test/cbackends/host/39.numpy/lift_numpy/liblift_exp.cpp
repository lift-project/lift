
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
void lift_exp(float * v_initial_param_463_212, float * & v_user_func_465_213, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_465_213 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_211 = 0;(v_i_211 <= (-1 + v_N_0)); (++v_i_211)){
        v_user_func_465_213[v_i_211] = exp_uf(v_initial_param_463_212[v_i_211]); 
    }
}
}; 