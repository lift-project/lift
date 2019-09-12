
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
void lift_exp(float * v_initial_param_7435_2995, float * & v_user_func_7437_2996, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7437_2996 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2994 = 0;(v_i_2994 <= (-1 + v_N_2763)); (++v_i_2994)){
        v_user_func_7437_2996[v_i_2994] = exp_uf(v_initial_param_7435_2995[v_i_2994]); 
    }
}
}; 