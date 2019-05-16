
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
void lift_exp(float * v_initial_param_435_182, float * & v_user_func_437_183, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_437_183 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_181 = 0;(v_i_181 <= (-1 + v_N_0)); (++v_i_181)){
        v_user_func_437_183[v_i_181] = exp_uf(v_initial_param_435_182[v_i_181]); 
    }
}
}; 