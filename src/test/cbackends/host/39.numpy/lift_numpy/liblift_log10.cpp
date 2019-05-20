
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG10_UF_H
#define LOG10_UF_H
; 
float log10_uf(float x){
    return log10(x) ;; 
}

#endif
 ; 
void lift_log10(float * v_initial_param_510_240, float * & v_user_func_512_241, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_512_241 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_239 = 0;(v_i_239 <= (-1 + v_N_0)); (++v_i_239)){
        v_user_func_512_241[v_i_239] = log10_uf(v_initial_param_510_240[v_i_239]); 
    }
}
}; 