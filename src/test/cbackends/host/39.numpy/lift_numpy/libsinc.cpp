
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINC_UF_H
#define SINC_UF_H
; 
float sinc_uf(float x){
    return sin(M_PI*x)/(M_PI*x) ;; 
}

#endif
 ; 
void sinc(float * v_initial_param_509_210, float * & v_user_func_511_211, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_511_211 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_209 = 0;(v_i_209 <= (-1 + v_N_0)); (++v_i_209)){
        v_user_func_511_211[v_i_209] = sinc_uf(v_initial_param_509_210[v_i_209]); 
    }
}
}; 