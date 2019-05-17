
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
void sinc(float * v_initial_param_531_220, float * & v_user_func_533_221, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_533_221 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_219 = 0;(v_i_219 <= (-1 + v_N_0)); (++v_i_219)){
        v_user_func_533_221[v_i_219] = sinc_uf(v_initial_param_531_220[v_i_219]); 
    }
}
}; 