
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
void sinc(float * v_initial_param_534_223, float * & v_user_func_536_224, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_536_224 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_222 = 0;(v_i_222 <= (-1 + v_N_0)); (++v_i_222)){
        v_user_func_536_224[v_i_222] = sinc_uf(v_initial_param_534_223[v_i_222]); 
    }
}
}; 