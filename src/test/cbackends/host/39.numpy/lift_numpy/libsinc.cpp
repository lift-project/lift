
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
void sinc(float * v_initial_param_573_259, float * & v_user_func_575_260, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_575_260 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_258 = 0;(v_i_258 <= (-1 + v_N_0)); (++v_i_258)){
        v_user_func_575_260[v_i_258] = sinc_uf(v_initial_param_573_259[v_i_258]); 
    }
}
}; 