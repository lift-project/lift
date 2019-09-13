
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
void sinc(float * v_initial_param_7526_3026, float * & v_user_func_7528_3027, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7528_3027 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3025 = 0;(v_i_3025 <= (-1 + v_N_2763)); (++v_i_3025)){
        v_user_func_7528_3027[v_i_3025] = sinc_uf(v_initial_param_7526_3026[v_i_3025]); 
    }
}
}; 