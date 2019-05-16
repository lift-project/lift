
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
void sinc(float * v_initial_param_527_215, float * & v_user_func_529_216, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_529_216 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_214 = 0;(v_i_214 <= (-1 + v_N_0)); (++v_i_214)){
        v_user_func_529_216[v_i_214] = sinc_uf(v_initial_param_527_215[v_i_214]); 
    }
}
}; 