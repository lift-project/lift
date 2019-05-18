
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LDEXP_UF_H
#define LDEXP_UF_H
; 
float ldexp_uf(float x, float y){
    return x* pow(2,y) ;; 
}

#endif
 ; 
void ldexp(float * v_initial_param_578_246, float * v_initial_param_579_247, float * & v_user_func_585_249, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_585_249 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_245 = 0;(v_i_245 <= (-1 + v_N_0)); (++v_i_245)){
        v_user_func_585_249[v_i_245] = ldexp_uf(v_initial_param_578_246[v_i_245], v_initial_param_579_247[v_i_245]); 
    }
}
}; 