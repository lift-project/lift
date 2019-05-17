
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
void ldexp(float * v_initial_param_568_236, float * v_initial_param_569_237, float * & v_user_func_575_239, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_575_239 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_235 = 0;(v_i_235 <= (-1 + v_N_0)); (++v_i_235)){
        v_user_func_575_239[v_i_235] = ldexp_uf(v_initial_param_568_236[v_i_235], v_initial_param_569_237[v_i_235]); 
    }
}
}; 