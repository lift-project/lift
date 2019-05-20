
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
void ldexp(float * v_initial_param_621_279, float * v_initial_param_622_280, float * & v_user_func_628_282, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_628_282 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_278 = 0;(v_i_278 <= (-1 + v_N_0)); (++v_i_278)){
        v_user_func_628_282[v_i_278] = ldexp_uf(v_initial_param_621_279[v_i_278], v_initial_param_622_280[v_i_278]); 
    }
}
}; 