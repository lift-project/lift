
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
void ldexp(float * v_initial_param_589_259, float * v_initial_param_590_260, float * & v_user_func_596_262, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_596_262 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_258 = 0;(v_i_258 <= (-1 + v_N_0)); (++v_i_258)){
        v_user_func_596_262[v_i_258] = ldexp_uf(v_initial_param_589_259[v_i_258], v_initial_param_590_260[v_i_258]); 
    }
}
}; 