
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
void ldexp(float * v_initial_param_562_228, float * v_initial_param_563_229, float * & v_user_func_569_231, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_569_231 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_227 = 0;(v_i_227 <= (-1 + v_N_0)); (++v_i_227)){
        v_user_func_569_231[v_i_227] = ldexp_uf(v_initial_param_562_228[v_i_227], v_initial_param_563_229[v_i_227]); 
    }
}
}; 