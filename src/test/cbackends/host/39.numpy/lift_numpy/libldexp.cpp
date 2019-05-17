
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
void ldexp(float * v_initial_param_572_240, float * v_initial_param_573_241, float * & v_user_func_579_243, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_579_243 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_239 = 0;(v_i_239 <= (-1 + v_N_0)); (++v_i_239)){
        v_user_func_579_243[v_i_239] = ldexp_uf(v_initial_param_572_240[v_i_239], v_initial_param_573_241[v_i_239]); 
    }
}
}; 