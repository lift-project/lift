
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
void ldexp(float * v_initial_param_573_241, float * v_initial_param_574_242, float * & v_user_func_580_244, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_580_244 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_240 = 0;(v_i_240 <= (-1 + v_N_0)); (++v_i_240)){
        v_user_func_580_244[v_i_240] = ldexp_uf(v_initial_param_573_241[v_i_240], v_initial_param_574_242[v_i_240]); 
    }
}
}; 