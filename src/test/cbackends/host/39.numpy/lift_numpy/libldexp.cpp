
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
void ldexp(float * v_initial_param_569_237, float * v_initial_param_570_238, float * & v_user_func_576_240, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_576_240 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_236 = 0;(v_i_236 <= (-1 + v_N_0)); (++v_i_236)){
        v_user_func_576_240[v_i_236] = ldexp_uf(v_initial_param_569_237[v_i_236], v_initial_param_570_238[v_i_236]); 
    }
}
}; 