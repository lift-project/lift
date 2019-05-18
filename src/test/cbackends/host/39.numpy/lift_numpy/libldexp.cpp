
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
void ldexp(float * v_initial_param_574_242, float * v_initial_param_575_243, float * & v_user_func_581_245, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_581_245 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_241 = 0;(v_i_241 <= (-1 + v_N_0)); (++v_i_241)){
        v_user_func_581_245[v_i_241] = ldexp_uf(v_initial_param_574_242[v_i_241], v_initial_param_575_243[v_i_241]); 
    }
}
}; 