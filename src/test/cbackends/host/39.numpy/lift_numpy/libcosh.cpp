
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COSH_UF_H
#define COSH_UF_H
; 
float cosh_uf(float x){
    { return cosh(x); }; 
}

#endif
 ; 
void cosh(float * v_initial_param_203_111, float * & v_user_func_205_112, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_205_112 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_110 = 0;(v_i_110 <= (-1 + v_N_0)); (++v_i_110)){
        v_user_func_205_112[v_i_110] = cosh_uf(v_initial_param_203_111[v_i_110]); 
    }
}
}; 