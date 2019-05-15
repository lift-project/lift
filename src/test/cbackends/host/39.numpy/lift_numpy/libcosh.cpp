
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef COSH_UF_H
#define COSH_UF_H
; 
float cosh_uf(float x){
    { return cosh(x); }; 
}

#endif
 ; 
void cosh(float * v_initial_param_179_85, float * & v_user_func_181_86, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_181_86 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_84 = 0;(v_i_84 <= (-1 + v_N_0)); (++v_i_84)){
        v_user_func_181_86[v_i_84] = cosh_uf(v_initial_param_179_85[v_i_84]); 
    }
}
}; 