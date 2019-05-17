
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
void cosh(float * v_initial_param_197_105, float * & v_user_func_199_106, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_199_106 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_104 = 0;(v_i_104 <= (-1 + v_N_0)); (++v_i_104)){
        v_user_func_199_106[v_i_104] = cosh_uf(v_initial_param_197_105[v_i_104]); 
    }
}
}; 