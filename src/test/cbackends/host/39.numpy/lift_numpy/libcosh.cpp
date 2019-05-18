
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
void cosh(float * v_initial_param_206_114, float * & v_user_func_208_115, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_208_115 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_113 = 0;(v_i_113 <= (-1 + v_N_0)); (++v_i_113)){
        v_user_func_208_115[v_i_113] = cosh_uf(v_initial_param_206_114[v_i_113]); 
    }
}
}; 