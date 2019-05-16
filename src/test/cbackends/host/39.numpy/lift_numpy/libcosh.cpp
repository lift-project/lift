
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
void cosh(float * v_initial_param_180_86, float * & v_user_func_182_87, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_182_87 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_85 = 0;(v_i_85 <= (-1 + v_N_0)); (++v_i_85)){
        v_user_func_182_87[v_i_85] = cosh_uf(v_initial_param_180_86[v_i_85]); 
    }
}
}; 