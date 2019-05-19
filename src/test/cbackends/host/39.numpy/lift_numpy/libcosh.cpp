
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
void cosh(float * v_initial_param_221_131, float * & v_user_func_223_132, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_223_132 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_130 = 0;(v_i_130 <= (-1 + v_N_0)); (++v_i_130)){
        v_user_func_223_132[v_i_130] = cosh_uf(v_initial_param_221_131[v_i_130]); 
    }
}
}; 