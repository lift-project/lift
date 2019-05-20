
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
void cosh(float * v_initial_param_221_135, float * & v_user_func_223_136, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_223_136 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_134 = 0;(v_i_134 <= (-1 + v_N_0)); (++v_i_134)){
        v_user_func_223_136[v_i_134] = cosh_uf(v_initial_param_221_135[v_i_134]); 
    }
}
}; 