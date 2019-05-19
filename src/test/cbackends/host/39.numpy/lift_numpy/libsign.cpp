
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIGN_UF_H
#define SIGN_UF_H
; 
float sign_uf(float x){
    { return x>=0?1:-1; }; 
}

#endif
 ; 
void sign(float * v_initial_param_869_375, float * & v_user_func_871_376, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_871_376 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_374 = 0;(v_i_374 <= (-1 + v_N_0)); (++v_i_374)){
        v_user_func_871_376[v_i_374] = sign_uf(v_initial_param_869_375[v_i_374]); 
    }
}
}; 