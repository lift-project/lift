
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif
 ; 
void trunc(float * v_initial_param_275_146, float * & v_user_func_277_147, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_277_147 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_145 = 0;(v_i_145 <= (-1 + v_N_0)); (++v_i_145)){
        v_user_func_277_147[v_i_145] = trunc_uf(v_initial_param_275_146[v_i_145]); 
    }
}
}; 