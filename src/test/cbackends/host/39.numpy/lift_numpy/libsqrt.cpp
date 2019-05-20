
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SQRT_UF_H
#define SQRT_UF_H
; 
float sqrt_uf(float x){
    { return sqrt(x); }; 
}

#endif
 ; 
void sqrt(float * v_initial_param_873_382, float * & v_user_func_875_383, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_875_383 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_381 = 0;(v_i_381 <= (-1 + v_N_0)); (++v_i_381)){
        v_user_func_875_383[v_i_381] = sqrt_uf(v_initial_param_873_382[v_i_381]); 
    }
}
}; 