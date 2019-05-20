
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
void sqrt(float * v_initial_param_860_376, float * & v_user_func_862_377, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_862_377 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_375 = 0;(v_i_375 <= (-1 + v_N_0)); (++v_i_375)){
        v_user_func_862_377[v_i_375] = sqrt_uf(v_initial_param_860_376[v_i_375]); 
    }
}
}; 