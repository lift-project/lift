
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
void trunc(float * v_initial_param_273_144, float * & v_user_func_275_145, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_275_145 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_143 = 0;(v_i_143 <= (-1 + v_N_0)); (++v_i_143)){
        v_user_func_275_145[v_i_143] = trunc_uf(v_initial_param_273_144[v_i_143]); 
    }
}
}; 