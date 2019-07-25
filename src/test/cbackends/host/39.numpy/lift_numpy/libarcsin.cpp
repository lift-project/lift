
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H
; 
float arcsin_uf(float x){
    { return asin(x); }; 
}

#endif
 ; 
void arcsin(float * v_initial_param_3138_449, float * & v_user_func_3140_450, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3140_450 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_448 = 0;(v_i_448 <= (-1 + v_N_352)); (++v_i_448)){
        v_user_func_3140_450[v_i_448] = arcsin_uf(v_initial_param_3138_449[v_i_448]); 
    }
}
}; 