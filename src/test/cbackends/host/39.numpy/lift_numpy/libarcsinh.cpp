
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H
; 
float arcsinh_uf(float x){
    { return asinh(x); }; 
}

#endif
 ; 
void arcsinh(float * v_initial_param_1482_279, float * & v_user_func_1484_280, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1484_280 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_278 = 0;(v_i_278 <= (-1 + v_N_190)); (++v_i_278)){
        v_user_func_1484_280[v_i_278] = arcsinh_uf(v_initial_param_1482_279[v_i_278]); 
    }
}
}; 