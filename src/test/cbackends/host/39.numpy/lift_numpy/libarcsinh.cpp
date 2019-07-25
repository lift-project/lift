
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
void arcsinh(float * v_initial_param_3231_491, float * & v_user_func_3233_492, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3233_492 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_490 = 0;(v_i_490 <= (-1 + v_N_352)); (++v_i_490)){
        v_user_func_3233_492[v_i_490] = arcsinh_uf(v_initial_param_3231_491[v_i_490]); 
    }
}
}; 