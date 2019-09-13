
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
void arcsinh(float * v_initial_param_7175_2902, float * & v_user_func_7177_2903, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7177_2903 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2901 = 0;(v_i_2901 <= (-1 + v_N_2763)); (++v_i_2901)){
        v_user_func_7177_2903[v_i_2901] = arcsinh_uf(v_initial_param_7175_2902[v_i_2901]); 
    }
}
}; 