
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef DIV_UF_H
#define DIV_UF_H
; 
float div_uf(float x, float y){
    { return (x)/(y); }; 
}

#endif; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif; 
void arctan2(float * v_initial_param_134_58, float * v_initial_param_135_59, float * & v_user_func_137_62, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_145_61 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_137_62 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_57 = 0;(v_i_57 <= (-1 + v_N_0)); (++v_i_57)){
        v_user_func_145_61[v_i_57] = div_uf(v_initial_param_134_58[v_i_57], v_initial_param_135_59[v_i_57]); 
    }
    // For each element processed sequentially
    for (int v_i_56 = 0;(v_i_56 <= (-1 + v_N_0)); (++v_i_56)){
        v_user_func_137_62[v_i_56] = arctan_uf(v_user_func_145_61[v_i_56]); 
    }
}
}; 