
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
void arctan2(float * v_initial_param_135_61, float * v_initial_param_136_62, float * & v_user_func_138_65, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_146_64 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_138_65 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_60 = 0;(v_i_60 <= (-1 + v_N_0)); (++v_i_60)){
        v_user_func_146_64[v_i_60] = div_uf(v_initial_param_135_61[v_i_60], v_initial_param_136_62[v_i_60]); 
    }
    // For each element processed sequentially
    for (int v_i_59 = 0;(v_i_59 <= (-1 + v_N_0)); (++v_i_59)){
        v_user_func_138_65[v_i_59] = arctan_uf(v_user_func_146_64[v_i_59]); 
    }
}
}; 