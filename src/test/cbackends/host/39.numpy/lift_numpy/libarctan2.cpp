
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIV_UF_H
#define DIV_UF_H
; 
float div_uf(float x, float y){
    { return (x)/(y); }; 
}

#endif
 ; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan2(float * v_initial_param_161_92, float * v_initial_param_162_93, float * & v_user_func_164_96, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_172_95 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_164_96 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_91 = 0;(v_i_91 <= (-1 + v_N_0)); (++v_i_91)){
        v_user_func_172_95[v_i_91] = div_uf(v_initial_param_161_92[v_i_91], v_initial_param_162_93[v_i_91]); 
    }
    // For each element processed sequentially
    for (int v_i_90 = 0;(v_i_90 <= (-1 + v_N_0)); (++v_i_90)){
        v_user_func_164_96[v_i_90] = arctan_uf(v_user_func_172_95[v_i_90]); 
    }
}
}; 