
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
void arctan2(float * v_initial_param_134_56, float * v_initial_param_135_57, float * & v_user_func_137_60, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_145_59 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_137_60 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_55 = 0;(v_i_55 <= (-1 + v_N_0)); (++v_i_55)){
        v_user_func_145_59[v_i_55] = div_uf(v_initial_param_134_56[v_i_55], v_initial_param_135_57[v_i_55]); 
    }
    // For each element processed sequentially
    for (int v_i_54 = 0;(v_i_54 <= (-1 + v_N_0)); (++v_i_54)){
        v_user_func_137_60[v_i_54] = arctan_uf(v_user_func_145_59[v_i_54]); 
    }
}
}; 