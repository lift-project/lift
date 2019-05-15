
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
void arctan2(float * v_initial_param_134_55, float * v_initial_param_135_56, float * & v_user_func_137_59, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_145_58 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_137_59 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_54 = 0;(v_i_54 <= (-1 + v_N_0)); (++v_i_54)){
        v_user_func_145_58[v_i_54] = div_uf(v_initial_param_134_55[v_i_54], v_initial_param_135_56[v_i_54]); 
    }
    // For each element processed sequentially
    for (int v_i_53 = 0;(v_i_53 <= (-1 + v_N_0)); (++v_i_53)){
        v_user_func_137_59[v_i_53] = arctan_uf(v_user_func_145_58[v_i_53]); 
    }
}
}; 