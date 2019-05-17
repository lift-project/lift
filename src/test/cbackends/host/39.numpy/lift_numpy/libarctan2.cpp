
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
void arctan2(float * v_initial_param_157_88, float * v_initial_param_158_89, float * & v_user_func_160_92, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_168_91 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_160_92 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_87 = 0;(v_i_87 <= (-1 + v_N_0)); (++v_i_87)){
        v_user_func_168_91[v_i_87] = div_uf(v_initial_param_157_88[v_i_87], v_initial_param_158_89[v_i_87]); 
    }
    // For each element processed sequentially
    for (int v_i_86 = 0;(v_i_86 <= (-1 + v_N_0)); (++v_i_86)){
        v_user_func_160_92[v_i_86] = arctan_uf(v_user_func_168_91[v_i_86]); 
    }
}
}; 