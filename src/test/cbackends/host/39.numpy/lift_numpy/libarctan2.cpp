
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
void arctan2(float * v_initial_param_177_114, float * v_initial_param_178_115, float * & v_user_func_180_118, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_188_117 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_180_118 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_113 = 0;(v_i_113 <= (-1 + v_N_0)); (++v_i_113)){
        v_user_func_188_117[v_i_113] = div_uf(v_initial_param_177_114[v_i_113], v_initial_param_178_115[v_i_113]); 
    }
    // For each element processed sequentially
    for (int v_i_112 = 0;(v_i_112 <= (-1 + v_N_0)); (++v_i_112)){
        v_user_func_180_118[v_i_112] = arctan_uf(v_user_func_188_117[v_i_112]); 
    }
}
}; 