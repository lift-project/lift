
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
void arctan2(float * v_initial_param_177_108, float * v_initial_param_178_109, float * & v_user_func_180_112, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_188_111 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_180_112 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_107 = 0;(v_i_107 <= (-1 + v_N_0)); (++v_i_107)){
        v_user_func_188_111[v_i_107] = div_uf(v_initial_param_177_108[v_i_107], v_initial_param_178_109[v_i_107]); 
    }
    // For each element processed sequentially
    for (int v_i_106 = 0;(v_i_106 <= (-1 + v_N_0)); (++v_i_106)){
        v_user_func_180_112[v_i_106] = arctan_uf(v_user_func_188_111[v_i_106]); 
    }
}
}; 