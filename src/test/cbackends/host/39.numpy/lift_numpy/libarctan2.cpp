
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
void arctan2(float * v_initial_param_158_89, float * v_initial_param_159_90, float * & v_user_func_161_93, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_169_92 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_161_93 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_88 = 0;(v_i_88 <= (-1 + v_N_0)); (++v_i_88)){
        v_user_func_169_92[v_i_88] = div_uf(v_initial_param_158_89[v_i_88], v_initial_param_159_90[v_i_88]); 
    }
    // For each element processed sequentially
    for (int v_i_87 = 0;(v_i_87 <= (-1 + v_N_0)); (++v_i_87)){
        v_user_func_161_93[v_i_87] = arctan_uf(v_user_func_169_92[v_i_87]); 
    }
}
}; 