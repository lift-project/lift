
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
void arctan2(float * v_initial_param_155_86, float * v_initial_param_156_87, float * & v_user_func_158_90, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_166_89 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_158_90 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_85 = 0;(v_i_85 <= (-1 + v_N_0)); (++v_i_85)){
        v_user_func_166_89[v_i_85] = div_uf(v_initial_param_155_86[v_i_85], v_initial_param_156_87[v_i_85]); 
    }
    // For each element processed sequentially
    for (int v_i_84 = 0;(v_i_84 <= (-1 + v_N_0)); (++v_i_84)){
        v_user_func_158_90[v_i_84] = arctan_uf(v_user_func_166_89[v_i_84]); 
    }
}
}; 