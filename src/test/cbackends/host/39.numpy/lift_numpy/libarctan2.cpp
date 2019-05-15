
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
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
void arctan2(float * v_initial_param_135_64, float * v_initial_param_136_65, float * & v_user_func_138_68, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_146_67 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_138_68 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_63 = 0;(v_i_63 <= (-1 + v_N_0)); (++v_i_63)){
        v_user_func_146_67[v_i_63] = div_uf(v_initial_param_135_64[v_i_63], v_initial_param_136_65[v_i_63]); 
    }
    // For each element processed sequentially
    for (int v_i_62 = 0;(v_i_62 <= (-1 + v_N_0)); (++v_i_62)){
        v_user_func_138_68[v_i_62] = arctan_uf(v_user_func_146_67[v_i_62]); 
    }
}
}; 