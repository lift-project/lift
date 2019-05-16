
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
void arctan2(float * v_initial_param_137_66, float * v_initial_param_138_67, float * & v_user_func_140_70, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_148_69 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_140_70 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_65 = 0;(v_i_65 <= (-1 + v_N_0)); (++v_i_65)){
        v_user_func_148_69[v_i_65] = div_uf(v_initial_param_137_66[v_i_65], v_initial_param_138_67[v_i_65]); 
    }
    // For each element processed sequentially
    for (int v_i_64 = 0;(v_i_64 <= (-1 + v_N_0)); (++v_i_64)){
        v_user_func_140_70[v_i_64] = arctan_uf(v_user_func_148_69[v_i_64]); 
    }
}
}; 