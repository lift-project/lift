
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
void arctan2(float * v_initial_param_138_67, float * v_initial_param_139_68, float * & v_user_func_141_71, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_149_70 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_141_71 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_66 = 0;(v_i_66 <= (-1 + v_N_0)); (++v_i_66)){
        v_user_func_149_70[v_i_66] = div_uf(v_initial_param_138_67[v_i_66], v_initial_param_139_68[v_i_66]); 
    }
    // For each element processed sequentially
    for (int v_i_65 = 0;(v_i_65 <= (-1 + v_N_0)); (++v_i_65)){
        v_user_func_141_71[v_i_65] = arctan_uf(v_user_func_149_70[v_i_65]); 
    }
}
}; 