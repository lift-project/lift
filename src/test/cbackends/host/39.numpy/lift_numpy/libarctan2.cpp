
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
void arctan2(float * v_initial_param_136_65, float * v_initial_param_137_66, float * & v_user_func_139_69, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_147_68 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_139_69 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_64 = 0;(v_i_64 <= (-1 + v_N_0)); (++v_i_64)){
        v_user_func_147_68[v_i_64] = div_uf(v_initial_param_136_65[v_i_64], v_initial_param_137_66[v_i_64]); 
    }
    // For each element processed sequentially
    for (int v_i_63 = 0;(v_i_63 <= (-1 + v_N_0)); (++v_i_63)){
        v_user_func_139_69[v_i_63] = arctan_uf(v_user_func_147_68[v_i_63]); 
    }
}
}; 