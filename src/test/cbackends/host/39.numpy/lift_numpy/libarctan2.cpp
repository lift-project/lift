
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
void arctan2(float * v_initial_param_1424_252, float * v_initial_param_1425_253, float * & v_user_func_1427_256, int v_N_190){
    // Allocate memory for output pointers
    float * v_user_func_1435_255 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float))));
    v_user_func_1427_256 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_251 = 0;(v_i_251 <= (-1 + v_N_190)); (++v_i_251)){
        v_user_func_1435_255[v_i_251] = div_uf(v_initial_param_1424_252[v_i_251], v_initial_param_1425_253[v_i_251]); 
    }
    // For each element processed sequentially
    for (int v_i_250 = 0;(v_i_250 <= (-1 + v_N_190)); (++v_i_250)){
        v_user_func_1427_256[v_i_250] = arctan_uf(v_user_func_1435_255[v_i_250]); 
    }
}
}; 