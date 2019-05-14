


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef DIV_UF_H
#define DIV_UF_H


; 
float div_uf(float x, float y){
    
    
    { return (x)/(y); }
    
    ; 
}



#endif
; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H


; 
float arctan_uf(float x){
    
    
    { return atan(x); }
    
    ; 
}



#endif
; 
void arctan2(float * v_initial_param_130_51, float * v_initial_param_131_52, float * & v_user_func_133_55, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_141_54 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_133_55 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_50 = 0;(v_i_50 <= (-1 + v_N_0)); (++v_i_50)){
        v_user_func_141_54[v_i_50] = div_uf(v_initial_param_130_51[v_i_50], v_initial_param_131_52[v_i_50]); 
    }
    // For each element processed sequentially
    for (int v_i_49 = 0;(v_i_49 <= (-1 + v_N_0)); (++v_i_49)){
        v_user_func_133_55[v_i_49] = arctan_uf(v_user_func_141_54[v_i_49]); 
    }
}


}

; 