


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
void arctan2(float * v_initial_param_129_50, float * v_initial_param_130_51, float * & v_user_func_132_54, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_140_53 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_132_54 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_49 = 0;(v_i_49 <= (-1 + v_N_0)); (++v_i_49)){
        v_user_func_140_53[v_i_49] = div_uf(v_initial_param_129_50[v_i_49], v_initial_param_130_51[v_i_49]); 
    }
    // For each element processed sequentially
    for (int v_i_48 = 0;(v_i_48 <= (-1 + v_N_0)); (++v_i_48)){
        v_user_func_132_54[v_i_48] = arctan_uf(v_user_func_140_53[v_i_48]); 
    }
}


}

; 