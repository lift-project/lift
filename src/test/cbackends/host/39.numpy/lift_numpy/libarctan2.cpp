


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
void arctan2(float * v_initial_param_133_54, float * v_initial_param_134_55, float * & v_user_func_136_58, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_144_57 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_136_58 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_53 = 0;(v_i_53 <= (-1 + v_N_0)); (++v_i_53)){
        v_user_func_144_57[v_i_53] = div_uf(v_initial_param_133_54[v_i_53], v_initial_param_134_55[v_i_53]); 
    }
    // For each element processed sequentially
    for (int v_i_52 = 0;(v_i_52 <= (-1 + v_N_0)); (++v_i_52)){
        v_user_func_136_58[v_i_52] = arctan_uf(v_user_func_144_57[v_i_52]); 
    }
}


}

; 