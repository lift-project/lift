


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
void arctan2(float * v_initial_param_119_40, float * v_initial_param_120_41, float * & v_user_func_122_44, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_130_43 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_122_44 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_39 = 0;(v_i_39 <= (-1 + v_N_0)); (++v_i_39)){
        v_user_func_130_43[v_i_39] = div_uf(v_initial_param_119_40[v_i_39], v_initial_param_120_41[v_i_39]); 
    }
    // For each element processed sequentially
    for (int v_i_38 = 0;(v_i_38 <= (-1 + v_N_0)); (++v_i_38)){
        v_user_func_122_44[v_i_38] = arctan_uf(v_user_func_130_43[v_i_38]); 
    }
}


}

; 