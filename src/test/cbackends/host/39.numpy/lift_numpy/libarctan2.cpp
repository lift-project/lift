


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
void arctan2(float * v_initial_param_122_43, float * v_initial_param_123_44, float * & v_user_func_125_47, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_133_46 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_125_47 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_42 = 0;(v_i_42 <= (-1 + v_N_0)); (++v_i_42)){
        v_user_func_133_46[v_i_42] = div_uf(v_initial_param_122_43[v_i_42], v_initial_param_123_44[v_i_42]); 
    }
    // For each element processed sequentially
    for (int v_i_41 = 0;(v_i_41 <= (-1 + v_N_0)); (++v_i_41)){
        v_user_func_125_47[v_i_41] = arctan_uf(v_user_func_133_46[v_i_41]); 
    }
}


}

; 