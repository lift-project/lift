


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
void arctan2(float * v_initial_param_123_44, float * v_initial_param_124_45, float * & v_user_func_126_48, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_134_47 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_126_48 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_43 = 0;(v_i_43 <= (-1 + v_N_0)); (++v_i_43)){
        v_user_func_134_47[v_i_43] = div_uf(v_initial_param_123_44[v_i_43], v_initial_param_124_45[v_i_43]); 
    }
    // For each element processed sequentially
    for (int v_i_42 = 0;(v_i_42 <= (-1 + v_N_0)); (++v_i_42)){
        v_user_func_126_48[v_i_42] = arctan_uf(v_user_func_134_47[v_i_42]); 
    }
}


}

; 