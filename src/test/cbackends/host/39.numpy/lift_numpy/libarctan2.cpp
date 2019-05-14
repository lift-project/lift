


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
void arctan2(float * v_initial_param_1155_222, float * v_initial_param_1156_223, float * & v_user_func_1158_226, int v_N_190){
    // Allocate memory for output pointers
    float * v_user_func_1166_225 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float))));
    v_user_func_1158_226 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_221 = 0;(v_i_221 <= (-1 + v_N_190)); (++v_i_221)){
        v_user_func_1166_225[v_i_221] = div_uf(v_initial_param_1155_222[v_i_221], v_initial_param_1156_223[v_i_221]); 
    }
    // For each element processed sequentially
    for (int v_i_220 = 0;(v_i_220 <= (-1 + v_N_190)); (++v_i_220)){
        v_user_func_1158_226[v_i_220] = arctan_uf(v_user_func_1166_225[v_i_220]); 
    }
}


}

; 