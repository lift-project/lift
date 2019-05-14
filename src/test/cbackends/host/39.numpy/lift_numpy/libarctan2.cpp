


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
void arctan2(float * v_initial_param_128_49, float * v_initial_param_129_50, float * & v_user_func_131_53, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_139_52 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_131_53 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_48 = 0;(v_i_48 <= (-1 + v_N_0)); (++v_i_48)){
        v_user_func_139_52[v_i_48] = div_uf(v_initial_param_128_49[v_i_48], v_initial_param_129_50[v_i_48]); 
    }
    // For each element processed sequentially
    for (int v_i_47 = 0;(v_i_47 <= (-1 + v_N_0)); (++v_i_47)){
        v_user_func_131_53[v_i_47] = arctan_uf(v_user_func_139_52[v_i_47]); 
    }
}


}

; 