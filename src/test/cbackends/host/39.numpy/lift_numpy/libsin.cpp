


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef SIN_UF_H
#define SIN_UF_H


; 
float sin_uf(float x){
    
    
    { return sin(x); }
    
    ; 
}



#endif
; 
void sin(float * v_initial_param_72_24, float * & v_user_func_74_25, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_74_25 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_23 = 0;(v_i_23 <= (-1 + v_N_0)); (++v_i_23)){
        v_user_func_74_25[v_i_23] = sin_uf(v_initial_param_72_24[v_i_23]); 
    }
}


}

; 