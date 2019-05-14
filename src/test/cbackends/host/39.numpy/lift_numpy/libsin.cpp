


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
void sin(float * v_initial_param_65_17, float * & v_user_func_67_18, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_67_18 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_16 = 0;(v_i_16 <= (-1 + v_N_0)); (++v_i_16)){
        v_user_func_67_18[v_i_16] = sin_uf(v_initial_param_65_17[v_i_16]); 
    }
}


}

; 