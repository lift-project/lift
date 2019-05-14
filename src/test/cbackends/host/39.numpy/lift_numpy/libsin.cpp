


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
void sin(float * v_initial_param_1113_203, float * & v_user_func_1115_204, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1115_204 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_202 = 0;(v_i_202 <= (-1 + v_N_190)); (++v_i_202)){
        v_user_func_1115_204[v_i_202] = sin_uf(v_initial_param_1113_203[v_i_202]); 
    }
}


}

; 