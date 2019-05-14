


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef CEIL_UF_H
#define CEIL_UF_H


; 
float ceil_uf(float x){
    
    
    return ceil(x);
    
    ; 
}



#endif
; 
void ceil(float * v_initial_param_240_105, float * & v_user_func_242_106, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_242_106 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_104 = 0;(v_i_104 <= (-1 + v_N_0)); (++v_i_104)){
        v_user_func_242_106[v_i_104] = ceil_uf(v_initial_param_240_105[v_i_104]); 
    }
}


}

; 