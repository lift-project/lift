


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef COSH_UF_H
#define COSH_UF_H


; 
float cosh_uf(float x){
    
    
    { return cosh(x); }
    
    ; 
}



#endif
; 
void cosh(float * v_initial_param_171_69, float * & v_user_func_173_70, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_173_70 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_68 = 0;(v_i_68 <= (-1 + v_N_0)); (++v_i_68)){
        v_user_func_173_70[v_i_68] = cosh_uf(v_initial_param_171_69[v_i_68]); 
    }
}


}

; 