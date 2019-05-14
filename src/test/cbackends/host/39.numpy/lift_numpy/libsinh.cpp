


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef SINH_UF_H
#define SINH_UF_H


; 
float sinh_uf(float x){
    
    
    { return sinh(x); }
    
    ; 
}



#endif
; 
void sinh(float * v_initial_param_167_69, float * & v_user_func_169_70, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_169_70 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_68 = 0;(v_i_68 <= (-1 + v_N_0)); (++v_i_68)){
        v_user_func_169_70[v_i_68] = sinh_uf(v_initial_param_167_69[v_i_68]); 
    }
}


}

; 