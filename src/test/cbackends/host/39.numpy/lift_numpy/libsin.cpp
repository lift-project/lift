


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
void sin(float * v_initial_param_66_18, float * & v_user_func_68_19, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_68_19 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_17 = 0;(v_i_17 <= (-1 + v_N_0)); (++v_i_17)){
        v_user_func_68_19[v_i_17] = sin_uf(v_initial_param_66_18[v_i_17]); 
    }
}


}

; 