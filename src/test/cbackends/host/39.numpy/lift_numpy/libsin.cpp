


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
void sin(float * v_initial_param_73_25, float * & v_user_func_75_26, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_75_26 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_24 = 0;(v_i_24 <= (-1 + v_N_0)); (++v_i_24)){
        v_user_func_75_26[v_i_24] = sin_uf(v_initial_param_73_25[v_i_24]); 
    }
}


}

; 