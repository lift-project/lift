


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef TAN_UF_H
#define TAN_UF_H


; 
float tan_uf(float x){
    
    
    { return tan(x); }
    
    ; 
}



#endif
; 
void tan(float * v_initial_param_1127_209, float * & v_user_func_1129_210, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1129_210 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_208 = 0;(v_i_208 <= (-1 + v_N_190)); (++v_i_208)){
        v_user_func_1129_210[v_i_208] = tan_uf(v_initial_param_1127_209[v_i_208]); 
    }
}


}

; 