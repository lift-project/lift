


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
void tan(float * v_initial_param_88_33, float * & v_user_func_90_34, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_90_34 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_32 = 0;(v_i_32 <= (-1 + v_N_0)); (++v_i_32)){
        v_user_func_90_34[v_i_32] = tan_uf(v_initial_param_88_33[v_i_32]); 
    }
}


}

; 