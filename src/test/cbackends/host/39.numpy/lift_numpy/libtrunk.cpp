


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef TRUNC_UF_H
#define TRUNC_UF_H


; 
float trunc_uf(float x){
    
    
    return trunc(x);
    
    ; 
}



#endif
; 
void trunk(float * v_initial_param_247_108, float * & v_user_func_249_109, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_249_109 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_107 = 0;(v_i_107 <= (-1 + v_N_0)); (++v_i_107)){
        v_user_func_249_109[v_i_107] = trunc_uf(v_initial_param_247_108[v_i_107]); 
    }
}


}

; 