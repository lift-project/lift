


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ROUND_UF_H
#define ROUND_UF_H


; 
float round_uf(float x){
    
    
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;
    
    ; 
}



#endif
; 
void around(float * v_initial_param_212_90, float * & v_user_func_214_91, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_214_91 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_89 = 0;(v_i_89 <= (-1 + v_N_0)); (++v_i_89)){
        v_user_func_214_91[v_i_89] = round_uf(v_initial_param_212_90[v_i_89]); 
    }
}


}

; 