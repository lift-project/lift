


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ROUND_UF_H
#define ROUND_UF_H


; 
float round_uf(float x){
    
    
    { return ceil(x); }
    
    ; 
}



#endif
; 
void around(float * v_initial_param_207_85, float * & v_user_func_209_86, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_209_86 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_84 = 0;(v_i_84 <= (-1 + v_N_0)); (++v_i_84)){
        v_user_func_209_86[v_i_84] = round_uf(v_initial_param_207_85[v_i_84]); 
    }
}


}

; 