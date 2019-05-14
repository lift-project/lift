


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef D2R_UF_H
#define D2R_UF_H


; 
float d2r_uf(float x){
    
    
    { return x*M_PI/180; }
    
    ; 
}



#endif
; 
void radians(float * v_initial_param_157_57, float * & v_user_func_159_58, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_159_58 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_56 = 0;(v_i_56 <= (-1 + v_N_0)); (++v_i_56)){
        v_user_func_159_58[v_i_56] = d2r_uf(v_initial_param_157_57[v_i_56]); 
    }
}


}

; 