


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
void cosh(float * v_initial_param_174_72, float * & v_user_func_176_73, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_176_73 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_71 = 0;(v_i_71 <= (-1 + v_N_0)); (++v_i_71)){
        v_user_func_176_73[v_i_71] = cosh_uf(v_initial_param_174_72[v_i_71]); 
    }
}


}

; 