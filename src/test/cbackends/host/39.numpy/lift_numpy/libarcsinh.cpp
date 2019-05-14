


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H


; 
float arcsinh_uf(float x){
    
    
    { return asinh(x); }
    
    ; 
}



#endif
; 
void arcsinh(float * v_initial_param_185_75, float * & v_user_func_187_76, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_187_76 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_74 = 0;(v_i_74 <= (-1 + v_N_0)); (++v_i_74)){
        v_user_func_187_76[v_i_74] = arcsinh_uf(v_initial_param_185_75[v_i_74]); 
    }
}


}

; 