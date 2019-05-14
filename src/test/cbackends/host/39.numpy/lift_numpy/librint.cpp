


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef RINT_UF_H
#define RINT_UF_H


; 
float rint_uf(float x){
    
    
    return round(x) ;
    
    ; 
}



#endif
; 
void rint(float * v_initial_param_216_93, float * & v_user_func_218_94, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_218_94 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_92 = 0;(v_i_92 <= (-1 + v_N_0)); (++v_i_92)){
        v_user_func_218_94[v_i_92] = rint_uf(v_initial_param_216_93[v_i_92]); 
    }
}


}

; 