


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H


; 
float arccos_uf(float x){
    
    
    { return acos(x); }
    
    ; 
}



#endif
; 
void arccos(float * v_initial_param_1141_215, float * & v_user_func_1143_216, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1143_216 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_214 = 0;(v_i_214 <= (-1 + v_N_190)); (++v_i_214)){
        v_user_func_1143_216[v_i_214] = arccos_uf(v_initial_param_1141_215[v_i_214]); 
    }
}


}

; 