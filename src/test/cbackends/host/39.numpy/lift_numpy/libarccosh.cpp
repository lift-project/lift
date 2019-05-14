


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
void arccosh(float * v_initial_param_105_84, float * & v_user_func_107_85, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_107_85 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_83 = 0;(v_i_83 <= (-1 + v_N_0)); (++v_i_83)){
        v_user_func_107_85[v_i_83] = arccos_uf(v_initial_param_105_84[v_i_83]); 
    }
}


}

; 