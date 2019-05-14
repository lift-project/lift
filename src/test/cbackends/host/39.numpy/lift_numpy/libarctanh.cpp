


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H


; 
float arctanh_uf(float x){
    
    
    { return atanh(x); }
    
    ; 
}



#endif
; 
void arctanh(float * v_initial_param_202_84, float * & v_user_func_204_85, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_204_85 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_83 = 0;(v_i_83 <= (-1 + v_N_0)); (++v_i_83)){
        v_user_func_204_85[v_i_83] = arctanh_uf(v_initial_param_202_84[v_i_83]); 
    }
}


}

; 