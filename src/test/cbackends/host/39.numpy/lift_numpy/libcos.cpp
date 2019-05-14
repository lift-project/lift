


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef COS_UF_H
#define COS_UF_H


; 
float cos_uf(float x){
    
    
    { return cos(x); }
    
    ; 
}



#endif
; 
void cos(float * v_initial_param_79_28, float * & v_user_func_81_29, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_81_29 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_27 = 0;(v_i_27 <= (-1 + v_N_0)); (++v_i_27)){
        v_user_func_81_29[v_i_27] = cos_uf(v_initial_param_79_28[v_i_27]); 
    }
}


}

; 