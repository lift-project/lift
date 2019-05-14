


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
void cos(float * v_initial_param_70_19, float * & v_user_func_72_20, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_72_20 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_18 = 0;(v_i_18 <= (-1 + v_N_0)); (++v_i_18)){
        v_user_func_72_20[v_i_18] = cos_uf(v_initial_param_70_19[v_i_18]); 
    }
}


}

; 