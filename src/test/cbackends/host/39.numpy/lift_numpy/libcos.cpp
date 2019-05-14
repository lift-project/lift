


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
void cos(float * v_initial_param_1120_206, float * & v_user_func_1122_207, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1122_207 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_205 = 0;(v_i_205 <= (-1 + v_N_190)); (++v_i_205)){
        v_user_func_1122_207[v_i_205] = cos_uf(v_initial_param_1120_206[v_i_205]); 
    }
}


}

; 