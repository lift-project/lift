


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H


; 
float arctan_uf(float x){
    
    
    { return atan(x); }
    
    ; 
}



#endif
; 
void arctan(float * v_initial_param_100_33, float * & v_user_func_102_34, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_102_34 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_32 = 0;(v_i_32 <= (-1 + v_N_0)); (++v_i_32)){
        v_user_func_102_34[v_i_32] = arctan_uf(v_initial_param_100_33[v_i_32]); 
    }
}


}

; 