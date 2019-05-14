


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
void arctan(float * v_initial_param_98_31, float * & v_user_func_100_32, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_100_32 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_30 = 0;(v_i_30 <= (-1 + v_N_0)); (++v_i_30)){
        v_user_func_100_32[v_i_30] = arctan_uf(v_initial_param_98_31[v_i_30]); 
    }
}


}

; 