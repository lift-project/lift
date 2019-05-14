


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
void arctan(float * v_initial_param_108_41, float * & v_user_func_110_42, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_110_42 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_40 = 0;(v_i_40 <= (-1 + v_N_0)); (++v_i_40)){
        v_user_func_110_42[v_i_40] = arctan_uf(v_initial_param_108_41[v_i_40]); 
    }
}


}

; 