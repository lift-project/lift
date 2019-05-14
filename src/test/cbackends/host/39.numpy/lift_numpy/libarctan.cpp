


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
void arctan(float * v_initial_param_1148_218, float * & v_user_func_1150_219, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1150_219 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_217 = 0;(v_i_217 <= (-1 + v_N_190)); (++v_i_217)){
        v_user_func_1150_219[v_i_217] = arctan_uf(v_initial_param_1148_218[v_i_217]); 
    }
}


}

; 