


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H


; 
float arcsin_uf(float x){
    
    
    { return asin(x); }
    
    ; 
}



#endif
; 
void arcsin(float * v_initial_param_1134_212, float * & v_user_func_1136_213, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1136_213 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_211 = 0;(v_i_211 <= (-1 + v_N_190)); (++v_i_211)){
        v_user_func_1136_213[v_i_211] = arcsin_uf(v_initial_param_1134_212[v_i_211]); 
    }
}


}

; 