


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
void arctanh(float * v_initial_param_201_83, float * & v_user_func_203_84, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_203_84 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_82 = 0;(v_i_82 <= (-1 + v_N_0)); (++v_i_82)){
        v_user_func_203_84[v_i_82] = arctanh_uf(v_initial_param_201_83[v_i_82]); 
    }
}


}

; 