


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef FIX_UF_H
#define FIX_UF_H


; 
float fix_uf(float x){
    
    
    return x>0?floor(x):ceil(x) ;
    
    ; 
}



#endif
; 
void fix(float * v_initial_param_223_96, float * & v_user_func_225_97, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_225_97 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_95 = 0;(v_i_95 <= (-1 + v_N_0)); (++v_i_95)){
        v_user_func_225_97[v_i_95] = fix_uf(v_initial_param_223_96[v_i_95]); 
    }
}


}

; 