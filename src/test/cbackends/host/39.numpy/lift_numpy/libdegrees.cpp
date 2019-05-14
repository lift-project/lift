


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef R2D_UF_H
#define R2D_UF_H


; 
float r2d_uf(float x){
    
    
    { return x*180/M_PI; }
    
    ; 
}



#endif
; 
void degrees(float * v_initial_param_150_54, float * & v_user_func_152_55, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_152_55 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_53 = 0;(v_i_53 <= (-1 + v_N_0)); (++v_i_53)){
        v_user_func_152_55[v_i_53] = r2d_uf(v_initial_param_150_54[v_i_53]); 
    }
}


}

; 