


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef D2R_UF_H
#define D2R_UF_H


; 
float d2r_uf(float x){
    
    
    { return x*M_PI/180; }
    
    ; 
}



#endif
; 
void deg2rad(float * v_initial_param_160_63, float * & v_user_func_162_64, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_162_64 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_62 = 0;(v_i_62 <= (-1 + v_N_0)); (++v_i_62)){
        v_user_func_162_64[v_i_62] = d2r_uf(v_initial_param_160_63[v_i_62]); 
    }
}


}

; 