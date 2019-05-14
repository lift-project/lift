


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef HYPOT_UF_H
#define HYPOT_UF_H


; 
float hypot_uf(float x, float y){
    
    
    { return sqrt((x*x)+(y*y)); }
    
    ; 
}



#endif
; 
void hypot(float * v_initial_param_105_34, float * v_initial_param_106_35, float * & v_user_func_112_37, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_112_37 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_33 = 0;(v_i_33 <= (-1 + v_N_0)); (++v_i_33)){
        v_user_func_112_37[v_i_33] = hypot_uf(v_initial_param_105_34[v_i_33], v_initial_param_106_35[v_i_33]); 
    }
}


}

; 