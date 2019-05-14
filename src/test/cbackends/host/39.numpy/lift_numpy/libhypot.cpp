


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
void hypot(float * v_initial_param_113_42, float * v_initial_param_114_43, float * & v_user_func_120_45, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_120_45 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_41 = 0;(v_i_41 <= (-1 + v_N_0)); (++v_i_41)){
        v_user_func_120_45[v_i_41] = hypot_uf(v_initial_param_113_42[v_i_41], v_initial_param_114_43[v_i_41]); 
    }
}


}

; 