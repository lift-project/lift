


#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef DIV_UF_H
#define DIV_UF_H


; 
float div_uf(float x, float y){
    
    
    { return (x)/(y); }
    
    ; 
}



#endif
; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H


; 
float arctan_uf(float x){
    
    
    { return atan(x); }
    
    ; 
}



#endif
; 
void arctan2(float * v_initial_param_121_42, float * v_initial_param_122_43, float * & v_user_func_124_46, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_132_45 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_124_46 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_41 = 0;(v_i_41 <= (-1 + v_N_0)); (++v_i_41)){
        v_user_func_132_45[v_i_41] = div_uf(v_initial_param_121_42[v_i_41], v_initial_param_122_43[v_i_41]); 
    }
    // For each element processed sequentially
    for (int v_i_40 = 0;(v_i_40 <= (-1 + v_N_0)); (++v_i_40)){
        v_user_func_124_46[v_i_40] = arctan_uf(v_user_func_132_45[v_i_40]); 
    }
}


}

; 