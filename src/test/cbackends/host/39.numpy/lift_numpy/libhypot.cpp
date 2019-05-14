


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
void hypot(float * v_initial_param_116_45, float * v_initial_param_117_46, float * & v_user_func_123_48, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_123_48 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_44 = 0;(v_i_44 <= (-1 + v_N_0)); (++v_i_44)){
        v_user_func_123_48[v_i_44] = hypot_uf(v_initial_param_116_45[v_i_44], v_initial_param_117_46[v_i_44]); 
    }
}


}

; 