


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
void hypot(float * v_initial_param_109_38, float * v_initial_param_110_39, float * & v_user_func_116_41, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_116_41 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_37 = 0;(v_i_37 <= (-1 + v_N_0)); (++v_i_37)){
        v_user_func_116_41[v_i_37] = hypot_uf(v_initial_param_109_38[v_i_37], v_initial_param_110_39[v_i_37]); 
    }
}


}

; 