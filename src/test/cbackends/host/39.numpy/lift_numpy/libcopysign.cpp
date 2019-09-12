
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COPYSIGN_UF_H
#define COPYSIGN_UF_H
; 
float copysign_uf(float x, float y){
    return y<0? x*(-1):x ;; 
}

#endif
 ; 
void copysign(float * v_initial_param_7540_3032, float * v_initial_param_7541_3033, float * & v_user_func_7547_3035, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7547_3035 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3031 = 0;(v_i_3031 <= (-1 + v_N_2763)); (++v_i_3031)){
        v_user_func_7547_3035[v_i_3031] = copysign_uf(v_initial_param_7540_3032[v_i_3031], v_initial_param_7541_3033[v_i_3031]); 
    }
}
}; 