
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTIPLY_UF_H
#define MULTIPLY_UF_H
; 
float multiply_uf(float x, float y){
    return x * y;; 
}

#endif
 ; 
void multiply(float * v_initial_param_3666_653, float * v_initial_param_3667_654, float * & v_user_func_3673_656, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3673_656 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_652 = 0;(v_i_652 <= (-1 + v_N_352)); (++v_i_652)){
        v_user_func_3673_656[v_i_652] = multiply_uf(v_initial_param_3666_653[v_i_652], v_initial_param_3667_654[v_i_652]); 
    }
}
}; 