
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIVIDE_UF_H
#define DIVIDE_UF_H
; 
float divide_uf(float x, float y){
    return x / y;; 
}

#endif
 ; 
void divide(float * v_initial_param_7624_3069, float * v_initial_param_7625_3070, float * & v_user_func_7631_3072, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7631_3072 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3068 = 0;(v_i_3068 <= (-1 + v_N_2763)); (++v_i_3068)){
        v_user_func_7631_3072[v_i_3068] = divide_uf(v_initial_param_7624_3069[v_i_3068], v_initial_param_7625_3070[v_i_3068]); 
    }
}
}; 