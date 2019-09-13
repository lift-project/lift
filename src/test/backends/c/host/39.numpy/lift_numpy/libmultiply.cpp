
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
void multiply(float * v_initial_param_7610_3064, float * v_initial_param_7611_3065, float * & v_user_func_7617_3067, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7617_3067 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3063 = 0;(v_i_3063 <= (-1 + v_N_2763)); (++v_i_3063)){
        v_user_func_7617_3067[v_i_3063] = multiply_uf(v_initial_param_7610_3064[v_i_3063], v_initial_param_7611_3065[v_i_3063]); 
    }
}
}; 