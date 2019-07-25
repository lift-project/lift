
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
void divide(float * v_initial_param_3680_658, float * v_initial_param_3681_659, float * & v_user_func_3687_661, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3687_661 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_657 = 0;(v_i_657 <= (-1 + v_N_352)); (++v_i_657)){
        v_user_func_3687_661[v_i_657] = divide_uf(v_initial_param_3680_658[v_i_657], v_initial_param_3681_659[v_i_657]); 
    }
}
}; 